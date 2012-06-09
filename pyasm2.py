"""

pyasm2 - x86 assembler library          (C) 2012 Jurriaan Bremer

Although its called pyasm2, this is not per se a successor of Pyasm or pyASM.
pyasm2 aims to be as flexible as possible, it will support x86, SSE and SSE2.

A key feature of pyasm2 is the ability to have blocks of instructions and
being able to give the base address at a later time, that is, you don't need
to know the address of instructions before-hand. For example, you can construct
a series of instructions, request the size that will be needed in order to
store all instructions as sequence, allocate this memory and write the
instructions from there, this approach is very useful when making JIT
compilers etc.

The syntax of pyasm2 is supposed to be as simple as possible.

"""
import struct, types, copy

class Immediate:
    """Defines Immediates, has the ability to treat immediates as addresses."""
    def __init__(self, value=0, addr=False):
        self.value = value
        self.addr = addr

        if value < 2**8:
            self.size = byte.size
        elif value < 2**16:
            self.size = word.size
        else:
            self.size = dword.size

    def __int__(self):
        return self.value

    def __long__(self):
        return self.value

    def __cmp__(self, other):
        return self.value != int(other)

    def __str__(self):
        return '0x%x' % self.value

class SegmentRegister:
    """Defines the Segment Registers."""
    def __init__(self, index, name):
        self.index = index
        self.name = name

    def __str__(self):
        return self.name

    def __repr__(self):
        return self.name

    def __index__(self):
        return self.index

# make an alias `imm' to Immediate in order to simplify the creation of
# Instruction's
imm = Immediate

# define each segment register.
es = SegmentRegister(0, 'es')
cs = SegmentRegister(1, 'cs')
ss = SegmentRegister(2, 'ss')
ds = SegmentRegister(3, 'ds')
fs = SegmentRegister(4, 'fs')
gs = SegmentRegister(5, 'gs')

# array of segment registers, according to their index
SegmentRegister.register = (es, cs, ss, ds, fs, gs)

class MemoryAddress:
    def __init__(self, size=None, segment=None, reg1=None, reg2=None,
            mult=None, disp=None):
        """Create a new Memory Address."""
        # check if a register is valid..
        f = lambda x: x is None or isinstance(x, (GeneralPurposeRegister,
            XmmRegister))
        assert size is None or size in (8, 16, 32, 64, 128)
        assert segment is None or isinstance(segment, SegmentRegister)
        f(reg1)
        f(reg2)
        assert mult is None or mult in (1, 2, 4, 8)
        assert disp is None or int(disp) >= 0 and int(disp) < 2**32

        self.size = size
        self.segment = segment
        self.reg1 = reg1
        self.reg2 = reg2
        self.mult = mult
        self.disp = Immediate(disp) if isinstance(disp, (int, long)) else disp

        self.clean()

    def clean(self):
        """Makes sure that the internal representation of the Memory Address
            is as easy as possible.

        For example, we don't want `esp' in `reg2' (and `esp' can't have a
        `mult' other than one. Neither do we want to have a `reg2' with `mult'
        1 when `reg1' is None.

        Note that we can't use `esp' directly, because it's not initialized
        the first time(s) we call this function, therefore we use its index,
        which is 4.

        """
        # `esp' can't have a multiplier other than one.
        if self.reg2 is not None:
            assert self.reg2.index != 4 or self.mult == 1

        # swap registers if `reg2' contains `esp'
        if self.reg2 is not None and self.reg2.index == 4:
            self.reg1, self.reg2 = self.reg2, self.reg1

        # store `reg2' as `reg1' if `reg1' is None and `mult' is one.
        if self.reg1 is None and self.mult == 1:
            self.reg1, self.reg2, self.mult = self.reg2, None, None

        return self

    def final_clean(self):
        """Special clean function to clean and/or optimize right before
            assembling this Memory Address.

        When `reg1' is None, `mult' is two and `reg2' is not `esp', then we
        can optimize it by using `reg1', ie [eax*2] -> [eax+eax].

        """
        if self.reg1 is None and self.mult == 2 and self.reg2 != esp:
            self.reg1, self.mult = self.reg2, 1

    def merge(self, other):
        """Merge self with a Displacement, Register or Memory Address."""
        # it is not possible to merge with one of the predefined Memory
        # Addresses
        assert id(self) not in map(id, (byte, word, dword, qword, oword))

        if isinstance(other, (int, long, Immediate)):
            assert int(other) >= 0 and int(other) < 2**32 and self.disp is None

            self.disp = other

            return self.clean()

        if isinstance(other, (GeneralPurposeRegister, XmmRegister)):
            assert self.reg1 is None or self.reg2 is None

            if self.reg1 is None:
                self.reg1 = other
            else:
                self.reg2 = other

            return self.clean()

        if isinstance(other, MemoryAddress):
            assert self.size is None or other.size is None
            assert self.segment is None or other.segment is None
            assert self.disp is None or other.disp is None

            if self.size is None:
                self.size = other.size

            if self.segment is None:
                self.segment = other.segment

            reg1, reg2 = other.reg1, other.reg2

            if self.reg1 is None:
                if reg1 is not None:
                    self.reg1, reg1 = reg1, None
                elif reg2 is not None and other.mult == 1:
                    self.reg1, reg2 = reg2, None

            if self.reg2 is None:
                if reg1 is not None:
                    self.reg2, self.mult, reg1 = reg1, 1, None
                elif reg2 is not None:
                    self.reg2, self.mult, reg2 = reg2, other.mult, None

            assert reg1 is None and reg2 is None

            if self.disp is None:
                self.disp = other.disp

            return self.clean()

        raise Exception('Invalid Parameter')

    def __index__(self):
        """Encode a Memory Address as index.

        We have to be able to encode a Memory Address into an integer in
        order to use slices (which we do for instruction that use segment
        register.)

        Memory Layout is as following (displacement has to be the lower 32 bits
        in the event that something like `dword [cs:0x401000]' is used.)
        32 bits - displacement
        4  bits - reg1
        4  bits - reg2
        3  bits - mult

        If the displacement is None, it will be encoded as 0, and will be
        decoded as None later.
        General Purpose Registers are encoded as their `index' increased with
        one, or 0 if None.
        Multiplication is encoded using a table, which can be found below.

        """
        mults = {None: 0, 1: 1, 2: 2, 4: 3, 8: 4}
        # for encoding general purpose registers
        f = lambda x: x.index + 1 if x is not None else 0
        return \
            (int(self.disp) if self.disp is not None else 0) + \
            (f(self.reg1) << 32) + \
            (f(self.reg2) << 36) + \
            (mults[self.mult] << 40)

    def _decode_index(self, index):
        """Decodes a Memory Address encoded with __index__()."""
        mults = (None, 1, 2, 4, 8)
        # for decoding general purpose registers
        f = lambda x, y: y.register32[x-1] if x else None
        return MemoryAddress(disp=index % 2**32 if index % 2**32 else None,
            reg1=f((index >> 32) % 2**4, GeneralPurposeRegister),
            reg2=f((index >> 36) % 2**4, GeneralPurposeRegister),
            mult=mults[(index >> 40) % 2**3])

    def __getitem__(self, key):
        """Item or Slice to this MemoryAddress size.

        A slice, represented as [segment:address], defines a segment register
        and an address, the address is a combination of Displacements and
        General Purpose Registers (optionally with multiplication.)

        An item, represented as [address], only defines an address.

        """
        if isinstance(key, slice):
            ma = MemoryAddress(size=self.size,
                segment=SegmentRegister.register[key.start])
            return ma.merge(self._decode_index(key.stop))
        else:
            return MemoryAddress(size=self.size).merge(key)

    def __add__(self, other):
        """self + other"""
        return self.merge(other)

    def __radd__(self, other):
        """other + self"""
        return self.merge(other)

    def __str__(self):
        """Representation of this Memory Address."""
        sizes = {8: 'byte', 16: 'word', 32: 'dword', 64: 'qword', 128: 'oword'}
        s = ''
        if self.reg1 is not None:
            s += str(self.reg1)
        if self.reg2 is not None:
            q = str(self.reg2) if self.mult == 1 else \
                str(self.reg2) + '*' + str(self.mult)
            s += q if not len(s) else '+' + q
        if self.disp is not None:
            q = '0x%x' % int(self.disp)
            s += q if not len(s) else '+' + q
        if self.size is not None:
            if self.segment is not None:
                return '%s [%s:%s]' % (sizes[self.size], str(self.segment), s)
            else:
                return '%s [%s]' % (sizes[self.size], s)
        return '[%s]' % s if self.segment is None else \
            '[%s:%s]' % (str(self.segment), s)

    def __repr__(self):
        """Representation of this Memory Address."""
        return self.__str__()

    def __cmp__(self, other):
        """Check if two elements are the same, or not."""
        return 0 if self.size == other.size and \
            self.segment == other.segment and \
            self.reg1 == other.reg1 and self.reg2 == other.reg2 and \
            self.mult == other.mult and self.disp == other.disp else -1

    def pack(self, value):
        """Pack a value depending on the `size' of this Memory Address."""
        assert self.size is not None

        fmt = {8: 'B', 16: 'H', 32: 'I', 64: 'Q'}

        # convert the value, if it's negative.
        value = int(value) if int(value) >= 0 else int(value) + 2**self.size

        return struct.pack(fmt[self.size], value)

# define the size for the memory addresses
byte = MemoryAddress(size=8)
word = MemoryAddress(size=16)
dword = MemoryAddress(size=32)
qword = MemoryAddress(size=64)
oword = MemoryAddress(size=128)

# make an alias `mem' to MemoryAddress in order to simplify the creation of
# Instruction's
mem = MemoryAddress

class GeneralPurposeRegister:
    """Defines the General Purpose Registers."""
    def __init__(self, index, name, size):
        self.index = index
        self.name = name
        self.size = size.size

    def __add__(self, other):
        """self + other"""
        if isinstance(other, (int, long, Immediate)):
            return MemoryAddress(reg1=self, disp=other)
        if isinstance(other, GeneralPurposeRegister):
            return MemoryAddress(reg1=self, reg2=other, mult=1)
        if isinstance(other, MemoryAddress):
            return other.merge(self)
        raise Exception('Invalid Parameter')

    def __radd__(self, other):
        """other + self"""
        return self.__add__(other)

    def __sub__(self, other):
        """self - other"""
        return self.__add__(2**32 - other)

    def __mul__(self, other):
        """self * other"""
        return MemoryAddress(reg2=self, mult=other)

    def __rmul__(self, other):
        """other * self"""
        return MemoryAddress(reg2=self, mult=other)

    def __str__(self):
        return self.name

    def __repr__(self):
        return self.name

    def __index__(self):
        """Index of this register."""
        return MemoryAddress(reg1=self).__index__()

# define the general purpose registers
al = GeneralPurposeRegister(0, 'al', byte)
cl = GeneralPurposeRegister(1, 'cl', byte)
dl = GeneralPurposeRegister(2, 'dl', byte)
bl = GeneralPurposeRegister(3, 'bl', byte)
ah = GeneralPurposeRegister(4, 'ah', byte)
ch = GeneralPurposeRegister(5, 'ch', byte)
dh = GeneralPurposeRegister(6, 'dh', byte)
bh = GeneralPurposeRegister(7, 'bh', byte)

ax = GeneralPurposeRegister(0, 'ax', word)
cx = GeneralPurposeRegister(1, 'cx', word)
dx = GeneralPurposeRegister(2, 'dx', word)
bx = GeneralPurposeRegister(3, 'bx', word)
sp = GeneralPurposeRegister(4, 'sp', word)
bp = GeneralPurposeRegister(5, 'bp', word)
si = GeneralPurposeRegister(6, 'si', word)
di = GeneralPurposeRegister(7, 'di', word)

eax = GeneralPurposeRegister(0, 'eax', dword)
ecx = GeneralPurposeRegister(1, 'ecx', dword)
edx = GeneralPurposeRegister(2, 'edx', dword)
ebx = GeneralPurposeRegister(3, 'ebx', dword)
esp = GeneralPurposeRegister(4, 'esp', dword)
ebp = GeneralPurposeRegister(5, 'ebp', dword)
esi = GeneralPurposeRegister(6, 'esi', dword)
edi = GeneralPurposeRegister(7, 'edi', dword)

# array of general purpose registers, according to their index
GeneralPurposeRegister.register8 = (al, cl, dl, bl, ah, ch, dh, bh)
GeneralPurposeRegister.register16 = (ax, cx, dx, bx, sp, bp, si, di)
GeneralPurposeRegister.register32 = (eax, ecx, edx, ebx, esp, ebp, esi, edi)

# make an alias `gpr' to GeneralPurposeRegister in order to simplify the
# creation of Instruction's
gpr = GeneralPurposeRegister

class XmmRegister:
    """Defines the Xmm Registers, registers used for the SSE instructions."""
    def __init__(self, index, name):
        self.index = index
        self.name = name
        self.size = oword.size

    def __str__(self):
        return self.name

    def __repr__(self):
        return self.name

xmm0 = XmmRegister(0, 'xmm0')
xmm1 = XmmRegister(1, 'xmm1')
xmm2 = XmmRegister(2, 'xmm2')
xmm3 = XmmRegister(3, 'xmm3')
xmm4 = XmmRegister(4, 'xmm4')
xmm5 = XmmRegister(5, 'xmm5')
xmm6 = XmmRegister(6, 'xmm6')
xmm7 = XmmRegister(7, 'xmm7')

# make an alias `xmm' to XmmRegister in order to simplify the creation of
# Instruction's
xmm = XmmRegister

class MemoryGeneralPurposeRegister(MemoryAddress, GeneralPurposeRegister):
    """A combination of MemoryAddress and GeneralPurposeRegister,
        useful for modrm encoding etc."""
    pass

# a combination of operand types that can be used in modrm bytes.
memgpr = MemoryGeneralPurposeRegister

class MemoryXmmRegister(MemoryAddress, XmmRegister):
    """Combination of MemoryAddress and XmmRegister."""
    pass

memxmm = MemoryXmmRegister

class Instruction:
    """Base class for every instruction.

    Instructions that don't take any operands place their opcode as integer or
    string in `_opcode_'.
    Instructions that have one or more (maximum of three) operands fill the
    `_enc_' table, one entry per encoding. The layout of this encoding is a
    list of tuples, with a layout like the following.

    (opcode, operand1, operand2, operand3)

    `opcode' is an integer or string representing the opcode of this encoding.
    `operand1', `operand2' and `operand3' are tuples defining the size and
    type of operand, `operand2' and `operand3' are obviously optional. If an
    operand is not a tuple, it defines a hardcoded operand.

    """
    VALID_OPERANDS = (int, long, SegmentRegister, GeneralPurposeRegister,
        MemoryAddress, Immediate, XmmRegister, list)

    # we use a ctypes-like way to implement instructions.
    _opcode_ = None
    _enc_ = []
    _name_ = None

    def __init__(self, operand1=None, operand2=None, operand3=None,
            lock=False, rep=False, repne=False):
        """Initialize a new Instruction object."""
        assert operand1 is None or isinstance(operand1, self.VALID_OPERANDS)
        assert operand2 is None or isinstance(operand2, self.VALID_OPERANDS)
        assert operand3 is None or isinstance(operand3, self.VALID_OPERANDS)
        assert not isinstance(operand1, list) or len(operand1) == 1
        assert not isinstance(operand2, list) or len(operand2) == 1

        # convert int and long's to Immediate values.
        f = lambda x: x if not isinstance(x, (int, long)) else Immediate(x)
        # convert lists with one entry to Memory Addresses
        g = lambda x: x if not isinstance(x, list) else x[0]

        self.op1 = g(f(operand1))
        self.op2 = g(f(operand2))
        self.op3 = f(operand3)
        self.lock = lock
        self.rep = rep
        self.repne = repne

        # clean operands, if needed
        self.clean()

        # find the correct encoding for this combination of operands
        self.encoding()

    def clean(self):
        """Alters the order of operands if needed."""

        # the `xchg' instruction requires operands ordered as `memgpr, gpr'.
        if isinstance(self, xchg) and isinstance(self.op1, gpr) and \
                isinstance(self.op2, mem):
            self.op1, self.op2 = self.op2, self.op1

        if isinstance(self.op1, mem):
            self.op1.final_clean()

        if isinstance(self.op2, mem):
            self.op2.final_clean()

    def modrm(self, op1, op2):
        """Encode two operands into their modrm representation."""
        # we make sure `op2' is always the Memory Address (if present at all)
        if isinstance(op1, MemoryAddress):
            op1, op2 = op2, op1

        # a brief explanation of variabele names in this function.
        # there is a modrm byte, which contains `reg', `mod' and `rm' and
        # there is a sib byte, which contains `S', `index' and `base'.
        # for more explanation on the encoding, see also:
        # http://sandpile.org/x86/opc_rm.htm for the modrm byte, and
        # http://sandpile.org/x86/opc_rm.htm for the sib byte.

        reg = op1.index

        buf = ''
        sib = False

        if isinstance(op2, (GeneralPurposeRegister, XmmRegister)):
            mod = 3
            rm = op2.index

        elif isinstance(op2, MemoryAddress):
            mults = {1: 0, 2: 1, 4: 2, 8: 3}
            if op2.reg1 is None:
                if op2.reg2 is None:
                    # there should be atleast a displacement
                    assert op2.disp is not None
                    mod = 0
                    rm = 5
                    buf = struct.pack('I', op2.disp)
                else:
                    sib = True
                    S = mults[op2.mult]
                    index = op2.reg2.index
                    mod = 0
                    rm = 4
                    # it's not possible to have a register with a
                    # multiplication other than one without a 32bit
                    # displacement.
                    base = 5
                    buf = struct.pack('I', op2.disp if op2.disp else 0)
            else:
                if op2.reg2 is None:
                    # special case for `esp', since it requires the sib byte
                    if op2.reg1.index == 4:
                        sib = True
                        base = 4
                        index = 4
                        S = 0
                        rm = 4
                        mod = 2
                    # special case for `ebp', since it requires a displacement
                    elif op2.reg1.index == 5:
                        rm = 5
                        mod = 3
                    else:
                        rm = op2.reg1.index
                        mod = 2
                # special case for `esp', since it requires the sib byte
                elif op2.reg1.index == 4:
                    sib = True
                    base = 4
                    index = op2.reg2.index
                    S = mults[op2.mult]
                    rm = 4
                    mod = 2
                # special case for `ebp', since it requires a displacement
                elif op2.reg1.index == 5:
                    sib = True
                    index = op2.reg2.index
                    S = mults[op2.mult]
                    base = 5
                    rm = 4
                    mod = 3
                else:
                    sib = True
                    rm = 4
                    base = op2.reg1.index
                    index = op2.reg2.index
                    S = mults[op2.mult]
                    mod = 2

            # if `mod' is two here, then there can be either a 8bit, 32bit or
            # no displacement at all. when `mod' is three, there has to be
            # either a 8bit displacement or a 32bit one.
            if mod in (2, 3):
                if op2.disp is None:
                    if mod == 3:
                        mod = 1
                        buf = '\x00'
                    else:
                        mod = 0
                elif int(op2.disp) >= 0 and int(op2.disp) < 0x80:
                    mod = 1
                    buf = chr(int(op2.disp))
                elif int(op2.disp) >= 0xffffff80 and int(op2.disp) < 2**32:
                    mod = 1
                    buf = chr(int(op2.disp) & 0xff)
                else:
                    mod = 2
                    buf = struct.pack('I', op2.disp)

        # construct the modrm byte
        ret = chr((mod << 6) + (reg << 3) + rm)
        if sib:
            # if required, construct the sib byte
            ret += chr((S << 6) + (index << 3) + base)
        # append the buf, if it contains anything.
        return ret + buf

    def encoding(self):
        """Returns the Encoding used, as defined by `_enc_'.

        If the instruction doesn't take any operands, None is returned.
        If the instruction takes one or more (maximum of three) operands and a
        match is found in `_enc_', the match is returned, otherwise an
        Exception is raised.

        """
        if self.op1 is None:
            return None

        for enc in self._enc_:
            opcode, op1, op2, op3 = enc + (None,) * (4 - len(enc))

            # check if the amount of operands match.
            if len(filter(lambda x: x is not None, (self.op1, self.op2,
                    self.op3))) != len(filter(lambda x: x is not None, (op1,
                    op2, op3))):
                continue

            # if the encoding is not a tuple, then it's a hardcoded value.
            if not isinstance(op1, tuple):
                # check if the classes and objects match.
                if op1.__class__ != self.op1.__class__ or op1 != self.op1:
                    continue
            # check the operand (and size) of this match
            elif not issubclass(op1[1], self.op1.__class__) or \
                    hasattr(self.op1, 'size') and op1[0] is not None and \
                    (op1[0].size != self.op1.size if op1[1] != imm else
                    self.op1.size > op1[0].size):
                continue

            if op2 is None:
                self._encoding = (opcode, op1, op2, op3)
                return self._encoding

            # if the encoding is not a tuple, then it's a hardcoded value.
            if not isinstance(op2, tuple):
                # check if the classes and objects match.
                if op2.__class__ != self.op2.__class__ or op2 != self.op2:
                    continue
            # check the operand (and size) of this match
            elif not issubclass(op2[1], self.op2.__class__) or \
                    hasattr(self.op2, 'size') and op2[0] is not None and \
                    (op2[0].size != self.op2.size if op2[1] != imm else
                    self.op2.size > op2[0].size):
                continue

            if op3 is None:
                self._encoding = (opcode, op1, op2, op3)
                return self._encoding

            # check if the third operand matches (can only be an Immediate)
            if op3[1] != self.op3.__class__:
                continue

            # we found a matching encoding, return it.
            self._encoding = (opcode, op1, op2, op3)
            return self._encoding

        raise Exception('Unknown or Invalid Encoding')

    def __repr__(self):
        """Representation of this Instruction."""
        s = ''

        if self.lock:
            s += 'lock '

        if self.repne:
            s += 'repne '

        if self.rep:
            s += 'rep '

        s += self._name_ or self.__class__.__name__
        ops = filter(lambda x: x is not None, (self.op1, self.op2, self.op3))
        if len(ops):
            return s + ' ' + ', '.join(map(str, ops))
        return s

    def __len__(self):
        """Return the Length of the Machine Code."""
        return self.__str__().__len__()

    def __str__(self):
        """Encode this Instruction into its machine code representation."""
        enc = self.encoding()

        ret = ''

        if self.lock:
            ret += '\xf0'

        if self.repne:
            ret += '\xf2'

        if self.rep:
            ret += '\xf3'

        if enc is None:
            op = self._opcode_
            ret += chr(op) if isinstance(op, int) else op
            return ret

        opcode, op1, op2, op3 = enc
        ops = (self.op1, self.op2, self.op3)
        modrm_reg = modrm_rm = None

        ret += chr(opcode) if isinstance(opcode, int) else opcode
        disp = ''

        for i in xrange(3):
            op = enc[i+1]
            # we don't have to process empty operands or hardcoded values
            if op is None or not isinstance(op, tuple):
                continue

            size, typ = op[:2]

            # if a third index is given in the operand's tuple, then that
            # means that we have to emulate the `reg' for the modrm byte.
            # the value of `reg' is therefore given as third value.
            if len(op) == 3:
                modrm_reg = gpr.register32[op[2]]

            # handle Immediates
            if typ == imm:
                disp += size.pack(ops[i])
                continue

            # handle the reg part of the modrm byte
            if not typ in (mem, memgpr, memxmm) and modrm_reg is None:
                modrm_reg = ops[i]
                continue

            # handle the rm part of the modrm byte
            if typ in (mem, gpr, xmm, memgpr, memxmm):
                modrm_rm = ops[i]
                continue

            raise Exception('Unknown Type')

        if modrm_reg or modrm_rm:
            ret += self.modrm(modrm_reg, modrm_rm)

        self._encode = ret + disp
        return self._encode

class RelativeJump:
    _index_ = None
    _name_ = None

    def __init__(self, value):
        self.value = value

    def __len__(self):
        return 6 if self._index_ is not None else 5

    def __repr__(self):
        name = self._name_ or self.__class__.__name__
        return name + ' ' + repr(self.value)

    def assemble(self, short=True, labels={}, offset=0):
        """Assemble the Relative Jump.

        `short' indicates if the relative offset should be encoded as 8bit
        value, if possible.
        `offset' is the offset of this jump

        """
        to = self.value
        if isinstance(self.value, Label):
            to = labels[self.value.index + self.value.base]

        if self._index_ is None:
            return chr(self._opcode_) + dword.pack(to - offset - 5)
        else:
            return '\x0f' + chr(0x80 + self._index_) + dword.pack(
                to - offset - 6)

class Block:
    def __init__(self, *args):
        self._l = []

        # current index for new labels
        self.label_base = 0

        # add each argument to the list
        map(self.append, args)

    def __repr__(self):
        """Return a string representation of all instructions chained."""
        # convert an instruction into a string representation, labels need an
        # additional semicolon and have to be absolute offset, rather than
        # a relative one
        ret = ''
        index = 0
        for instr in self._l:
            if isinstance(instr, Label):
                instr.base = index
                index += 1
                ret += repr(instr) + ':\n'
            elif isinstance(instr, RelativeJump) and isinstance(instr.value,
                    Label):
                instr.value.base = index
                ret += repr(instr) + '\n'
            else:
                ret += repr(instr) + '\n'
        return ret

    def __str__(self):
        """Return the Machine Code representation."""
        return ''.join(map(str, self._l))

    def assemble(self, recursion=10):
        """Assemble the given Block.

        Assembly can *ONLY* be called on the top-level assembly block. Any
        unresolved labels will result in an exception.

        `recursion' indicates the maximal amount of times to recurse in order
        to optimize the size of conditional jumps (see docs.)

        """
        local_labels = {}
        global_labels = {}
        offset = 0

        # first we obtain the offset of each label
        for idx, instr in enumerate(self._l):
            # convert any class objects to instances
            if isinstance(instr, types.ClassType):
                instr = instr()
                self._l[idx] = instr

            if isinstance(instr, (str, Label)):
                # named local label
                if isinstance(instr, str):
                    local_labels[instr] = offset

                # anonymous local label
                elif not instr.index:
                    instr.index = len(local_labels)
                    local_labels[instr.index] = offset

                # global named label
                elif isinstance(instr.index, str):
                    global_labels[instr.index] = offset
                    local_labels[instr.index] = offset

            elif isinstance(instr, Instruction):
                offset += len(instr)

            elif isinstance(instr, RelativeJump):
                offset += 6

                # is this a label?
                if isinstance(instr.value, Label):

                    # is this an anonymous label?
                    if isinstance(instr.value, (int, long)):
                        # make an absolute index from the relative one
                        instr.value.index += len(local_labels)

        # do at most `recursion' iterations in order to try to optimize the
        # relative jumps
        # ...

        machine_code = ''
        offset = 0

        # now we assemble the machine code
        for instr in self._l:

            if isinstance(instr, Instruction):
                machine_code += str(instr)

            elif isinstance(instr, RelativeJump):
                machine_code += instr.assemble(short=False,
                    labels=local_labels, offset=offset)

            offset = len(machine_code)

        return machine_code

    def append(self, other):
        """Append instruction(s) in `other' to `self'."""
        # if a class object was given, we create an instance ourselves
        # this can be either an Instruction or a Label
        if isinstance(other, types.ClassType):
            other = other()

        if isinstance(other, Label):
            other.base = self.label_base
            self._l.append(other)
            self.label_base += 1

        elif isinstance(other, RelativeJump):
            self._l.append(other)

            if isinstance(other.value, Label):
                other.value.base = self.label_base

        elif isinstance(other, Instruction):
            self._l.append(other)

            if isinstance(other.op1, Label):
                other.op1.base = self.label_base
            if isinstance(other.op2, Label):
                other.op2.base = self.label_base
            # TODO add memory address support

        elif isinstance(other, Block):
            # we merge the `other' block with ours, by appending.
            # TODO deepcopy might get in a recursive loop somehow, if that
            # ever occurs, implement a __deepcopy__ which only makes a new
            # copy of Labels
            #map(self.append, map(copy.deepcopy, other._l))
            map(self.append, other._l)

        else:
            raise Exception('This object is not welcome here.')

        return self

    def __iadd__(self, other):
        """self += other"""
        self.append(other)
        return self

    def __add__(self, other):
        """self + other"""
        return Block(self, other)

    def __radd__(self, other):
        """other + self"""
        return Block(other, self)

    def __iter__(self):
        return self._l

block = Block

class Label:
    def __init__(self, index=0):
        self.index = index

        # any base to add to `index'
        self.base = 0

    def __repr__(self):
        return '__lbl_%s' % (self.index + self.base)

lbl = Label

class retn(Instruction):
    _opcode_ = 0xc3
    _enc_ = [(0xc2, (word, imm))]

ret = retn

class leave(Instruction):
    _opcode_ = 0xc9

class nop(Instruction):
    _opcode_ = 0x90

class mov(Instruction):
    # mov r32, imm32 and mov r8, imm32
    _enc_ = \
        zip(range(0xb0, 0xb8), gpr.register8, ((byte, imm),) * 8) + \
        zip(range(0xb8, 0xc0), gpr.register32, ((dword, imm),) * 8) + [
        (0x8b, (dword, gpr), (dword, memgpr)),
        (0x89, (dword, memgpr), (dword, gpr)),
        (0x88, (byte, memgpr), (byte, gpr)),
        (0x8a, (byte, gpr), (byte, memgpr)),
        (0xc6, (byte, memgpr, 0), (byte, imm)),
        (0xc7, (dword, memgpr, 0), (dword, imm)),
    ]

class movzx(Instruction):
    _enc_ = [
        ('\x0f\xb6', (dword, gpr), (byte, memgpr)),
        ('\x0f\xb7', (dword, gpr), (word, memgpr)),
    ]

class movsx(Instruction):
    _enc_ = [
        ('\x0f\xbe', (dword, gpr), (byte, memgpr)),
        ('\x0f\xbf', (dword, gpr), (word, memgpr)),
    ]

class push(Instruction):
    # push r32
    _enc_ = zip(range(0x50, 0x58), gpr.register32) + [
        (0x06, es),
        (0x0e, cs),
        (0x16, ss),
        (0x1e, ds),
        ('\x0f\xa0', fs),
        ('\x0f\xa8', gs),
        (0x6a, (byte, imm)),
        (0x68, (dword, imm)),
        (0xff, (dword, mem, 6)),
    ]

class pop(Instruction):
    # pop r32
    _enc_ = zip(range(0x58, 0x60), gpr.register32) + [
        (0x07, es),
        (0x17, ss),
        (0x1f, ds),
        ('\x0f\xa1', fs),
        ('\x0f\xa9', gs),
        (0x8f, (dword, mem, 0)),
    ]

class inc(Instruction):
    # inc r32
    _enc_ = zip(range(0x40, 0x48), gpr.register32) + [
        (0xfe, (byte, memgpr, 0)),
        (0xff, (dword, memgpr, 0))]

class dec(Instruction):
    # dec r32
    _enc_ = zip(range(0x48, 0x50), gpr.register32) + [
        (0xfe, (byte, memgpr, 1)),
        (0xff, (dword, memgpr, 1))]

class xchg(Instruction):
    # xchg eax, r32
    _enc_ = zip(range(0x91, 0x98), gpr.register32[1:], (eax,) * 8) + [
        (0x86, (byte, memgpr), (byte, gpr)),
        (0x87, (dword, memgpr), (dword, memgpr))]

class stosb(Instruction):
    _opcode_ = 0xaa

class stosd(Instruction):
    _opcode_ = 0xab

class lodsb(Instruction):
    _opcode_ = 0xac

class lodsd(Instruction):
    _opcode_ = 0xad

class scasb(Instruction):
    _opcode_ = 0xae

class scasd(Instruction):
    _opcode_ = 0xaf

class lea(Instruction):
    _enc_ = [(0x8d, (dword, gpr), (None, mem))]

class pshufd(Instruction):
    _enc_ = [('\x66\x0f\x70', (oword, xmm), (oword, memxmm), (byte, imm))]

class paddb(Instruction):
    _enc_ = [('\x66\x0f\xfc', (oword, xmm), (oword, memxmm))]

class paddw(Instruction):
    _enc_ = [('\x66\x0f\xfd', (oword, xmm), (oword, memxmm))]

class paddd(Instruction):
    _enc_ = [('\x66\x0f\xfe', (oword, xmm), (oword, memxmm))]

class psubb(Instruction):
    _enc_ = [('\x66\x0f\xf8', (oword, xmm), (oword, memxmm))]

class psubw(Instruction):
    _enc_ = [('\x66\x0f\xf9', (oword, xmm), (oword, memxmm))]

class psubd(Instruction):
    _enc_ = [('\x66\x0f\xfa', (oword, xmm), (oword, memxmm))]

class pand(Instruction):
    _enc_ = [('\x66\x0f\xdb', (oword, xmm), (oword, memxmm))]

class pandn(Instruction):
    _enc_ = [('\x66\x0f\xdf', (oword, xmm), (oword, memxmm))]

class por(Instruction):
    _enc_ = [('\x66\x0f\xeb', (oword, xmm), (oword, memxmm))]

class pxor(Instruction):
    _enc_ = [('\x66\x0f\xef', (oword, xmm), (oword, memxmm))]

class pmuludq(Instruction):
    _enc_ = [('\x66\x0f\xf4', (oword, xmm), (oword, memxmm))]

class movaps(Instruction):
    _enc_ = [
        ('\x0f\x28', (oword, xmm), (oword, memxmm)),
        ('\x0f\x29', (oword, memxmm), (oword, xmm)),
    ]

class movups(Instruction):
    _enc_ = [
        ('\x0f\x10', (oword, xmm), (oword, memxmm)),
        ('\x0f\x11', (oword, memxmm), (oword, xmm)),
    ]

class movapd(Instruction):
    _enc_ = [
        ('\x66\x0f\x28', (oword, xmm), (oword, memxmm)),
        ('\x66\x0f\x29', (oword, memxmm), (oword, xmm)),
    ]

class movd(Instruction):
    _enc_ = [
        ('\x66\x0f\x6e', (oword, xmm), (dword, memgpr)),
        ('\x66\x0f\x7e', (dword, memgpr), (oword, xmm)),
    ]

class movss(Instruction):
    _enc_ = [
        ('\xf3\x0f\x10', (oword, xmm), (oword, memxmm)),
        ('\xf3\x0f\x11', (oword, memxmm), (oword, xmm)),
    ]

class jo(RelativeJump):
    _index_ = 0

class jno(RelativeJump):
    _index_ = 1

class jb(RelativeJump):
    _index_ = 2

class jnb(RelativeJump):
    _index_ = 3

jae = jnb

class jz(RelativeJump):
    _index_ = 4

class jnz(RelativeJump):
    _index_ = 5

class jbe(RelativeJump):
    _index_ = 6

class jnbe(RelativeJump):
    _index_ = 7

ja = jnbe

class js(RelativeJump):
    _index_ = 8

class jns(RelativeJump):
    _index_ = 9

class jp(RelativeJump):
    _index_ = 10

class jnp(RelativeJump):
    _index_ = 11

class jl(RelativeJump):
    _index_ = 12

class jnl(RelativeJump):
    _index_ = 13

jge = jnl

class jle(RelativeJump):
    _index_ = 14

class jnle(RelativeJump):
    _index_ = 15

def _branch_instr(name, opcode, enc, arg):
    if not isinstance(arg, (int, long, str, Label)):
        i = Instruction(arg)
        i._enc_ = enc
        i._name_ = name
        return i
    r = RelativeJump(arg)
    r._opcode_ = opcode
    r._name_ = name
    return r

def jmp(arg):
    return _branch_instr('jmp', 0xe9, None, arg)

def call(arg):
    return _branch_instr('call', 0xe8, None, arg)

_group_1_opcodes = lambda x: [
    (0x00+8*x, (byte, memgpr), (byte, gpr)),
    (0x01+8*x, (dword, memgpr), (dword, gpr)),
    (0x02+8*x, (byte, gpr), (byte, memgpr)),
    (0x03+8*x, (dword, gpr), (dword, memgpr)),
    (0x04+8*x, al, (byte, imm)),
    (0x80, (byte, memgpr, x), (byte, imm)),
    (0x83, (dword, memgpr, x), (byte, imm)),
    (0x05+8*x, eax, (dword, imm)),
    (0x81, (dword, memgpr, x), (dword, imm))]

class add(Instruction):
    _enc_ = _group_1_opcodes(0)

class or_(Instruction):
    _enc_ = _group_1_opcodes(1)
    _name_ = 'or'

class adc(Instruction):
    _enc_ = _group_1_opcodes(2)

class sbb(Instruction):
    _enc_ = _group_1_opcodes(3)

class and_(Instruction):
    _enc_ = _group_1_opcodes(4)
    _name_ = 'and'

class sub(Instruction):
    _enc_ = _group_1_opcodes(5)

class xor(Instruction):
    _enc_ = _group_1_opcodes(6)

class cmp_(Instruction):
    _enc_ = _group_1_opcodes(7)
    _name_ = 'cmp'

class test(Instruction):
    _enc_ = [
        (0x84, (byte, memgpr), (byte, gpr)),
        (0x85, (dword, memgpr), (dword, memgpr)),
        (0xa8, al, (byte, imm)),
        (0xa9, eax, (dword, imm)),
        (0xf6, (byte, memgpr, 0), (byte, imm)),
        (0xf7, (dword, memgpr, 0), (dword, imm)),
    ]

_group_2_opcodes = lambda x: [
    (0xd0, (byte, memgpr, x), imm(1)),
    (0xd1, (dword, memgpr, x), imm(1)),
    (0xd2, (byte, memgpr, x), cl),
    (0xd3, (dword, memgpr, x), cl),
    (0xc0, (byte, memgpr, x), (byte, imm)),
    (0xc1, (dword, memgpr, x), (byte, imm))]

class rol(Instruction):
    _enc_ = _group_2_opcodes(0)

class ror(Instruction):
    _enc_ = _group_2_opcodes(1)

class rcl(Instruction):
    _enc_ = _group_2_opcodes(2)

class rcr(Instruction):
    _enc_ = _group_2_opcodes(3)

class shl(Instruction):
    _enc_ = _group_2_opcodes(4)

class shr(Instruction):
    _enc_ = _group_2_opcodes(5)

class sal(Instruction):
    _enc_ = _group_2_opcodes(6)

class sar(Instruction):
    _enc_ = _group_2_opcodes(7)

_group_3_opcodes = lambda x: [
    (0xf6, (byte, memgpr, x)),
    (0xf7, (dword, memgpr, x))]

class not_(Instruction):
    _enc_ = _group_3_opcodes(2)
    _name_ = 'not'

class neg(Instruction):
    _enc_ = _group_3_opcodes(3)

class mul(Instruction):
    _enc_ = _group_3_opcodes(4)

class imul(Instruction):
    _enc_ = _group_3_opcodes(5) + [
        ('\x0f\xaf', (dword, gpr), (dword, memgpr)),
        (0x6b, (dword, gpr), (dword, memgpr), (byte, imm)),
        (0x69, (dword, gpr), (dword, memgpr), (dword, imm))
    ]

class div(Instruction):
    _enc_ = _group_3_opcodes(6)

class idiv(Instruction):
    _enc_ = _group_3_opcodes(7)

class movsb(Instruction):
    _opcode_ = 0xa4

class movsd(Instruction):
    _opcode_ = 0xa5

class cmpsb(Instruction):
    _opcode_ = 0xa6

class cmpsd(Instruction):
    _opcode_ = 0xa7

class pushf(Instruction):
    _opcode_ = 0x9c

class popf(Instruction):
    _opcode_ = 0x9d

class cpuid(Instruction):
    _opcode_ = '\x0f\xa2'

class sysenter(Instruction):
    _opcode_ = '\x0f\x34'

class fninit(Instruction):
    _opcode_ = '\xdb\xe3'
