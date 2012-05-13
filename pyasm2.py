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
import struct

class Immediate:
    """Defines Immediates, has the ability to treat immediates as addresses."""
    def __init__(self, value=0, addr=False):
        self.value = value
        self.addr = addr

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
es = ES = SegmentRegister(0, 'es')
cs = CS = SegmentRegister(1, 'cs')
ss = SS = SegmentRegister(2, 'ss')
ds = DS = SegmentRegister(3, 'ds')
fs = FS = SegmentRegister(4, 'fs')
gs = GS = SegmentRegister(5, 'gs')

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
        f = lambda x, y: y.register[x-1] if x else None
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
        return s if self.segment is None else \
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
        return struct.pack(fmt[self.size], int(value))

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
    def __init__(self, index, name):
        self.index = index
        self.name = name

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
eax = EAX = GeneralPurposeRegister(0, 'eax')
ecx = ECX = GeneralPurposeRegister(1, 'ecx')
edx = EDX = GeneralPurposeRegister(2, 'edx')
ebx = EBX = GeneralPurposeRegister(3, 'ebx')
esp = ESP = GeneralPurposeRegister(4, 'esp')
ebp = EBP = GeneralPurposeRegister(5, 'ebp')
esi = ESI = GeneralPurposeRegister(6, 'esi')
edi = EDI = GeneralPurposeRegister(7, 'edi')

# array of general purpose registers, according to their index
GeneralPurposeRegister.register = (eax, ecx, edx, ebx, esp, ebp, esi, edi)

# make an alias `gpr' to GeneralPurposeRegister in order to simplify the
# creation of Instruction's
gpr = GeneralPurposeRegister

class XmmRegister:
    """Defines the Xmm Registers, registers used for the SSE instructions."""
    def __init__(self, index, name):
        self.index = index
        self.name = name

    def __str__(self):
        return self.name

    def __repr__(self):
        return self.name

xmm0 = XMM0 = XmmRegister(0, 'xmm0')
xmm1 = XMM1 = XmmRegister(1, 'xmm1')
xmm2 = XMM2 = XmmRegister(2, 'xmm2')
xmm3 = XMM3 = XmmRegister(3, 'xmm3')
xmm4 = XMM4 = XmmRegister(4, 'xmm4')
xmm5 = XMM5 = XmmRegister(5, 'xmm5')
xmm6 = XMM6 = XmmRegister(6, 'xmm6')
xmm7 = XMM7 = XmmRegister(7, 'xmm7')

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
        MemoryAddress, Immediate, XmmRegister)

    # we use a ctypes-like way to implement instructions.
    _opcode_ = None
    _enc_ = []

    def __init__(self, operand1=None, operand2=None, operand3=None,
            disable_valid_instr_check=False):
        """Initialize a new Instruction object."""
        assert operand1 is None or isinstance(operand1, self.VALID_OPERANDS)
        assert operand2 is None or isinstance(operand2, self.VALID_OPERANDS)
        assert operand3 is None or isinstance(operand3, self.VALID_OPERANDS)

        # convert int and long's to Immediate values.
        f = lambda x: x if not isinstance(x, (int, long)) else Immediate(x)

        self.op1 = f(operand1)
        self.op2 = f(operand2)
        self.op3 = f(operand3)

        # check if this combination of operands is valid
        if not disable_valid_instr_check:
            self.encoding()

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
            elif not issubclass(op1[1], self.op1.__class__):
                continue

            if op2 is None:
                return (opcode, op1, op2, op3)

            # if the encoding is not a tuple, then it's a hardcoded value.
            if not isinstance(op2, tuple):
                # check if the classes and objects match.
                if op2.__class__ != self.op2.__class__ or op2 != self.op2:
                    continue
            # check the operand (and size) of this match
            elif not issubclass(op2[1], self.op2.__class__):
                continue

            if op3 is None:
                return (opcode, op1, op2, op3)

            # check if the third operand matches (can only be an Immediate)
            if op3[1] != self.op3.__class__:
                continue

            # we found a matching encoding, return it.
            return (opcode, op1, op2, op3)

        raise Exception('Unknown or Invalid Encoding')

    def __str__(self):
        """Representation of this Instruction."""
        s = self.__class__.__name__
        ops = filter(lambda x: x is not None, (self.op1, self.op2, self.op3))
        if len(ops):
            return s + ' ' + ', '.join(map(str, ops))
        return s

    def encode(self):
        """Encode this Instruction into its machine code representation."""
        enc = self.encoding()
        if enc is None:
            op = self._opcode_
            return chr(op) if isinstance(op, int) else op

        opcode, op1, op2, op3 = enc
        ops = (self.op1, self.op2, self.op3)
        modrm_reg = modrm_rm = None

        ret = chr(opcode) if isinstance(opcode, int) else opcode
        disp = ''

        for i in xrange(3):
            op = enc[i+1]
            # we don't have to process empty operands or hardcoded values
            if op is None or not isinstance(op, tuple):
                continue

            size, typ = op

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

        return ret + disp

class retn(Instruction):
    _opcode_ = 0xc3
    _enc_ = [
        (0xc2, (word, imm))
    ]

class nop(Instruction):
    _opcode_ = 0x90

class mov(Instruction):
    # mov r32, imm32
    _enc_ = zip(range(0xb8, 0xbf), gpr.register, ((dword, imm),) * 8) + [
        (0x8b, (dword, gpr), (dword, mem)),
        (0x89, (dword, mem), (dword, gpr)),
        (0x88, (byte, mem), (byte, gpr)),
        (0x8a, (byte, gpr), (byte, mem)),
    ]

class push(Instruction):
    # push r32
    _enc_ = zip(range(0x50, 0x58), gpr.register)

class pop(Instruction):
    # pop r32
    _enc_ = zip(range(0x58, 0x60), gpr.register)

class inc(Instruction):
    # inc r32
    _enc_ = zip(range(0x40, 0x48), gpr.register)

class dec(Instruction):
    # dec r32
    _enc_ = zip(range(0x48, 0x50), gpr.register)

class xchg(Instruction):
    # xchg eax, r32
    _enc_ = zip(range(0x91, 0x98), gpr.register[1:], (eax,) * 8)

class pshufd(Instruction):
    _enc_ = [('\x66\x0f\x70', (oword, xmm), (oword, memxmm), (byte, imm))]

class paddd(Instruction):
    _enc_ = [('\x66\x0f\xfe', (oword, xmm), (oword, memxmm))]

class psubd(Instruction):
    _enc_ = [('\x66\x0f\xfa', (oword, xmm), (oword, memxmm))]

class pand(Instruction):
    _enc_ = [('\x66\x0f\xdb', (oword, xmm), (oword, memxmm))]
