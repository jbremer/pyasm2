
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

class SegmentRegister:
    """Defines the Segment Registers."""
    def __init__(self, index, name):
        """Create a new Segment Register with an index and name."""
        self.index = index
        self.name = name

    def __str__(self):
        """Representation of this register."""
        return self.name

    def __repr__(self):
        """Representation of this register."""
        return self.name

    def __index__(self):
        """Index of this register."""
        return self.index

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
        assert size is None or size in (8, 16, 32, 64, 128)
        assert segment is None or isinstance(segment, SegmentRegister)
        assert reg1 is None or isinstance(reg1, GeneralPurposeRegister)
        assert reg2 is None or isinstance(reg2, GeneralPurposeRegister)
        assert mult is None or mult in (1, 2, 4, 8)
        assert disp is None or disp >= 0 and disp < 2**32

        self.size = size
        self.segment = segment
        self.reg1 = reg1
        self.reg2 = reg2
        self.mult = mult
        self.disp = disp

        self.clean()

    def clean(self):
        """Makes sure that the internal representation of the Memory Address
            is as easy as possible.

        For example, we don't want `esp' in `reg2' (and `esp' can't have a
        `mult' other than one. Neither do we want to have a `reg2' with `mult'
        1 when `reg1' is None.

        Note that we can't use `esp' directly, because it's not initialized
        the first time(s) we call this function, therefore we use it's index,
        which is 4.

        """
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
        assert id(self) not in map(id, (byte, word, dword, qword))

        if isinstance(other, (int, long)):
            assert other >= 0 and other < 2**32 and self.disp is None

            self.disp = other
            return self.clean()

        if isinstance(other, GeneralPurposeRegister):
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
            (self.disp if self.disp is not None else 0) + \
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
        sizes = {8: 'byte', 16: 'word', 32: 'dword', 64: 'qword'}
        s = ''
        if self.reg1 is not None:
            s += str(self.reg1)
        if self.reg2 is not None:
            q = str(self.reg2) if self.mult == 1 else \
                str(self.reg2) + '*' + str(self.mult)
            s += q if not len(s) else '+' + q
        if self.disp is not None:
            q = '0x%x' % self.disp
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

# define the size for the memory addresses
byte = MemoryAddress(size=8)
word = MemoryAddress(size=16)
dword = MemoryAddress(size=32)
qword = MemoryAddress(size=64)

class GeneralPurposeRegister:
    """Defines the General Purpose Registers."""
    def __init__(self, index, name):
        """Create a new General Purpose Register with an index and name."""
        self.index = index
        self.name = name

    def __add__(self, other):
        """self + other"""
        if isinstance(other, (int, long)):
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
        """Representation of this register."""
        return self.name

    def __repr__(self):
        """Representation of this register."""
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

class Instruction:
    """Base class for every instruction."""
    VALID_OPERANDS = (int, long, SegmentRegister, GeneralPurposeRegister,
        MemoryAddress)

    def __init__(self, operand1=None, operand2=None, operand3=None):
        """Initialize a new Instruction object."""
        assert operand1 is None or isinstance(operand1, self.VALID_OPERANDS)
        assert operand2 is None or isinstance(operand2, self.VALID_OPERANDS)
        assert operand3 is None or isinstance(operand3, self.VALID_OPERANDS)

        self.operand1 = operand1
        self.operand2 = operand2
        self.operand3 = operand3

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

        if isinstance(op2, GeneralPurposeRegister):
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
                elif op2.disp >= 0 and op2.disp < 0x80:
                    mod = 1
                    buf = chr(op2.disp)
                elif op2.disp >= 0xffffff80 and op2.disp < 2**32:
                    mod = 1
                    buf = chr(op2.disp & 0xff)
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
