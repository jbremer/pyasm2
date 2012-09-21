from construct import SBInt16, SBInt32, UBInt16

# http://en.wikipedia.org/wiki/Java_bytecode_instruction_listings
_table = {
    0x32: 'aaload',
    0x53: 'aastore',
    0x01: 'aconst_null',
    0x19: 'aload',
    0x2a: 'aload_0',
    0x2b: 'aload_1',
    0x2c: 'aload_2',
    0x2d: 'aload_3',
    0xbd: 'anewarray',
    0xb0: 'areturn',
    0xbe: 'arraylength',
    0x3a: 'astore',
    0x4b: 'astore_0',
    0x4c: 'astore_1',
    0x4d: 'astore_2',
    0x4e: 'astore_3',
    0xbf: 'athrow',
    0x33: 'baload',
    0x54: 'bastore',
    0x10: 'bipush',
    0x34: 'caload',
    0x55: 'castore',
    0xc0: 'checkcast',
    0x90: 'd2f',
    0x8e: 'd2i',
    0x8f: 'd2l',
    0x63: 'dadd',
    0x31: 'daload',
    0x52: 'dastore',
    0x98: 'dcmpg',
    0x97: 'dcmpl',
    0x0e: 'dconst_0',
    0x0f: 'dconst_1',
    0x6f: 'ddiv',
    0x18: 'dload',
    0x26: 'dload_0',
    0x27: 'dload_1',
    0x28: 'dload_2',
    0x29: 'dload_3',
    0x6b: 'dmul',
    0x77: 'dneg',
    0x73: 'drem',
    0xaf: 'dreturn',
    0x39: 'dstore',
    0x47: 'dstore_0',
    0x48: 'dstore_1',
    0x49: 'dstore_2',
    0x4a: 'dstore_3',
    0x67: 'dsub',
    0x59: 'dup',
    0x5a: 'dup_x1',
    0x5b: 'dup_x2',
    0x5c: 'dup2',
    0x5d: 'dup2_x1',
    0x5e: 'dup2_x2',
    0x8d: 'f2d',
    0x8b: 'f2i',
    0x8c: 'f2l',
    0x62: 'fadd',
    0x30: 'faload',
    0x51: 'fastore',
    0x96: 'fcmpg',
    0x95: 'fcmpl',
    0x0b: 'fconst_0',
    0x0c: 'fconst_1',
    0x0d: 'fconst_2',
    0x6e: 'fdiv',
    0x17: 'fload',
    0x22: 'fload_0',
    0x23: 'fload_1',
    0x24: 'fload_2',
    0x25: 'fload_3',
    0x6a: 'fmul',
    0x76: 'fneg',
    0x72: 'frem',
    0xae: 'freturn',
    0x38: 'fstore',
    0x43: 'fstore_0',
    0x44: 'fstore_1',
    0x45: 'fstore_2',
    0x46: 'fstore_3',
    0x66: 'fsub',
    0xb4: 'getfield',
    0xb2: 'getstatic',
    0xa7: 'goto',
    0xc8: 'goto_w',
    0x91: 'i2b',
    0x92: 'i2c',
    0x87: 'i2d',
    0x86: 'i2f',
    0x85: 'i2l',
    0x93: 'i2s',
    0x60: 'iadd',
    0x2e: 'iaload',
    0x7e: 'iand',
    0x4f: 'iastore',
    0x02: 'iconst_m1',
    0x03: 'iconst_0',
    0x04: 'iconst_1',
    0x05: 'iconst_2',
    0x06: 'iconst_3',
    0x07: 'iconst_4',
    0x08: 'iconst_5',
    0x6c: 'idiv',
    0xa5: 'if_acmpeq',
    0xa6: 'if_acmpne',
    0x9f: 'if_icmpeq',
    0xa0: 'if_icmpne',
    0xa1: 'if_icmplt',
    0xa2: 'if_icmpge',
    0xa3: 'if_icmpgt',
    0xa4: 'if_icmple',
    0x99: 'ifeq',
    0x9a: 'ifne',
    0x9b: 'iflt',
    0x9c: 'ifge',
    0x9d: 'ifgt',
    0x9e: 'ifle',
    0xc7: 'ifnonnull',
    0xc6: 'ifnull',
    0x84: 'iinc',
    0x15: 'iload',
    0x1a: 'iload_0',
    0x1b: 'iload_1',
    0x1c: 'iload_2',
    0x1d: 'iload_3',
    0x68: 'imul',
    0x74: 'ineg',
    0xc1: 'instanceof',
    0xba: 'invokedynamic',
    0xb9: 'invokeinterface',
    0xb7: 'invokespecial',
    0xb8: 'invokestatic',
    0xb6: 'invokevirtual',
    0x80: 'ior',
    0x70: 'irem',
    0xac: 'ireturn',
    0x78: 'ishl',
    0x7a: 'ishr',
    0x36: 'istore',
    0x3b: 'istore_0',
    0x3c: 'istore_1',
    0x3d: 'istore_2',
    0x3e: 'istore_3',
    0x64: 'isub',
    0x7c: 'iushr',
    0x82: 'ixor',
    0xa8: 'jsr',
    0xc9: 'jsr_w',
    0x8a: 'l2d',
    0x89: 'l2f',
    0x88: 'l2i',
    0x61: 'ladd',
    0x2f: 'laload',
    0x7f: 'land',
    0x50: 'lastore',
    0x94: 'lcmp',
    0x09: 'lconst_0',
    0x0a: 'lconst_1',
    0x12: 'ldc',
    0x13: 'ldc_w',
    0x14: 'ldc2_w',
    0x6d: 'ldiv',
    0x16: 'lload',
    0x1e: 'lload_0',
    0x1f: 'lload_1',
    0x20: 'lload_2',
    0x21: 'lload_3',
    0x69: 'lmul',
    0x75: 'lneg',
    0xab: 'lookupswitch',
    0x81: 'lor',
    0x71: 'lrem',
    0xad: 'lreturn',
    0x79: 'lshl',
    0x7b: 'lshr',
    0x37: 'lstore',
    0x3f: 'lstore_0',
    0x40: 'lstore_1',
    0x41: 'lstore_2',
    0x42: 'lstore_3',
    0x65: 'lsub',
    0x7d: 'lushr',
    0x83: 'lxor',
    0xc2: 'monitorenter',
    0xc3: 'monitorexit',
    0xc5: 'multianewarray',
    0xbb: 'new',
    0xbc: 'newarray',
    0x00: 'nop',
    0x57: 'pop',
    0x58: 'pop2',
    0xb5: 'putfield',
    0xb3: 'putstatic',
    0xa9: 'ret',
    0xb1: 'return',
    0x35: 'saload',
    0x56: 'sastore',
    0x11: 'sipush',
    0x5f: 'swap',
    0xaa: 'tableswitch',
    0xc4: 'wide',
    0xca: 'breakpoint',
    0xfe: 'impdep1',
    0xff: 'impdep2',
}

# name to opcode table
_names = dict((v if type(v) == str else v[0], k) for k, v in _table.items())

# opcodes which are valid for the "wide" instruction with length 3
_wide_opcodes = sorted(_names[x] for x in ('iload', 'fload', 'aload', 'lload',
    'dload', 'istore', 'fstore', 'astore', 'lstore', 'dstore', 'ret'))

# opcode which is valid for the "wide" instruction with length 5
_wide_inc = _names['iinc']

# opcodes which have a two-byte index into the constant pool (and no further
# arguments)
_index_opcodes = sorted(_names[x] for x in ('anewarray', 'checkcast',
    'getfield', 'getstatic', 'instanceof', 'invokespecial', 'invokestatic',
    'invokevirtual', 'ldc_w', 'ldc2_w', 'new', 'putfield', 'putstatic'))

# all opcodes that take two branch bytes; "goto", "jsr", and all opcodes
# starting with "if"
_branch_opcodes = sorted(_names[x] for x in _names if x[:2] == 'if' or
    x == 'goto' or x == 'jsr')

_primite_types = {
    10: 'int',
    8: 'byte',
    11: 'long',
    7: 'double',
    6: 'float',
    5: 'char',
    9: 'short',
}

_other_opcodes = {
    'bipush': lambda ch, d, o: Instruction(name=_table[ch], length=2,
        value=ord(d[o+1]), rep='%s %d' % (_table[ch], ord(d[o+1]))),
    'sipush': lambda ch, d, o: Instruction(name=_table[ch], length=3,
        value=SBInt16(None).parse(d[o+1:o+3]), rep='%s %d' % (_table[ch],
        SBInt16(None).parse(d[o+1:o+3]))),
    'lookupswitch': lambda ch, d, o: None,
    'tableswitch': lambda ch, d, o: None,
    'newarray': lambda ch, d, o: Instruction(name=_table[ch], length=2,
        value=ord(d[o+1]), rep='%s %s' % (_table[ch],
        _primite_types[ord(d[o+1])])),
    'goto_w': lambda ch, d, o: Instruction(name=_table[ch], length=5,
        value=SBInt32(None).parse(d[o+1:o+5]), rep='%s %d' % (_table[ch],
        SBInt32(None).parse(d[o+1:o+5]))),
    'invokedynamic': lambda ch, d, o: None,
    'invokeinterface': lambda ch, d, o: None,
    'jsr_w': lambda ch, d, o: Instruction(name=_table[ch], length=5,
        value=SBInt32(None).parse(d[o+1:o+5]), rep='%s %d' % (_table[ch],
        SBInt32(None).parse(d[o+1:o+5]))),
    'multianewarray': lambda ch, d, o: Instruction(name=_table[ch], length=4,
        cp=UBInt16(None).parse(d[o+1:o+3]), value=ord(d[o+3]),
        rep='%s #%d %d' % (_table[ch], UBInt16(None).parse(d[o+1:o+3]),
        ord(d[o+3]))),
    'ldc': lambda ch, d, o: Instruction(name=_table[ch], length=2,
        cp=ord(d[o+1]), rep='%s #%d' % (_table[ch], ord(d[o+1]))),
}

# convert the opcode names of _other_opcodes into opcode indices
_other_opcodes = dict((_names[k], v) for k, v in _other_opcodes.items())

class Instruction:
    def __init__(self, name=None, cp=None, local=None, length=None,
            value=None, rep=None):
        self.name = name
        self.cp = cp
        self.local = local
        self.length = length
        self.value = value
        self.rep = rep

    def __str__(self):
        return self.rep or self.name

    def __repr__(self):
        ret = ['name="%s"' % self.name]
        if self.cp:
            ret += ['cp=%s' % self.cp]
        if self.local:
            ret += ['local=%d' % self.local]
        if self.length:
            ret += ['length=%d' % self.length]
        if self.value:
            ret += ['value="%s"' % self.value]
        if self.rep:
            ret += ['rep="%s"' % self.rep]
        return 'Instruction(%s)' % ', '.join(ret)

def disassemble(data, offset=0):
    # opcode
    ch = ord(data[offset])

    # the "wide" instruction
    if ch == 0xc4:
        ch2 = ord(data[offset+1])

        if ch2 == _wide_inc:
            idx = UBInt16(None).parse(data[offset+2:offset+4])
            val = SBInt16(None).parse(data[offset+4:offset+6])
            return Instruction(name=_table[ch2], local=idx, length=6,
                value=val, rep='%s v%d, %d' % (_table[ch2], idx, val))
        elif ch2 in _wide_opcodes:
            idx = UBInt16(None).parse(data[offset+2:offset+4])
            return Instruction(name=_table[ch2], local=idx, length=4,
                rep='%s v%d' % (_table[ch2], idx))
        else:
            return None

    # if the opcode is in _wide_opcodes then it loads or stores a local
    if ch in _wide_opcodes:
        return Instruction(name=_table[ch], length=2,
            local=ord(data[offset+1]), rep='%s v%d' % (_table[ch],
            ord(data[offset+1])))

    # instructions which only have an index into the constant pool as argument
    if ch in _index_opcodes:
        return Instruction(name=_table[ch], length=3,
            cp=UBInt16(None).parse(data[offset+1:offset+3]),
            rep='%s #%d' % (_table[ch],
            UBInt16(None).parse(data[offset+1:offset+3])))

    # branch instructions that take a two-byte branch offset
    if ch in _branch_opcodes:
        return Instruction(name=_table[ch], length=3,
            value=SBInt16(None).parse(data[offset+1:offset+3]),
            rep='%s %d' % (_table[ch],
            SBInt16(None).parse(data[offset+1:offset+3])))

    # other opcodes which have to be handled independently
    if ch in _other_opcodes:
        return _other_opcodes[ch](ch, data, offset)

    # if the entry in the table is a string, then it's an instruction without
    # anything special, so we can simply return it
    if ch in _table:
        return Instruction(name=_table[ch], length=1)

    # unknown opcode
    return None
