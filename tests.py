
"""

Unittests that verify the integrity of pyasm2.

"""

from pyasm2 import *
import unittest

class CheckSyntax(unittest.TestCase):
    def test_syntax(self):
        eq = self.assertEqual
        ra = self.assertRaises

        eq(str(dword[eax]), 'dword [eax]')
        eq(str(byte[eax+eax*4]), 'byte [eax+eax*4]')
        eq(str(word[0xdeadf00d+8*esi+esp]), 'word [esp+esi*8+0xdeadf00d]')
        eq(str(eax+esi), '[eax+esi]')
        eq(str(dword[0x00112233]), 'dword [0x112233]')
        ra(AssertionError, lambda: eax+eax+eax)
        ra(AssertionError, lambda: esp*8)
        eq(0xb00b+ebp*8+ebx, ebx+ebp*8+0xb00b)
        ra(AssertionError, lambda: eax+0x111223344)
        #eq(str(dword[cs:eax+ebx]), 'dword [cs:eax+ebx]')
        eq(dword[cs:0x13371337], dword[cs:0x13371337])
        #eq(str(dword[cs:0xdeadf00d]), 'dword [cs:0xdeadf00d]')
        eq(dword[eax-0x1000], dword[eax+0xfffff000])

    def test_modrm(self):
        eq = self.assertEqual
        m = Instruction().modrm

        eq(m(eax, dword[eax]), '\x00')
        eq(m(ecx, dword[ebx]), m(dword[ebx], ecx))
        eq(m(esi, dword[esp+ebp*8+0x11223344]), '\xb4\xec\x44\x33\x22\x11')
        eq(m(eax, dword[ebp]), '\x45\x00')
        eq(m(edi, dword[esp]), '\x3c\x24')
        eq(m(dword[esi+eax], ebx), '\x1c\x06')
        eq(m(esi, dword[edi]), '\x37')
        eq(m(ecx, dword[edx+ebp+0xdeadf00d]), '\x8c\x2a\x0d\xf0\xad\xde')
        eq(m(edi, dword[esi*8]), '\x3c\xf5\x00\x00\x00\x00')
        eq(m(edx, dword[ebp+eax*4]), '\x54\x85\x00')
        eq(m(eax, dword[eax+0x7f]), '\x40\x7f')
        eq(m(eax, dword[eax+0x80]), '\x80\x80\x00\x00\x00')
        eq(m(eax, dword[eax-0x80]), '\x40\x80')
        eq(m(eax, dword[eax-0x81]), '\x80\x7f\xff\xff\xff')
        eq(m(eax, dword[eax-2]), '\x40\xfe')
        eq(m(eax, dword[eax+0x40]), '\x40\x40')
        eq(m(eax, ebx), '\xc3')
        eq(m(esi, edi), '\xf7')

    def test_pack(self):
        eq = self.assertEqual

        eq(byte.pack(1), '\x01')
        eq(word.pack(1), '\x01\x00')
        eq(dword.pack(1), '\x01\x00\x00\x00')
        eq(qword.pack(1), '\x01\x00\x00\x00\x00\x00\x00\x00')

    def test_instructions(self):
        eq = lambda i, s, b: (self.assertEqual(repr(i), s,
            'Invalid string representation for: ' + repr(i)),
            self.assertEqual(str(i), b, 'Invalid encoding for: ' +
                repr(i) + ' -> ' + repr(str(i))))
        ra = self.assertRaises

        eq(retn(), 'retn', '\xc3')
        eq(nop(), 'nop', '\x90')
        eq(retn(0x80), 'retn 0x80', '\xc2\x80\x00')

        eq(mov(eax, 0xdeadf00d), 'mov eax, 0xdeadf00d', '\xb8\x0d\xf0\xad\xde')
        eq(mov(esi, 0x11223344), 'mov esi, 0x11223344', '\xbe\x44\x33\x22\x11')
        eq(mov(edi, dword [esp+ebx*4+0x0c]), 'mov edi, dword [esp+ebx*4+0xc]',
            '\x8b\x7c\x9c\x0c')
        eq(mov(dword[ebp+0x30], ecx), 'mov dword [ebp+0x30], ecx',
            '\x89\x4d\x30')

        eq(push(ebx), 'push ebx', '\x53')
        eq(xchg(ebp, eax), 'xchg ebp, eax', '\x95')
        eq(push(edi), 'push edi', '\x57')
        eq(pop(ebx), 'pop ebx', '\x5b')
        eq(inc(edx), 'inc edx', '\x42')
        eq(dec(esi), 'dec esi', '\x4e')

        eq(pshufd(xmm4, oword[edx], 0x11), 'pshufd xmm4, oword [edx], 0x11',
            '\x66\x0f\x70\x22\x11')
        eq(pshufd(xmm2, xmm0, 0x40), 'pshufd xmm2, xmm0, 0x40',
            '\x66\x0f\x70\xd0\x40')

        eq(paddd(xmm2, xmm5), 'paddd xmm2, xmm5', '\x66\x0f\xfe\xd5')

        ra(Exception, lambda: paddd(xmm0, eax))
        ra(Exception, lambda: mov(eax, xmm1))
        ra(Exception, lambda: mov(eax, byte[ebx]))

        eq(inc(ecx, lock=True), 'lock inc ecx', '\xf0\x41')
        eq(stosd(rep=True), 'rep stosd', '\xf3\xab')
        eq(scasb(repne=True), 'repne scasb', '\xf2\xae')

        eq(lea(eax, [esp+eax*2+0x42]), 'lea eax, [esp+eax*2+0x42]',
            '\x8d\x44\x44\x42')

        eq(mov(dword[ebx+0x44332211], 0x88776655),
            'mov dword [ebx+0x44332211], 0x88776655', '\xc7\x83' + ''.join(
            map(chr, range(0x11, 0x99, 0x11))))

        eq(movss(xmm6, xmm3), 'movss xmm6, xmm3', '\xf3\x0f\x10\xf3')
        eq(movd(xmm7, edi), 'movd xmm7, edi', '\x66\x0f\x6e\xff')
        eq(pand(xmm4, oword [ecx]), 'pand xmm4, oword [ecx]',
            '\x66\x0f\xdb\x21')
        eq(movapd(xmm6, oword [ebx]), 'movapd xmm6, oword [ebx]',
            '\x66\x0f\x28\x33')

        eq(add(byte[eax], 0x42), 'add byte [eax], 0x42', '\x80\x00\x42')
        eq(cmp_(dword[esp+ecx*8+0x0c], 0x42),
            'cmp dword [esp+ecx*8+0xc], 0x42', '\x83\x7c\xcc\x0c\x42')
        eq(cmp_(byte[ebx], 0x13), 'cmp byte [ebx], 0x13', '\x80\x3b\x13')
        eq(mov(byte[ecx], 0x37), 'mov byte [ecx], 0x37', '\xc6\x01\x37')
        eq(add(eax, 1), 'add eax, 0x1', '\x83\xc0\x01')
        eq(mov(bl, 1), 'mov bl, 0x1', '\xb3\x01')
        eq(add(eax, 0x1111), 'add eax, 0x1111', '\x05\x11\x11\x00\x00')
        eq(add(ebx, 0x2222), 'add ebx, 0x2222', '\x81\xc3\x22\x22\x00\x00')
        eq(push(es), 'push es', '\x06')
        eq(push(0x42), 'push 0x42', '\x6a\x42')
        eq(push(0x111), 'push 0x111', '\x68\x11\x01\x00\x00')
        eq(push(dword[2]), 'push dword [0x2]', '\xff\x35\x02\x00\x00\x00')
        eq(push(dword[esp+edx*2]), 'push dword [esp+edx*2]', '\xff\x34\x54')
        eq(pop(eax), 'pop eax', '\x58')
        eq(pop(dword[edx]), 'pop dword [edx]', '\x8f\x02')
        eq(pop(dword[6]), 'pop dword [0x6]', '\x8f\x05\x06\x00\x00\x00')
        eq(pop(ss), 'pop ss', '\x17')
        eq(rol(ebx, 1), 'rol ebx, 0x1', '\xd1\xc3')
        eq(rol(ebx, 2), 'rol ebx, 0x2', '\xc1\xc3\x02')
        eq(rol(edx, cl), 'rol edx, cl', '\xd3\xc2')
        eq(xor(edx, esi), 'xor edx, esi', '\x31\xf2')
        eq(shl(esi, 4), 'shl esi, 0x4', '\xc1\xe6\x04')
        eq(xchg(byte[esp+0x42], al), 'xchg byte [esp+0x42], al',
            '\x86\x44\x24\x42')
        eq(xchg(al, byte[esp+0x42]), 'xchg byte [esp+0x42], al',
            '\x86\x44\x24\x42')
        eq(div(eax), 'div eax', '\xf7\xf0')
        eq(movzx(eax, byte [1]), 'movzx eax, byte [0x1]',
            '\x0f\xb6\x05\x01\x00\x00\x00')
        eq(movsx(eax, al), 'movsx eax, al', '\x0f\xbe\xc0')

    def test_block(self):
        eq = lambda i, s, b: (self.assertEqual(repr(i), s,
            'Invalid string representation for: ' + repr(i)),
            self.assertEqual(str(i), b, 'Invalid encoding for: ' +
                str(i) + ' -> ' + repr(str(i))))

        eq(block(mov(eax, 1), mov(ebx, 1)), 'mov eax, 0x1\nmov ebx, 0x1',
            '\xb8\x01\x00\x00\x00\xbb\x01\x00\x00\x00')

        b = block(mov(eax, ebx))
        b += mov(ecx, edx)
        eq(b, 'mov eax, ebx\nmov ecx, edx', '\x8b\xc3\x8b\xca')

        c = block(mov(esi, dword[eax]), scasb(rep=True))
        eq(c, 'mov esi, dword [eax]\nrep scasb', '\x8b\x30\xf3\xae')

        b += c
        b_s = 'mov eax, ebx\nmov ecx, edx\nmov esi, dword [eax]\nrep scasb'
        b_e = '\x8b\xc3\x8b\xca\x8b\x30\xf3\xae'
        eq(b, b_s, b_e)

        #d = block(xor(eax, eax), lbl, inc(eax), cmp_(eax, 0x10), jnz(lbl(-1)))
        #eq(d, 'xor eax, eax\n__lbl_0:\ninc eax\ncmp eax, 0x10\njnz __lbl_0',
        #    '\x31\xc0\x40\x83\xf8\x10\x0f\x85\xf6\xff\xff\xff')

        # blocks allow instructions / labels without actually creating an
        # instance if that's not required, e.g. instructions that don't take
        # any operators
        #eq(block(jmp(0), nop, lbl, retn), 'jmp __lbl_0\nnop\n__lbl_0:\nretn',
        #    '\xe9\x01\x00\x00\x00\x90\xc3')

        # partially unrolling a useless loop, to show "merging" of blocks.
        e_init = block(xor(ebx, ebx), mov(ecx, 0x40))
        e_init_s = 'xor ebx, ebx\nmov ecx, 0x40'
        e_init_e = '\x31\xdb\xb9\x40\x00\x00\x00'
        e_end = block(mov(eax, dword[esp+8]), retn)
        e_end_s = 'mov eax, dword [esp+0x8]\nretn'
        e_end_e = '\x8b\x44\x24\x08\xc3'
        eq(block(e_init, b, b, b, b, e_end), '\n'.join((e_init_s, b_s, b_s,
            b_s, b_s, e_end_s)), e_init_e + b_e * 4 + e_end_e)

        # merging blocks with relative jumps
        #eq(block(d, d, d), 'xor eax, eax\n__lbl_0:\ninc eax\ncmp eax, 0x10\n' +
        #    'jnz __lbl_0\nxor eax, eax\n__lbl_1:\ninc eax\ncmp eax, 0x10\n' +
        #    'jnz __lbl_1\nxor eax, eax\n__lbl_2:\ninc eax\ncmp eax, 0x10\n' +
        #    'jnz __lbl_2',
        #    '\x31\xc0\x40\x83\xf8\x10\x0f\x85\xf6\xff\xff\xff' * 3)

    def test_optimization(self):
        eq = lambda i, s, b: (self.assertEqual(repr(i), s,
            'Invalid string representation for: ' + repr(i)),
            self.assertEqual(str(i), b, 'Invalid encoding for: ' +
                str(i) + ' -> ' + repr(str(i))))

        # [ebx*2] -> [ebx+ebx]
        eq(mov(eax, dword[ebx*2+3]), 'mov eax, dword [ebx+ebx+0x3]',
            '\x8b\x44\x1b\x03')

if __name__ == '__main__':
    unittest.main(verbosity=2)
