
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
        eq(str(dword[cs:eax+ebx]), 'dword [cs:eax+ebx]')
        eq(dword[cs:0x13371337], dword[cs:0x13371337])
        eq(str(dword[cs:0xdeadf00d]), 'dword [cs:0xdeadf00d]')
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
        eq = lambda i, s, b: (self.assertEqual(str(i), s,
            'Invalid string representation for: ' + str(i)),
            self.assertEqual(i.encode(), b, 'Invalid encoding for: ' +
                str(i) + ' -> ' + repr(i.encode())))
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

        eq(movss(xmm6, xmm3), 'movss xmm6, xmm3', '\xf3\x0f\x10\xf3')
        eq(movd(xmm7, edi), 'movd xmm7, edi', '\x66\x0f\x6e\xff')
        eq(pand(xmm4, oword [ecx]), 'pand xmm4, oword [ecx]',
            '\x66\x0f\xdb\x21')
        eq(movapd(xmm6, oword [ebx]), 'movapd xmm6, oword [ebx]',
            '\x66\x0f\x28\x33')

        eq(mov(byte[eax], 0x42), 'mov byte [eax], 0x42', '\x80\x00\x42')
        eq(cmp(dword[esp+ecx*8+0x0c], 0x42),
            'cmp dword [esp+ecx*8+0xc], 0x42', '\x83\x7c\xcc\x0c\x42')
        eq(cmp(byte[ebx], 0x13), 'cmp byte [ebx], 0x13', '\x80\x3b\x13')

    def test_block(self):
        eq = self.assertEqual

        eq(len(block(mov(eax, 1), mov(ebx, 1))), 10)
        eq(str(block(mov(eax, 1), mov(ebx, 1))), 'mov eax, 0x1\nmov ebx, 0x1')
        b = block(mov(eax, ebx))
        b += mov(ecx, edx)
        eq((len(b), str(b)), (4, 'mov eax, ebx\nmov ecx, edx'))

        c = block(mov(esi, dword[eax]), scasb(rep=True))
        eq((len(c), str(c)), (4, 'mov esi, dword [eax]\nrep scasb'))
        b += c
        eq((len(b), str(b)), (8, 'mov eax, ebx\nmov ecx, edx\n' +
            'mov esi, dword [eax]\nrep scasb'))

if __name__ == '__main__':
    unittest.main(verbosity=2)
