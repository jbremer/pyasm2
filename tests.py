
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
        eq(str(eax+esi), 'eax+esi')
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
        i = Instruction()
        eq = self.assertEqual
        m = i.modrm

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

if __name__ == '__main__':
    unittest.main(verbosity=2)
