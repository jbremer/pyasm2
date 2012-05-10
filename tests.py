
"""

Unittests that verify the integrity of pyasm2.

"""

from pyasm2 import *
import unittest

class CheckSyntax(unittest.TestCase):
    def runTest(self):
        self.assertEqual(str(dword[eax]), 'dword [eax]')
        self.assertEqual(str(byte[eax+eax*4]), 'byte [eax+eax*4]')
        self.assertEqual(str(word[0xdeadf00d+8*esi+esp]),
            'word [esp+esi*8+0xdeadf00d]')
        self.assertEqual(str(eax+esi), 'eax+esi')
        self.assertEqual(str(dword[0x00112233]), 'dword [0x112233]')
        self.assertRaises(AssertionError, lambda: eax+eax+eax)
        self.assertEqual(0xb00b+esp*8+ebx, ebx+esp*8+0xb00b)
        self.assertRaises(AssertionError, lambda: eax+0x111223344)
        self.assertEqual(str(dword[cs:eax+ebx]), 'dword [cs:eax+ebx]')
        self.assertEqual(dword[cs:0x13371337], dword[cs:0x13371337])
        self.assertEqual(str(dword[cs:0xdeadf00d]), 'dword [cs:0xdeadf00d]')

if __name__ == '__main__':
    unittest.main(verbosity=2)
