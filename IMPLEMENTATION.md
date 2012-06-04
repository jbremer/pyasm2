# pyasm2
&copy; 2012, Jurriaan Bremer

## Introduction

_pyasm2_ is an x86 assembler library. It allows an easy intel-like assembly
syntax, with support for sequences of instructions, as well as labels.

## Simple Usage

Some examples to show different types of instructions, the intel syntax is
given, followed by the pyasm2 equivalent.

*   push eax

    `push(eax)`

*   mov eax, ebx

    `mov(eax, ebx)`

*   lea edx, [ebp+eax*4+32]

    `lea(edx, [ebp+eax*4+32])`

*   movzx ebx, byte [esp-64]

    `movzx(ebx, byte [esp-64])`

## Labels
