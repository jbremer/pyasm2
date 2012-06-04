# pyasm2
&copy; 2012, Jurriaan Bremer

## Introduction

_pyasm2_ is an x86 assembler library. It allows an easy intel-like assembly
syntax, with support for sequences of instructions, as well as labels.

## Simple Usage

To show the simplicity of the pyasm2 syntax, here are some examples. For each
example the normal intel-syntax is given, followed by the equivalent using
pyasm2.

*   push eax

    `push(eax)`

*   mov eax, ebx

    `mov(eax, ebx)`

*   lea edx, [ebp+eax*4+32]

    `lea(edx, [ebp+eax*4+32])`

*   movzx ebx, byte [esp-64]

    `movzx(ebx, byte [esp-64])`

Note that pyasm2 throws an exception if the instruction doesn't support the
given operands (an operand is like a parameter to an instruction.)

## Blocks

Besides normal instructions pyasm2 also supports sequences of instructions,
referred to as *blocks* from now on.

Blocks are especially useful when chaining multiple instructions. Besides
that, blocks automatically resolve relative jumps, labels, etc.

## Labels

pyasm2 supports two types of Labels; anonymous labels and labels with a name.

### Anonymous Labels

Anonymous labels get an index, and can be referred to by a relative index.

For example, the following block increases the *eax* register infinite times.
```python
Block(
    Label,
    inc(eax),
    jmp(Label(-1))
)
```

### Named Labels

A new named label can be created by creating a new Label instance with the
name as first parameter. Referencing a named label is just like referencing
an anonymous label, but instead of passing an index, you give a string as
parameter. For example, rewriting the anonymous label example to use named
labels, results in the following snippet.
```python
Block(
    Label('loop'),
    inc(eax),
    jmp(Label('loop'))
)
```
