# pyasm2
&copy; 2012, Jurriaan Bremer

## Introduction

_pyasm2_ is an x86 assembler library. It allows an easy Intel-like assembly
syntax, with support for sequences of instructions, as well as labels.

## Simple Usage

Here are some examples to illustrate the simplicity of pyasm2. For each
example the normal Intel-syntax is given, followed by the equivalent using
pyasm2.

*   `push eax` &rarr; **`push(eax)`**
*   `mov eax, ebx` &rarr; **`mov(eax, ebx)`**
*   `lea edx, [ebp+eax*4+32]` &rarr; **`lea(edx, [ebp+eax*4+32])`**
*   `movzx ebx, byte [esp-64]` &rarr; **`movzx(ebx, byte [esp-64])`**
*   `mov eax, dword fs:[0xc0]` &rarr; **`mov(eax, dword [fs:0xc0])`**

Note that pyasm2 throws an exception if the instruction doesn't support the
given operands (an operand is like a parameter to an instruction.)

A few simple command-line examples.
```python
>>> from pyasm2 import *
>>> mov(eax, dword[ebx+0x100])
mov eax, dword [ebx+0x100]
>>> push(dword[esp])
push dword [esp]
>>> mov(eax, eax, eax) # invalid encoding
... snip ...
Exception: Unknown or Invalid Encoding
```

## Blocks

Besides normal instructions pyasm2 also supports sequences of instructions,
referred to as *blocks* from now on.

Blocks are especially useful when chaining multiple instructions. Besides
that, blocks automatically resolve relative jumps, labels, etc.

A simple example of a function that does only one thing; zero the *eax*
register (the default return value of a function on x86) and returning to the
caller, looks like the following.

```python
Block(
    xor(eax, eax),
    retn()
)
```

Before we discuss further on blocks, we first need an introduction on pyasm2
labels.

## Labels

pyasm2 supports two types of Labels; anonymous labels and named labels.

#### Anonymous Labels

Anonymous labels get an index, and can be referred to by a relative index.

For example, the following block increases the *eax* register infinite times.
(The -1 in this example is a relative index, so -1 points to the last defined
Label.)

```python
Block(
    Label(),
    inc(eax),
    jmp(Label(-1))
)
```

It is, however, not possible to reference to anonymous labels outside of the
current block (i.e. an IndexError is thrown.)

There are three different possible values for relative indices.

*   *Negative Index* &rarr; Points to an anonymous label before the current
    instruction.
*   *Zero Index* &rarr; Points to a transparant label which points to the
    current instruction.
*   *Positive Index* &rarr; Points to an anonymous label after the current
    instruction.

(This does indeed mean that relative index *1* points to the first label
after the current instruction.)

Throughout the following sections we will refer to this snippet, by rewriting
it a little bit every time.

#### Global Named Labels

A new named label can be created by creating a new Label instance with the
name as first parameter. Referencing a named label is just like referencing
an anonymous label, but instead of passing an index, you give a string as
parameter.

```python
Block(
    Label('loop'),
    inc(eax),
    jmp(Label('loop'))
)
```

Note that this type of named label is global, that is, other blocks can
reference to this particular label as well. This is useful for example when
defining a function. (Note that two or more blocks can *not* declare the same
global named labels!)

#### Local Named Labels

Whereas one could make a global named label using e.g. `Label('name')`, it is
also possible to make a *local* named label; a label that's only defined for
the current block. Because local labels are more commonly used than global
labels, their syntax is easier as well. Local named labels are simply created
by using a string as name.

```python
Block(
    'loop',
    inc(eax),
    jmp(Label('loop'))
)
```

#### Label References

Labels are referenced by e.g. `Label('name')`. When looking up label
references, pyasm2 will first try to find the label in the current block,
and only if there is no such label in the current block, it will look it up
in the parent. In other words, local named labels are more important than
global named labels.

### Further Label Tweaks

Now we've seen the types of labels supported by pyasm2, it is time to get to
some awesome tweaks which will speed up development and clean up your code
even further.

#### Simple Named Label References

It is possible to reference a label simply by the name as string, rather than
e.g. `Label('name')`.

```python
Block(
    'loop',
    inc(eax),
    jmp('loop')
)
```

#### Label classobj instead of instance

It is possible to define an Anonymous Label by passing the Label class,
instead of passing an instance.

```python
Block(
    Label,
    inc(eax),
    jmp(Label(-1))
)
```

#### Global Named Labels as variabele

Because global named labels are able to reference to labels outside their
current scope (a block), it is also possible to reference to them as a
variabele (e.g. a function.)

```python
return_zero = Label('return_zero')
f = Block(
    return_zero,
    xor(eax, eax),
    retn()
)
f2 = Block(
    call(return_zero),
    # ... do something ...
)
```

#### Alias Label to L

For those of us that think that the classname *Label* is too long, you could
simply make an alias to **L** (i.e. `L = Label`.)

```python
Block(
    L,
    inc(eax),
    jmp(L(-1))
)
```

#### Tweaked Anonymous Label References

Because `jmp(L(-1))` looks pretty ugly (see the [Alias Label to L][] section),
we've tweaked anonymous label references even further to the point where you
can add or subtract a relative index directly to/from the `Label` class.

[Alias Label to L]: #alias-label-to-l

```python
Block(
    L,
    inc(eax),
    jmp(L-1)
)
```

#### Offset from a Label

Sometimes it might be necessary to add or subtract a value from the address of
a label, in those cases the following technique applies.

```python
Block(
    L,
    nop,
    mov(eax, Label(-1)+1)
)
```

In this example the anonymous label will be referenced, but the value one is
added to it. So `Label(-1)+1` points to the `mov` instruction, because the
`nop` instruction is only one byte in length.

Do note that `Label(-1)+1` could be rewritten as `L-1+1`, but *please* don't
do that, we don't want to torture python.

## Blocks part two

Now we've seen how pyasm2 handles labels, it's time for some more in-depth
information about blocks.

#### Instruction classobj instead of instance

Any instruction that does *not* take any additional operands (e.g. `retn`,
`stosb`, `sysenter`, etc.) can be used directly in a block without actually
making an instance. For example, the following two snippets are equal to
pyasm2.

```python
Block(
    mov(eax, 0),
    retn()
)
```
```python
Block(
    mov(eax, 0),
    retn
)
```

#### Combining Blocks

One can combine multiple blocks by *adding* one to the other. Combining blocks
is actually just merging them, e.g. one block is appended to the other block.

```python
a = Block(
    mov(eax, ebx),
    mov(ebx, 42)
)
b = Block(
    mov(ecx, edx)
)
print repr(a + b)
# Block(mov(eax, ebx), mov(ebx, 42), mov(ecx, edx))
```

#### Temporary Blocks as Lists

Temporary blocks, those that you only use to add to other blocks, can be
written as lists (or tuples, for that matter.)

```python
a = Block(
    mov(eax, ebx),
    mov(ebx, 42)
)
print repr(a + [xor(ecx, ecx), retn])
# Block(mov(eax, ebx), mov(ebx, 42), xor(ecx, ecx), retn)
```

This does, however, not work if you want to call *repr* or *str* on the block.
In that particular case, you can do the following.

```python
a = [xor(eax, eax), retn]
print repr(Block(a))
# Block(xor(eax, eax), retn)
```

#### Combining Instructions Directly

Instead of writing e.g. `Block(mov(eax, ebx), mov(ebx, 42))`, pyasm2 offers a
shorthand.

```python
a = mov(eax, ebx) + mov(ebx, 42)
print repr(a)
# Block(mov(eax, ebx), mov(ebx, 42))
```

## pyasm2 Internals

Although most of pyasm2 is fairly straightforward (chaining instructions is
not that hard), there is one tricky part: **labels**.

To start off, the x86 instruction set provides two types of relative jumps.
Those with an 8bit relative offset, and those with a 32bit relative offset.

Besides that, instructions can refer to other instructions or addresses within
a data section, using labels. This means that pyasm2 has to keep track of
these references, and magically fix them in the final step.

#### Relative Offset Size

So a relative jump can point to another instruction, by using a label. This
raises the question; is the offset to this instruction within the size of an
8bit relative offset, or a 32bit one?

(8bit relative jumps are 2 bytes in length, 32bit ones are 5 bytes for
unconditional jumps, and 6 bytes for conditional ones.)

There are two solutions to this problem, as far as I can tell.

*   Each label keeps a list of instructions pointing to it. When assembling,
    each of the instructions is updated with the location of the label, so the
    instructions can assemble the address or relative offset accordingly.
    From here the instruction can determine if the offset has to be 8bit or
    32bit.
*   At first each relative jump is created using a 32bit relative offset.
    Then, after assembling each instruction, the instructions are enumerated
    and a check is done if the relative jumps would fit as jumps with an 8bit
    relative offset as well. If that is the case, the jump is updated, and all
    the other instructions are updated as well. This goes one until there are
    no relative jumps left to tweak, or a recursive limit has exceeded.

Although the first implementation might be a little better, performance wise.
pyasm2 uses the latter implementation, which is much easier to implement.
