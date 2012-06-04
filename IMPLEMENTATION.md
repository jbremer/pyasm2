# pyasm2
&copy; 2012, Jurriaan Bremer

## Introduction

_pyasm2_ is an x86 assembler library. It allows an easy Intel-like assembly
syntax, with support for sequences of instructions, as well as labels.

## Simple Usage

Here are some examples to illustrate the simplicity of pyasm2. For each
example the normal Intel-syntax is given, followed by the equivalent using
pyasm2.

* `push eax` &rarr; **`push(eax)`**
* `mov eax, ebx` &rarr; **`mov(eax, ebx)`**
* `lea edx, [ebp+eax*4+32]` &rarr; **`lea(edx, [ebp+eax*4+32])`**
* `movzx ebx, byte [esp-64]` &rarr; **`movzx(ebx, byte [esp-64])`**
* `mov eax, fs:[0xc0]` &rarr; **`mov eax, [fs:0xc0]`**

Note that pyasm2 throws an exception if the instruction doesn't support the
given operands (an operand is like a parameter to an instruction.)

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

#### Global Named Labels

A new named label can be created by creating a new Label instance with the
name as first parameter. Referencing a named label is just like referencing
an anonymous label, but instead of passing an index, you give a string as
parameter. For example, rewriting the anonymous label example to use global
named labels, results in the following snippet.

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
labels, there syntax is easier as well. Local named labels are simply created
and referenced by using a string as name, tweaking the Global Named Labels
example to use a Local Named Label results in the following.

```python
Block(
    'loop',
    inc(eax),
    jmp('loop')
)
```

### Further Label Tweaks

Now we've seen the types of labels supported by pyasm2, it is time to get to
some awesome tweaks which will speed up development and clean up your code
even further.

#### Label classobj instead of instance

It is possible to define an Anonymous Label by passing the Label class,
instead of passing an instance. Rewriting the Anonymous Label example using
this tweak results in the following snippet.

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
variabele (e.g. a function.) The following example says more than a 1000
words.

```python
return_zero = Label('return_zero')
f = Block(
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
simply make an alias to **L** (i.e. `L = Label`.) Rewriting the anonymous
labels snippet using this alias results in the following.

```python
Block(
    L,
    inc(eax),
    jmp(L(-1))
)
```

#### Tweaked Anonymous Label References

Because `jmp(L(-1))` looks pretty ugly (see the *Alias Label to L* section),
we've tweaked anonymous label references even further, to the following.

```python
Block(
    L,
    inc(eax),
    jmp(L-1)
)
```

As you can see it is now possible to add or subtract the relative index from
the Label class directly.
