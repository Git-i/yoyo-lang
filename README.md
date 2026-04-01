# Yoyo

Yoyo is (going to be) a statically typed, research language, written in C++20.

> [!IMPORTANT]
> This implementation is very far from useful

## Syntax

The syntax is heavily inspired by [cpp2/cppfront](https://github.com/hsutter/cppfront)
with minor derivations.

First off, all declarations are of the form:

```cppfront
name: kind = value;
```

Notes:

- In some cases(variable declarations) the `kind` can be omitted.
- Function blocks are implicit if they only have one statement
(`name: () -> i32 = return 5;`)

Expressions have mostly the same syntax and precedence rules as C++.

## This branch

This branch allows the compiler to write its output to files that can be parsed
by other programs
