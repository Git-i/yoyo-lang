# Yoyo

Yoyo is (going to be) a statically typed, JIT compiled, embeddable scripting
langauge, written in (and primarily for) C++20.

> [!IMPORTANT]
> This implementation is very far from useful

## Progress

- [x] Functions
- [x] Variables
- [x] Mutability
- [x] If Statements
- [x] Lambdas
- [x] Tuples
- [x] Variants
- [x] Classes
- [x] Structs
- [x] Enums
- [x] Optionals
- [x] Conditional Decomposition
- [x] While loops
- [ ] Modules
- [ ] Iterators
- [ ] For loops
- [ ] With statements*
- [ ] Arrays
- [ ] Span
- [ ] Associative Arrays
- [ ] Garbage Collections
- [ ] Interfaces
- [ ] Schemes
- [ ] Unions
- [ ] Destructors

*With statement is discussed under the memory model

## Syntax

The syntax is heavily inspired by [cpp2/cppfront](https://github.com/hsutter/cppfront)
with minor derivations.

First off, all declarations are of the form:
```
name: kind = value;
```
Notes:
- In some cases(variable declarations) the `kind` can be omitted.
- Function blocks are implicit if they only have one statement 
(`name: () -> i32 = return 5;`)

Expressions have standard syntax and mostly the same precedence rules as C++.

## Functions

A function is declared as
```
name: signature = body
```
The function signature list the inputs and output of the function.
An example signature is shown below:
```
(x: in i32, y: u32, z: inout u64) -> ref i32
```
the `in` and `inout` are the methods the parameter is passed
- `in` takes an argument by copying(if primitive) or `const&`
- `inout` takes an argument by reference

When either is omitted, the parameter defaults to `in`

the `ref` specifies the function returns a reference and can be omitted
## Memory model

The language features a garbage collector, but it's very optional. Because
the language is designed to be embedded and we cannot force C++ types to
follow lifetime rules of the garbage collector, we cannot enforce the use of the
GC.

For safety reasons there are 'no' references. 'no' in quotes for several reasons.
- Function parameters passed with `inout` take a value by reference
- Function with a `ref` return type return a reference

As such there's three sources of mutable 'l-values':
- Member access of a mutable l-value
- Mutable variables
- Functions that return a `ref`

The main goal is to avoid invalid references. Disallowing storing references
makes sense, but re-invoking a function to get a reference can be inefficient,
especially when the function is provided by C++ at runtime and cannot be optimized
away and It also does not completely solve the problem. However, references can only be 
invalid when the object that produced them changed(we assume). If we can ensure that, 
then we can store a reference (No, I don't want to write a borrow checker).

### The Solution(s)

#### Part 1
Only allow a reference source to be used once per expression, consider this:
```
change_arr_and_val(arr: inout [i32], val: inout i32) = {
    arr.push(90); // can invalidate references to the array
    val = 10; 
}
/*  call site  */
arr := [10, 20]
change_arr_and_val(arr, arr[0]); // write to invalid memory
```
For each of the mutable 'l-value' sources, there's other sources that generate them
- Member access of a mutable l-value -> The mutable variable itself
- Mutable variables -> The variable itself
- Functions that return a `ref` -> All `inout` parameters of the function
and their sources

If we only allow one source to be used once the above code would fail to compile,
because `arr` is used as a reference twice in the same function. What if the case
was like this:
```
foo: struct = {...}
change_arr_and_val(arr: inout [foo], val: foo) = {
    arr.push(90);
    val = 10; 
}
/*  call site  */
arr := [foo{...}, foo{...}]
change_arr_and_val(arr, arr[0]); // write to invalid memory is arr[0] is passed by ref
```
Since we've already mutably referenced array once, we're forced to copy
the parameter(I'm not writing a borrow checker).

##### What about lambdas??

Lambdas can capture mutable variables by reference, so using a lambda
(passing it to be called, or calling it) counts as referencing everything it
has captured.

Lambdas also cannot be stored if they capture a mutable variable by reference

#### Part 2

Part 1 solved the safety issue, but there's the issue of having to re-invoke
reference producing functions (like array indexing). The issue becomes apparent
when there is a cost to get the reference, like searching through a container.

This is where `with` statements come in, within a `with` block you're allowed to
actually 'store' a reference.

Syntax:
```
with ref_name as ref_producer {...}
with elem1 as arr[1] {...}
```

Within a `with` block you're allowed to store a reference, but anything involved
in producing that reference becomes immutable.

#### But I want to actually store a reference

Use the garbage Collector


