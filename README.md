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
- [x] References
- [x] With statements*
- [ ] Modules
- [ ] Iterators
- [ ] For loops
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

Expressions have mostly the same syntax and precedence rules as C++.


## Memory model

The language features a garbage collector(later), but it's very optional. Because
the language is designed to be embedded and cannot force C++ types to
follow lifetime rules of the garbage collector.

So there exists non-garbage collected references(`&T` and `&mut T`) and garbage collected
references(`^T`). For the non-gc references and non-owning types in general, 
a mechanism is needed to ensure they are never invalid.

The mechanism works on certain assumptions:
- Every non owning type refers to value that was passed into the function that created it
or a global variable
- Every non owning object is valid as long as the object(s) that produce it are never mutated

To achieve memory safety using the above assumptions, the language disallows storing references,
and disallows an owning object to be used more than once per expression if used mutably
```rust
change_arr_and_val(arr: &mut [i32], val: &i32) = {
    arr.push(90); // can invalidate references to the array
    val = 10; 
}
/*  call site  */
arr : mut = [10, 20]
change_arr_and_val(&mut arr, arr[0]); // write to invalid memory
```
If we only allow one source to be used once the above code would fail to compile,
because `arr` is used as a reference twice in the same function.

Not being allowed to store references can be very inefficient however, consider a case where
we have a scene of a game that has to search through a million entities every time we need to retrieve
```rust
Scene: class = {
    entities: [u64 & Entity]; //id and corresponding entity
    get_entity_mut: (&mut this, id: u64) -> {&mut Entity}? = {
        for (entity in this.entities) {
            if(entity.0 == id) return entity;
        }
        return null;
    }
}
```
calling `get_entity` over and over is terrible for performance, that's where the first assumption comes in,
as long as scene is not modified the reference we got is valid, the `with` statement makes sure of this.
The `with` statements gives us a scope where we can extend the lifetime of a non-owning object, but prevents
us from modifying any potential source of the non-owning object.
```rust
main: () = {
    scene: mut Scene;
    // initialize with a million entities somehow
    with entity as scene.get_entity_mut(90) {
        //entity is a {&mut Entity}? and scene cannot be modified
    }
}
```


#### But I want to actually store a reference?

If this sounds like you just use the garbage Collector


