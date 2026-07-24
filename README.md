# Yoyo

Yoyo is (going to be) a statically typed, research language, written in C++20.

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

## Memory Safety

The memory safety approach used here is a variation of the borrow checker inspired
by Polonius, [Group Borrowing](https://verdagon.dev/blog/group-borrowing), and
[Ante](https://antelang.org/).

The borrow checker performs a flow sensitive points-to analysis, every reference (or view-like object)
has an associated domain, which is just the points-to set of the object, multiple
objects can share one domain (e.g all array elements, recursive struct fields, etc.), and importantly
domains are typically inferred when writing code (save for declarations). e.g

```yoyo
Vec: struct = { x: i32, y: i32 }
main: fn = {
    value := Vec{.x = 100, .y = 200};
    val_ref: &Vec = &value;
    val_ref_ref: &&Vec = &val_ref;

    val_x: &i32 = &(*val_ref_ref).x;
}
```
In this example we can assign domains as such:
```
val_ref -> &'a Vec
val_ref_ref -> &'b &Vec
val_x -> &'c i32
```
One important thing to note is that domain even though its written that way,
domains are not associated with types, `val_ref_ref` only has a domain on the top level reference,
because domains are a property of the object, an object can have multiple domains if for example
its a struct where each field can point to different values at different times.

The code is lowered to an IR to make control flow explicit, and then a flow insensitive points-to analysis
is performed (the implementation uses anderson's pointer analysis).
From the flow insensitive analysis we can information that helps us deal with pointer-to-pointers,
for example we know that *val_ref_ref can only refer to references val_ref_ref has ever pointed to,
so we can add extra instructions for those domains, specifically (may-store and may-load instructions).

All uses of domains in the IR are then converted to SSA, then a final analysis is performed, and because of
the SSA, its flow sensitive. Knowing what every reference may point to at a specific point in the program allows us
make fairly lenient (but still correct) detections of unsafe code.

The detection process uses a standard forward dataflow to propagate valid domains across the Control Flow Graph.
The input set to an instructions (domains valid to use in that instruction) is the intersection of the
output sets of all preceeding instructions (a domain remains valid in the instructions, if it was valid
all paths reaching this point), and the output set depends on the instruction:

- If the instruction created a new reference(borrow, function-call, struct init, etc.),
it adds that domain to the output set.

- If the instruction assigned to a value, all domains that may point to a value that will be invalidated
are excluded from the output set. (The invalidation criteria is explained after)

- If the instruction drops a value, all domains that may point to a value that will be invalidated
are excluded from the output set. (The invalidated criteria is explained after)

### Invalidation criteria
The invalidation criteria depends on the concepts of stable and unstable paths:

- A stable path is one that when the parent is modified, the children still remain valid
to access, for example if a struct is modified all the fields still points to valid object
of the same type, so the path from a struct object to one of its fields is stable.

- An unstable path is one that when the parent is modified the children might not remain
valid, for example if a union is modified references within it, might point to the wrong type.

Stable paths are used for structs, tuples, and static arrays, and unstable paths are used for
unions and most dynamic memory.

The invalidation criteria for assignment is that when an object has been assigned to, if there exists
an unstable path from the object to a value a domain points to, that domain is invalidated.

The invalidated criteria for drops is that when an object has been dropped, all pointers to it, and
any of its children become invalidated.

> [!NOTE]
> The IR doesn't carry mutability information, so the entire thing
> supports mutable aliasing.

### Work In Progress

There has been some work with regard to functions, the code supports approximating points-to
information across function boundaries, but I haven't found a way to describe aliasing
of function inputs in the function signature.

I also highly doubt this system is thread safe, but I think there's a possibility for that to happen.


