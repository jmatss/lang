# Requirements
The strictest requirements can be found in the crate `codegen` because it depends on the crate https://github.com/TheDan64/inkwell to interact with LLVM. This implicitly makes the requirements for this project (copied from `inkwell`):

 * Rust 1.42+
 * Rust Stable, Beta, or Nightly
 * LLVM 10.0 (the current LLVM version used by this project)

For settings up LLVM, the project https://gitlab.com/taricorp/llvm-sys.rs contains good instructions on how to install and setup LLVM correctly (`llvm-sys` is a dependency of `inkwell`).

This project outputs object files, so if you want to create an executable, an external linker is needed. Some files in the `std` library depends on `libc`, so that might also be a requirement depending on the functionality used from the `std` library.


# Build
Runs the few existing unit tests and builds the project.
```
cargo build
```


# Run
```
> lang.exe --help

USAGE:
    lang.exe [FLAGS] [OPTIONS] [INPUTS]...

FLAGS:
    -a, --ast         Set to print AST.
    -h, --help        Prints help information
    -l, --llvm        Set to dump/print the generated LLVM IR code.
    -O, --optimize    Set to run optimization of the LLVM IR.
    -q, --quiet       Set to not print step progress to stdout.
    -v, --validate    Set to only validate the code and skip code generation.
    -V, --version     Prints version information

OPTIONS:
    -i, --input <FILE>       A file containing a list of input files. Relative paths inside the file will be resolved
                             relative to this file path.
    -m, --module <module>    Set the name of the LLVM module. [default: lang_module]
    -o, --output <NAME>      The output name of the produced object file. [default: a.o]
    -t, --triple <triple>    Target triple.

ARGS:
    <INPUTS>...    List of input files.
```

The batch file `scripts/run_tests.bat` can be used to compile and run the system-tests in `tests`. Since the batch file is a hacky solution, it most likely requires some modifications to run on other systems. For example the linker to use and path to the compiler will probably have to be changed.


# Language Information

## Literals

### Integer
```
1, -12, +24, 0xf, 0b1010
```

### Float
```
1.0, -16.32, 3.
```

### String
There are three different string types that are used in the language. 

#### std::string::String
A `std::string::String` is a mutable heap allocated string. The struct contains a pointer to the heap allocation, the size of the heap allocation and the length of the string in bytes. After use the string needs to be deallocated with a call to its `deinit` function.
```
var str: std::string::String = s"A heap allocated string"
str.&.deinit()
```

#### std::string::StringView
Represents a immutable "view" of a string. The struct contains a pointer and the length of the string in bytes. Since the `StringView` isn't in charge of the underlying memory of the string(/pointer), one needs to be careful to not use the `StringView` longer than the lifetime of the underlying string(/pointer).
This should be the preferred string representation in most situation. Creating a "normal" string literal will give back a `StringView` to access/view the static string literal.
```
var view: std::string::StringView = "A static string literal that is accessable through the StringView"
```

### C-string
A null-terminated C string. The type of the C string is a `u8` pointer and is used for interoperability with C code.
```
var c_str: {u8} = c"A null-terminated string literal"
```


### Character
UTF-8 char, represented with type `i32`.
```
'A', 'Ö'
```


## Operators
Operators in order of precedence:
```
0   ( )
1   . :: .* .& .[] .++ .-- (func/method calls, deref, address, indexing, inc, dec etc.)
2   +x -x        (negative/positive)
3   ~
4   as
5   * / %
6   + -
7   << >>
8   < > <= >= is of
9   == !=
10  &
11  ^
12  |
13  not          (!)
14  and          (bool)
15  or           (bool)
16  .. ..=
17  in

(Assignments aren't counted as expression, but they would have the lowest precedence if they were)
18  = += -= *= /= %= *= &= |= ^= <<= >>=
```


## Primitive Data Types
Any integer literals which can't be infered to a type will be given the type `i32`. Any float literal which can't be infered to a type will be given the type `f32`.

 * u8
 * i8
 * u16
 * i16
 * u32
 * i32
 * f32
 * u64
 * i64
 * f64
 * u128
 * i128
 * char (represented as `i32` internally)
 * bool (represented as `i1` internally)


## Array Type
The syntax for an array type is:
```
[<INNER_TYPE>: <SIZE>]

// Example of array containing two `i32` members.
var arr_i32: [i32: 2]
```
where `INNER_TYPE` can be any type.

Accessing a member of an array is done with the `.[]` syntax:
```
var arr_i32: [i32: 2]
arr_i32.[0] = 1
arr_i32.[1] = 2
```


## Pointer Type
The syntax for a pointer type is:
```
{<INNER_TYPE>}

// Example of variable containing a pointer to a `i32` type.
var ptr_i32: {i32}
```
where `INNER_TYPE` can be any type.

Dereferecing a pointer is done with the `.*` syntax and getting the address/pointer of a variable is done with `.&` syntax: 
```
var x: i32 = 123
var x_ptr: {i32} = x.&
var y: i32 = x + x_ptr.*
```


## Modifiers
Modifiers can be specified on ADT's and function declarations. The Modifiers should precede the declaration.

The current `Access Modifiers` are:
 * `priv (private)` - Only accessable from the current file and will not be exposed in the compiled object file. This is the default modifier if none is specified.
 * `hid (hidden)` - Accessable from all files in the current compilation unit (the LLVM module in this case) and will NOT be exposed in the compiled object file.
 * `pub (public)` - Accessable from all files in the current compilation unit and will be compiled as a global/public symbol into the object file. This means that other code can link to this symbol during linking.

*TODO: The modifiers are currently only implemented in the front-end. When the declarations are compiled, they are all compiled to have `external` linkage.*

*TODO: `Static` and `Global` for variables.*


## Variables
A variable is not allowed to shadow a variable with the same name in an outer scope.

### Declaration
```
[var | const] <IDENT> [: <TYPE>] [= <EXPR>]

var x
x = 3

const y = 3

var z: i32 = 3 + y
```


## Functions

### `GENERICS_LIST`
A list of generics/type parameters. The generics are "static dispatch" which means that the generics are replaced and checked at compile-time. A new function is created for every use of a specific generic type.
One can optionally specify a list of `trait`s on the generics to enforce that they must  implement the given `trait`s. This same information can also be specified in a `where`-clause.

### `PARAMETER_LIST`
A comma separate list of parameters to the function. A parameter may optionaly have a default values set. If the caller doesn't specify a value for that parameter, the default value will be used (inserted at the call-site during compilation).
If a parameter has a default value set, there must not exist a parameter to the right of it in the parameter list that doesn't have a default value set.

When calling a function, the arguments are matched with the parameters according to their order. One can use the names of parameters to specify the values. When doing this, one does not have to care about the order of the arguments/parameters. One can also use a mix between using names and indices/order (as long as the compiler is able to deduce the correct order).

### `where`-clause
An optional part that can only exist if the function has declared generics. The where-clause can be used to enforce that a generic type (ex. `T`) implements some specific `trait`s. This allows one to use any functions found in the enforced traits on any expression of type `T`.

### Declaration
```
[<MODIFIERS>] fn <NAME> [<GENERICS_LIST>] ([PARAMETER_LIST]) [where <TRAIT_CONSTRAINTS>]] [-> <RETURN_TYPE>] {
    // ...
}

hid fn func(x: i32 = 5, y: i32 = 0) -> i32 {
    return x - y
}

pub fn generic_func<T: AsView>(x: T) -> T {
    return x
}

fn square(x: i32) -> i32 {
    return x * x
}
```

### Usage
```
std::assert(func(6, 5), 1)
std::assert(func(y = 5, x = 7), 2)
std::assert(func(x = 8, 5), 3)
std::assert(func(4), 4)
std::assert(func(), 5)

std::assert(generic_func<f32>(3.0) == 3.0)

std::assert(square(3) == 9)
```


#### Static Functions
A function can be "static" in a ADT/impl-block which means that it isn't tied to a instance of the ADT/trait (think of it as a Java static function). Example of static function for a struct and how to call it:
```
struct TestStruct {
    fn static_func() -> i32 { return 0 }
}

var x = TestStruct::static_func()
std::assert(x == 0)
```

#### Methods
There can also be methods in a ADT/impl-block which are tied to an instance of the ADT/trait. They are declared with the `this` keyword following the `fn` keyword. Interally, when the methods are compiled, an instance of the ADT/trait are inserted as the first argument of the function call. A method can take the instance either by value or reference(/pointer).
Examples of methods for a struct and how to call them:
```
struct TestStruct {
    var member_a: i32

    fn this method_by_value() {
        this.member_a = 456
    }

    fn {this} method_by_reference() {
        this.*.member_a = 456
    }
}

var test_struct = TestStruct { 123 }

test_struct.method_by_value()
std::assert(test_struct.member_a == 123)

test_struct.method_by_reference()
std::assert(test_struct.member_a == 456)
```


## Algebraic Data Types (ADT)
A collective term used for convenience to refer to types that might consiting of
multiple other types. This includes `struct`, `enum` and `union`. `trait`s are NOT
considered ADT's in the code, but I put them under here in the readme file because I can't be arsed creating a section just for them.

One can specify `impl` blocks for any ADT to implement a specific trait. This impl-block will contain implementations of the the functions declared in the trait. For ADTs one can specify `impl-trait` blocks to implement a specific trait for the given ADT. See the sections `Impl` and `Impl Trait` below for more information.


### Struct
Compiles into a regular C struct, no reordering of the members are done. The generics are "static dispatch" which means that the generic types are determined at compile time and a copy of the struct is created for every unique instance of a generic implementation.

#### Declaration
```
[<MODIFIERS>] struct <NAME> [<GENERICS_LIST> [where <TRAIT_CONSTRAINTS>]] {
    <VARs> ...
    <FUNCTIONs> ...
}

pub struct TestStructA {
    var member_a: i8
    var member_b: f32

    fn example(x: i32) -> i32 {
        return x * 2
    }
}

// In this example the types `K` and `V` are generics. The `where` clause enforces
// that any type `K` must implement the trait `Hashable`. This also allows one to
// use any functions found in trait `Hashable` on the member `member_k`.
struct TestStructB<K, V> where K impls Hashable {
    var member_k: K,
    var member_v: V,
}
```

#### Initialization
```
<STRUCT_NAME> { [<MEMBER_NAME> =] <EXPR> [,] ...  }

var test_struct_a = TestStructA { member_a = 12, member_b = 34.0 }

var test_struct_b = TestStructB { 12, 34.0 }
```


### Enum
The first member of the enum will be given the value `0`, the second member will have the value `1` and so on. The members of the enum will have the type `i32`.

Internally, enums are compiled into structs with a single `i32` member. This means that different enums are NOT compatible. For example looking at the enum declarations in the `Examples` section below, one could not assign a value of `TestEnumB` to a variable containing an instance of the type `TestEnumA`. But since the member are of type `i32`, it is possible to ex. compare `TestEnumA::member_a` with `TestEnumB::member_v`.

#### Declaration
```
[<MODIFIERS>] enum <NAME> {
    [<MEMBER_NAME> [,]]...
}

pub enum TestEnumA {
    member_a,
    member_b,
}

enum TestEnumB {
    member_k,
    member_v,
}
```

#### Access
One accesses a variant/member of an enum with the syntax:
```
<ENUM_NAME>::<MEMBER_NAME>

var enum_variant = TestEnumA::member_b
```
This is NOT a shared access i.e. modyfing the internal `i32` value of the variable `enum_variant` in the example above will NOT modify it for other parts of the code, only the current instance will be changed.


### Union
The union is a "tagged union". It is compiled into a struct containing two members.
The first member will be an array of the type `u8` with size equal to the largest member of the union. The second member is a `u8` field that contains information about which member/variant the current instance of the union represents. The value `0` would represent the first member (top-to-bottom), value `1` the second and so on.

#### Declaration
```
[<MODIFIERS>] union <NAME> [<GENERICS_LIST> [where <TRAIT_CONSTRAINTS>]] {
    <VARs> ...
    <FUNCTIONs> ...
}

pub union TestUnionA {
    var member_a: i8,
    var member_b: f32,

    fn example(x: i32) -> i32 {
        return x * 2
    }
}

union TestUnionB<K, V> {
    member_k: K,
    member_v: V,
}
```

#### Initialization
Initializing a union is similar to initializing a struct. The only difference is that one is required to specify the name of the member to be initialized and only a single member can be spceified in the init.
```
<UNION_NAME> { <MEMBER_NAME> = <EXPR> }

var test_union_a = TestUnionA { member_a = 12 }

// OBS! One has to specify the generics manually since there is no way for the compiler to figure out the correct type for the type `K` in this case.
var test_union_b = TestUnionB<i8, u32> { member_v = 123 }
```

#### Union `is` Match
It is possible to match union variant with a `is` expression.
```
union Result {
    success: u32,
    error: {u8},
}

var result = Result { error = "errror message" }

if err is result.error {
    // If `result` contains the `error` variant (which it does in this case),
    // the variable `err` will now contain the `result.err` value and it can
    // be used in this if-block.
} else {
    // Ends up in here if `result` does NOT contain the `error` variant (which is
    // not the case in this example).
}
```


### Trait
Similar to a rust trait/interface/virtual in other languages. A trait can be used when using generics to ensure that a implementation of that generic implements some specific functions/logic.

The functions/methods declared in the trait should NOT have a body, it should only be function declarations/prototypes. The bodies will be implemented by the ADT that implements the trait.

#### Declaration
```
[<MODIFIERS>] trait <NAME> [<GENERICS_LIST>] {
    [<FN_DECL>]...
}

trait TestTrait {
    fn trait_func(x: u8) -> i32 
}
```


### Impl
`impl` blocks are used to implement traits for a specific ADT. These ADT types can then be used in place of any generics that reqires the type to implement a specific trait.

#### Declaration
```
impl <ADT_NAME>: <TRAIT_NAME> {
    [<TRAIT_FNs>]...
}

trait TestTrait {
    fn trait_func(x: u8) -> i32 
}

struct TestStruct;
impl TestStruct: TestTrait {
    fn trait_func(x: u8) -> i32 {
        return x as i32
    }
}
```

#### Declaration
```
impl <ADT_NAME/TRAIT_NAME> {
    [<FN_DECL>]...
}

impl TestStruct {
    fn func() {}
}
```


## Statements

### mod `<PATH>`
Defines the module(/namespace/path) for the current file. Any declarations done in the file will be prepended with the module name. A file may only contain a single `mod`-statement and that statement must be the first statement in the file. Multiple files can define the same module.

```
mod std::name_space

struct TestStruct;
```
If one wants to access the `TestStruct` in the example above from a file with another `mod` module, it would have to be accessed with the path `std::name_space::TestStruct`. Any file with the same module would be able to access it as `TestStruct`.

### use `<PATH>`
Includes the path/namespace/module `PATH` into the current file. The path can represent either a partial path, ADT or function.
```
use std::assert
use std::mem
use std::mem::Allocator

// Doesn't use any `use` statement, full path specified.
std::assert(true == true)
// Uses the `use std::assert` to find the correct path.
assert(true == true)

// Doesn't use any `use` statement, full path specified.
std::mem::Allocator::init("msg on heap")
// Uses the `use std::mem` to find the correct path.
mem::Allocator::init("msg on heap")
// Uses the `use std::mem::Allocator` to find the correct path.
Allocator::init("msg on heap")
```

### `return [<EXPRESSION>]`
Returns from a function with an optional expression that gets evaluated before returning:

### `break`
Breaks from a block of type:
 * If (/else)
 * While
 * For
 * Match

### `continue`
Skips the rest of a loop-block (for/while) and continues with the next iteration.


## Blocks
Blocks not mentioned here are ADT (`struct`, `union`, `enum`), `trait`, `impl` and function-blocks. There are instead described under the `Algebraic Data Types (ADT)` and `Functions` sections.

### If/else
```
if x == 0 {
    // ...
} else x == 3 {
    // ...
} else {
    // ...
}
```

### Match
```
match x {
    1 {
        // ..
    }
    2 {
        // ..
    }
    default {
        // ..
    }
}
```

### While
```
while x {
    // ..
}
```

### For
*TODO: Implement iterator over some saort of collection*

### Anonymous
Can be used to limit scope.
```
{
    var x = 3
}
// Ok to create `x` variable since previous `x` declared in the anonymous scope
// above has gone out of scope.
var x = 5
```


## Comparison
Any ADT that implements the `std::Eq<T>` trait and its method `eq` can be compared using the `==` or `!=` symbols. The compiler will rewrite the comparison using the symbols into a call to the `std::Eq<T>` traits `eq` method. The `std::Cmp<T>` and its `cmp` method can be used to allow comparisons with the built in `>`, `<`, `>=` and `<=` symbols.
Any primitive type can be compared with the comparison symbols without needing to implement the traits.


## Auto deref
ADT instances can be auto dereferenced depend on context. Given a ADT type `T`, one can access its members or call its methods on a expression of type `T` or `{T}`.
For method calls, the compiler will rewrite the expression (if needed) by adding a deref/address operation to match the form of `this` in the method declaration. For member access on an expression of type `{T}`, a deref operation will be added.


## Built-in functions
Built-in functions can be used in the same way as regular functions. Most of the built-in functions exists because they need some extra logic that needs to interact with the compiler and therefore can't be implemented as regular functions. A few of the built-in functions could be implemented as regular functions, but are built-in for convenience.

A built-in function is called in the same way as a regular function with the exception that they are prepended with an `@` symbol.

```
@ <NAME> [<GENERIC_LIST>] (<PARAMETER_LIST>)
```

### `@size<T>() -> u32`
Gets the size of the specified type `T`.

### `@type(expr: T)`
Gets the type of the expression `expr`. The type will be solved during type inference and can be used anywhere a normal type would be used.

### `@name(var: T) -> std::string::StringView`
Gets the name of the given variable `var`. The given expression `var` must be a reference to an already declared variable. It can be a local, global, ADT member or parameter.

### `@null() -> any`
Creates a null/empty value of a type inferred from its use.

### `@is_null(expr: {T}) -> bool`
Checks if the given argument `expr` is null.

### `@is_not_null(expr: {T}) -> bool`
Checks if the given argument `expr` isn't null.

### `@ptr_add(ptr: {T}, amount: u32) -> {T}`
Adds the value of the second parameter `amount` times the size of the type `T` to the pointer `ptr`.

### `@ptr_sub(ptr: {T}, amount: u32) -> {T}`
Subtracts the value of the second parameter `amount` times the size of the type `T` to the pointer `ptr`.

### `@format(format: std::string::StringView, ...) -> std::string::String`
The first argument of the `format` call is a `StringView` and the rest of the arguments (variadic) are the arguments to the given format string literal. Currently the rest of the arguments must either be `StringView`s or primitives. In the future it would make sense to allow anything that can be turned into a `StringView` or `String` (probably enforces through a trait).
All `{}` found in the `format` string literal will be replace with the arguments to the function in sequential order.

For example:
```
@format("abc{}def{}", 123, 456)
```
would result in a `std::string::String` containing the string "abc123def456".

### `@array(init_value: T, dimension: u32) -> [T: _]`
Creates a instance of an array with the specified length `dimension` and all values initialized to the value `init_value`.

### `@argc() -> u32`
Gets the amount of CLI arguments used when running the program (`argc`). If no `main` function is found in this module, this value will be set to 0.

### `@argv() -> {{u8}}`
Gets the CLI arguments used when running the program (`argv`). If no `main` function is found in this module, this value will be set to 0. The returned value is of type `{{u8}}` (pointer to array of C strings).

### `@file() -> std::string::StringView`
Gets the filename of the file that this built-in call is in.

### `@line() -> u32`
Gets the line number at which this built-in is called.

### `@column() -> u32`
Gets the column number at which this built-in is called.

### `@unreachable()`
Creates a unreachable instruction.