# Prerequisites
llvm 10

# Build


Statement, expression, block header
### Statement
##### return [<*expression*>]
Used to return from functions.
Can return a optional expression that must have the same Type as the specified function return Type.

Implemented in:
- [x] Lexer
- [x] Parser
- [ ] AST

##### yield <*expression*>
 All yield statements in the block must return a value of the same type.
Returns a value from a block of type: 

- If(/else).
- Match

TODO: Add yield to a loop. Either for a new *loop* type or for *for/iterate* or *while*.

##### break
Breaks from a block of type:

- If(/else)
- Match
- For
- While

##### next(continue)
Skips the rest of a loop block and starts the next iteration. Works for:

- For
- While

#### use <*path*>
Import/include statement.

#### package <*path*>
Specifies the path of this file. Is used by the *use*-statement to include it.

#### throw <*expression*>
Throws an exception that can be caught with *catch*.

### Expressions

#### literals
Integer literal:
```
1, -12, +25, 0xf
```
Float/Decimal literal:
```
1.0, -16.32, +0.0
```
Text literal:
```
"A utf-8 encoded string"
```
Character literal:
```
'A', 'Ö'
```

#### integers
*Integer* (see literals for examples of valid formats).

TODO: Number of size... Positive, Natural, Pointer size

TODO: hex. Maybe add ex: (2E16, 5E-12)

#### float
*Float*/*Decimal*

#### variables
```
identifier [@ Type? modifiers*]
```
Does not have specific declaration form, can be declared "as is".

#### operators
Valid expression operators and their precedence (with 1 being the highest):
    precedence -operators
```
1   +x -x
2   x++ x--
3   ++x --x ~ !
4   . (function calls etc.)
5   as
6   ** (power)
7   * / %
8   + -
9   << >>
10   < > <= >= is of
11  == !=
12  &
13  ^
14  |
15  and (bool)
16  or (bool)
17  .. ..=
18  in
19  = += -? *= /= %= **= &= |= ^= <<= >>=
```
All binary operations are evaluated left-to-right except all assignment operations, power(**) and the in-operator.

### Keywords
#### modifiers
A type that specifies the modifiers for a variable. Valid modifiers:

- public (makes this variable visible from all files, default is private)
- private (makes this variable only visible from the enclosing class)
- static
- constant
- owned
- borrowed
- shared
- (atomic)

TODO: move from statement

#### init
Can be used in a constructor to initialize the member variables with the parameters that has the same name.

Example:
```
class A:
    id1 @ Integer
    id2 @ Text
    constructor (id1 @ Integer, id2 @ Text):
        this.init
```
initializes the fields id1 and id2 with the corresponding constructor parameters with the same names.
    
### Block headers

#### class
Valid class header:
```
class name[<generics>] [is <interfaces>]:
```
Examples:
```
class MyClass:
class ClassWithGenerics<T>:
class A is Comparable<A>, Iterable<Integer>:
```
A class can contain member fields, static fields, member methods and static functions.

Has special functions constructor (called when initializing an object) and destructor (called when the object is freed).

#### function
Valid function header:
```
function name<generics>( [variable = default]... ) [@ ReturnType]:
```
Examples:
```
function add(a @ Integer, b @ Integer) @ Integer:
function f(a @ Boolean, b @ Boolean = true):
```
where the *f* function doesn't return anything and its parameter *a* is set to *true* as default. So if it is called without any argument, a gets set to *true*. If one mixes default/non-default parameters, the non-default parameters must be specified before the ones with default values.

Valid function calls for the previous examples:
```
add(1, 2)  ==  add(b = 2, a = 1)  ==  add(1, b = 2)
f(false) or f(true, false)
```
Functions can be called with named arguments. One can mix named and unnamed arguments if the unnamed ones are used before the named arguments.

TODO: Make *method*(s) for classes(?)

#### enum
A list of values.

#### interface(/trait)
Can be implemented by using the "is"-keyword for classes.
Specifies functions that the class then needs to implements, and the class can then be treated as it is of the same type as the interface.

#### macro
```
macro name([variable = default]...):
```
called as name{arguments}.

#### constructor
```
constructor<generics> ([variable = default]...):
```

#### destructor
```
destructor ():
```

#### if/else
Examples:
```
if expression:
if a and b:
if not a and b:   { == if (not a) and b: }

if a:
    ...
else:
    ...

if a:
    ...
else b:
    ...
else:
    ..
```

TODO: Treat if/else as expression so that the return values with *yield*.

TODO: Be moved to expressions(?)

TODO: Only use if/else and never else if(?)

#### match(/switch)
```
match x:
    'a':
        ...
    _:  
        ...

TODO:
match is x:
    Some(a):
        ...
    None:
        ...
```

#### iterate(for)
```
iterate i in Iterable:
iterate i in 0..10:
```

#### while
```
while expression:
while true:
```

#### with
```
with file.open(), lock.unlock():
```
where the "thing" implements *Closable*.

#### catch

#### test
Same as *function* but only run for testing.
    
keywords:
    modifiers
    init
    statements
    block headers
    constructor
    destructor
    is,as,of,in
    
default types:
    i,f,ui,char,byte,Text, boolean, array/vector, map(hash)   
    
    