# Aeon

A small language with Go-like syntax that can run on top of XenonCode

## Docs

### Comments
There are two formats for placing comments
* `// rest of line comment` - the rest of the line after `//` is treated as a comment
* `/* multiline/inline */` - the text between `/*` and `*/` is treated as a comment regardless of if it is inline or stretches across multiple lines

### Functions

#### Function Signatures
Functions are declared with the following syntax
- `func name() { /* body */ }` -- Declares a function called "name" with no arguments and no return value
- `func name(a int, b int) { /* body */ }` -- Declares a function that takes two arguments `a` and `b` of type `int`
- `func name(a, b int) { /* body */ }` -- It is possible to omit types of some parameters in function signatures. The parameter takes the type of the following parameter
- `func name() int {/* body */}` -- Declares a function that returns an int

#### Extern Functions
Extern functions allow calling externally defined functions.
Primitives and strings are well-supported but complex types are also allowed.
Complex types will be flattened into function arguments.
Currently return values of `size > 1` are not allowed.
They are declared with the following syntax
- `extern func name()` -- Declares the nullary extern function `name` which corresponds to a XenonCode
function `@name`.
- `extern func name() int` -- Declares the extern function `name` which corresponds to a XenonCode
function with signature `function @name():number`.
- `extern func name() string` -- Declares the extern function `name` which corresponds to a XenonCode
function with signature `function @name():text`.
- `extern func name(int, string) string` -- Declares the extern function `name` which corresponds to a XenonCode
function with signature `function @name($a0:number, $a1:text):text`.
- `extern func name(int, string) string` -- Declares the extern function `name` which corresponds to a XenonCode
function with signature `function @name($a0:number, $a1:text):text`.
- `extern func name([2]int)` -- Declares the extern function `name` which corresponds to a XenonCode
function with signature `function @name($a0:number, $a1:number)`.
- `extern func name([]int)` -- Declares the extern function `name` which corresponds to a XenonCode
function with signature `function @name($a0:number, $a1:number, $a2:number)`.
- `extern func name(*string)` -- Declares the extern function `name` which corresponds to a XenonCode
function with signature `function @name($a0:number)`.
- `extern func name((int, int, string))` -- Declares the extern function `name` which corresponds to a XenonCode
function with signature `function @name($a0:number, $a1:number, $a2:text)`.


#### Directives
Function directives control how functions are handled by the runtime
* `#init` - Causes the function to be called at program start. The function must have the signature `func()`
    ```
    #init
    func name() {
        // body
    }
    ```
* `#update` - Causes the function to be called on each update. The function must have the signature `func()`
    ```
    #update
    func name() {
        // body
    }
    ```
* `#timer interval <integer or constant>` (CURRENTLY UNIMPLEMENTED ) - Causes the function to be called each interval seconds. The function must have the signature `func()`
    ```
    #update
    func name() {
        // body
    }
    ```

### Types
Aeon is a statically typed language with the following types:

#### Primitives
* `int` - An integer of implementation-defined size. The following are valid literals
    * `190` - the decimal value 190
    * `0xabc` - the hexadecimal value `abc`
    * `0o777` - the octal value `777`
    * `0b1111` - the binary value `1111`
    * `0b1111_1111` - `_` may be used as a separator between digits in numeric literals
* `float` - A floating-point number of implementation-defined size
    * `60` - the decimal value `60` treated as a floating point number
    * `0.55` - the value 0.55
* `string` - A textual value
    * `"example string"` - the textual value "example string"
* `bool` - A true or false value
    * `true` - true
    * `false` - false

#### Zero Values
When a variable is unset it contains a default value. Some basic zero values are described below
* `int` -- `0`
* `float` -- `0`
* `bool` -- `false`
* `string` -- `""`
* Tuples -- each element is zero-valued
* Structs -- each field is zero-valued
* Function values -- a non-callable specific zero-value
* Pointers -- a pointer that is numerically `0`
* Arrays -- each element is zero-valued
* Slice -- a slice with capacity 0 and length 0

`nil` may be used as an alias for the zero value of any type. 
This is particularly useful for types without easily writable zero-values.

#### Pointers
Pointer values are an address of a memory location which containes the "pointee" type.
* `*int` is the address of a memory location containing an `int`

#### Tuples
Fixed size groups of potentially different types
* `()` - A zero-element tuple
* `(int)` - A single element tuple containing an integer as its first element
* `(int, float)` - A two element tuple containing an integer as its first element and a float as its second element
* `(int, (string, string))` - A two element tuple containing an integer as its first element and a tuple
  containing two strings as its second element

#### Arrays
Fixed-size collections of the same type. ((unimplemented) An integer constant may be used in place of the size integer)
* `[3]int` -- A 3 element array of `int`s

#### Structs
A collection of named & typed fields. The following describes a struct with the two fields `A`, and `B` where
`A` is an `int` and `B` is a `string`
    ```
    struct {
        A int
        B string
    }
    ```

#### Derived Types
User-defined types may be created with a type declaration. The following defines a type `T` with underlying
type `int`
```
type T int
```
Notably any operations between values of `int` and `T` will cause a compiler error unless an explicit
conversion is made.

More complex types may also be derived. Any valid type expression is allowed in a type declaration. The
following defines a type `X` that is a function that returns a two-tuple of `int`s.
```
type X func() (int, int)
```

#### Function Values
Function references may be used as values. Their values can be typed in the same way as a function signature
```
func x(a int, b int) string

var y func(a int, b int) string = x

```

These values may be called as normal

```
func f(n int) int {
    return n + 1
}

func example() {
    g := f
    g()
}
```

#### Interfaces
Interfaces are defined as sets of methods.
Any type that implements the set of methods implicitly implements the interface.

In the following example `X` implements `I` but `Y` does not implement `I`.
```
type I interface {
    F() int
}

type X int

func (x X) F() int {
    return 0
}

type Y int
```

The empty interface `interface{}` is implemented by every type.
For convenience the empty interface may also be referred to as `any`

When values are boxed into interfaces, only the set of methods in the interface is usable.
Interfaces may be compared against other interface values and will only return true if both the type of the
underlying value of each interface value is equal and if the values themselves are equal under the `==`
operator.

#### Errors
Errors are values in Aeon. They are handled by the `error` type which is defined as follows:
```
type error interface {
    Error() string
}
```

The convention for error returns is for a function to return an error as the last element of a tuple return
value. For example
```
func F() (int, error) {
    return 0, nil
}

func H() (int, bool, error) {
    return 0, false, nil
}
```

A common pattern is to check for an error value and return early after each function that can fail.
```
x, err := F()
if err != nil {
    return err
}
```

#### Error Check Expressions
For convenience there is a special operator `?` which performs an error check and early return. 
The following two snippets are equivalent.
```
func H() error {
    x, err := F()
    if err != nil {
        return err
    }

    return nil
}
```

```
func H() error {
    x := F()?
    return nil
}
```

In general if a function returns `(a,b,c,error)` an `?` expression has a value of `(a,b,c)` and can
potentially return the return value of the containing function. `?` is only valid inside of functions that
have `error` as a return type.

The `?` operator is especially useful for more chaining methods.
```
    x := v.Do()?.The()?.Thing()?
```

#### Error Handler Expressions
Sometimes it is useful to alter an error value before the function returns or even abort returning.

The `->` operator allows defining a function that is run before returning that can modify or cancel an error
return from a `?` expression.
```
    x := v.Do()?.The()?.Thing()? -> handler
```

In the above, `handler` must have type `func(error) error`. If `handler` returns `nil` then the return will be
aborted. This is useful for just logging a non-critical error or recovering from an `error`.

In the case of multiple error handler expressions in the same statement, all handlers will be chained together
starting with the most deeply nested. In the following statement, `v.Do()?` is wrapped by both `handler1` and
`handler2` while the rest of the `?` expressions are only wrapped with `handler2`.

```
    x := (v.Do()? -> handler1).The()?.Thing()? -> handler2
```
