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
