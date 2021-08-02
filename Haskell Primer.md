# Haskell Primer

## Week 2

### GHCi (REPL)

* `:i` - display the definition of a function, typeclass, or type
* `:t` - display the type of an expression
* `:k` - display the kind of a type
* `:l` - load a program (a source file)
* `:r` - reload the loaded program

### Unit

* `()` - unit type, similar to `Void` in other languages.
* The unit type can have only one value - the unit value `()` (similar to an empty tuple)

### Wildcard

`_` - wildcard character that matches any value

### Functions 

#### Declaration

```haskell
mkValidator :: Data -> Data -> Data -> ()
```

* `mkValidator` - function name
* `Data -> Data -> Data` - three parameters 
* `()` - return type
* In Haskell all functions with multiple parameters are curried, which allows [partial application](https://wiki.haskell.org/Partial_application)

#### Definition

```haskell
mkValidator _ _ _ = ()
```

* `mkValidator` - function name
* `_ _ _` - three arguments (ignored with wildcards)
* `()` - return value

### Type variables

```haskell
error :: () -> a
```

* `a` - any type, similar to generics in other languages

### Data types

 Haskell data types are similar to `struct` and value types in other languages.

#### Data

```haskell
data Data =
      Constr Integer [Data]
    | Map [(Data, Data)]
    | List [Data]
    | I Integer
    | B BS.ByteString
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)
```

* `data` - define a new data type
* `=` - everything after `=` are constructors
* `Constr` - constructor name
* `Integer [Data]` - constructor parameters
*  `|` - separates multiple constructors
* `Map`, `List`, `I`, `B` - other constructor names
* `deriving` - see [[#Typeclasses]]

#### Newtype

```haskell
newtype MySillyRedeemer = MySillyRedeemer Integer
```

* `newtype` - special case of `data` declaration that permits only one constructor and only one parameter
* `MySillyRedeemer` - name
* `MySillyRedeemer Integer` - constructor

#### Type

```haskell
type POSIXTimeRange = Interval POSIXTime
```

* `type` - define a type synonym (i.e., alias) for the specified data type
* `Interval` - data type declared as `Interval a`
* `POSIXTime` - type argument for `a` in `Interval` declaration

### Typeclasses

Haskell `typeclass` (or the shorthand `class`) is similar to `interface` or `protocol` in other languages.

```haskell
class ValidatorTypes (a :: Type) where
    type RedeemerType a :: Type
    type DatumType a :: Type
```

* `class` - declare a new typeclass
* `ValidatorTypes` - typeclass name
* `(a :: Type)` - type variable
* `type` - declare an associated type synonym (see [type families](https://wiki.haskell.org/GHC/Type_families))
* `RedeemerType` - type synonym name

#### Instance

```haskell
data Typed
instance Scripts.ValidatorTypes Typed where
	type instance DatumType Typed = ()
	type instance RedeemerType Typed = Integer
```

* `instance` - declare the data type as an instance of the specified typeclass, similar to `interface` or `protocol` implementation in other languages.
* `Scripts.ValidatorTypes` - typeclass
* `Typed` - type the instance is being provided for
* `where` - terminates the initial declaration and follows up with the implementation

#### Deriving

```haskell
data Data =
      Constr Integer [Data]
    | Map [(Data, Data)]
    | List [Data]
    | I Integer
    | B BS.ByteString
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)
```

* `deriving`  - automatically make this type an instance of the specified typeclasses
* `stock, anyclass` - [deriving strategies](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/deriving_strategies.html?highlight=deriving%20strategies)
* `(Show, Eq, Ord, Generic)` - typeclasses to be derived

### Guards

```haskell
mkValidator :: Data -> Data -> Data -> ()
mkValidator _ r _
    | r == I 42 = ()
    | otherwise = traceError "wrong redeemer"
```

* `|` - guard case
* `r == I 42` - expression used to test this case (must evaluate to `Bool`)
* `= ()` - expression to be evaluated and returned, if the test is `True`
* `otherwise` - default case which always evaluates to `True`
* Guards without pattern matching are similar to `if-else` statements in other languages.
* Guard with pattern matching are similar to `switch` statements in other languages.

### Where

```haskell
typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @Integer
```

* `where` - allows to define local functions and constants
* `where` definitions are scoped to the current function 

### Dollar operator

```haskell
traceIfFalse "wrong redeemer" $ r == 42
```

* Equivalent to `traceIfFalse "wrong redeemer" (r == 42)`
* `($) :: (a -> b) -> a -> b`
* `$` allows to avoid parentheses by separating expressions and giving *precedence* to anything after it.
* `$` is an infix operator with right *associativity* and the lowest *precedence* possible - 0, whereas normal function application is left *associative* and has the highest *precedence* possible - 10.

### Pragmas

```haskell
{-# INLINABLE mkValidator #-}`
```

* `{-# ... #-}` - [pragmas](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/pragmas.html) are instructions to the compiler (they affect the generated code)

### Template Haskell

[Template Haskell](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/template_haskell.html) is used for compile-time metaprogramming.

```haskell
$$(PlutusTx.compile [|| mkValidator ||])
```

* AST - Abstract Syntax Tree
* `[|| ... ||]` - typed expression quotation, quotes the expression and produces the AST of it (`Q Exp`)
* `$$(...)` - typed expression splice, converts from the AST to the expression
