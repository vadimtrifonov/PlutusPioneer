# Week 2

For anything to happen on the blockhain, it has to be externally triggered. For example, in order to finish an auction, a transaction that will close it has to be submitted.

* Each Cardano epoch consists of a number of slots, where each slot lasts for one second. 
* Block is a slot that contains a set of recent transactions on the network. 
* Slots that are inhabited by blocks are called _active_ slots

## Low-level untyped on-chain script

 ```haskell
{-# INLINABLE mkValidator #-}`
```

This macro makes the body of `mkValidator` function inlineable for `[|| ... ||]`. See [[#Pragmas]]

```haskell
{-# LANGUAGE OverloadedStrings #-}
```

This macro allows to produce Plutus strings from string literals.

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
```

This macro excludes the standard `Prelude` from being auto-imported to avoid name clashes with `PlutusTx.Prelude`

```haskell
mkValidator :: Data -> Data -> Data -> ()
mkValidator _ _ _ = ()
```

 The parameters are *Datum*, *Redeemer* and *Context* (transaction info). See [[#Predefined types]].
 
```haskell
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])
```

 Splice the Haskell expression to the Plutus one. This code relies on [[#Template Haskell]].

1. `[|| mkValidator ||]` - convert the `mkValidator` expression to the AST of it
2. `PlutusTx.compile` - compile the Haskell AST of the `mkValidator` and produce the Plutus Core AST
3. `$$(...)` - splice the Plutus Core AST to the Plutus Core expression
4. `mkValidatorScript` - transform the Plutus Core expression of `mkValidator`  to `Validator` 

```haskell
scrAddress = scriptAddress validator
```

The function `scriptLedger`  converts `Validator` (Plutus Core script) into a blockchain address

```haskell
error ()
```

Plutus error function (`PlutusTx.Prelude.error`) that takes `()` and causes the validator to always fail.

```haskell
traceError "Burnt!"
```

Plutus error function that takes `String` and causes the validator to always fail.

## High level typed on-chain script

```haskell
mkValidator :: () -> Integer -> ScriptContext -> Bool
mkValidator = _ r _ = traceIfFalse "wrong redeemer" $ r == 42
```

See [[#Operators]]

* `traceIfFalse` - `PlutusTx.Prelude` function with `String -> Bool -> Bool` signature
	* if the first `Bool` is `False`, the result is also `False` and the provided string will be logged
	* If the first `Bool` is `True`, the result is also `True` and the string is ignored

```haskell
data Typed
instance Scripts.ValidatorTypes Typed where
	type instance DatumType Typed = ()
	type instance RedeemerType Typed = Integer
```

See [[#Data types]], [[#Typeclasses]] and [[#Where]]

1. `data Type` - define a new type called `Typed`
2. `instance Scripts.ValidatorTypes Typed` - make `Typed` an instance of  `ValidatorTypes` typeclass, which wraps *Datum* and *Redeemer* types together
3. `type intance DatumType Typed = ()` - give `()`  type to `DatumType`
4. `type instance RedeemerType Typed = Integer` - give `Integer` type to `RedeemerType`

```haskell
typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
		$$(PlutusTx.compile [|| mkValidator ||])
		$$(PlutusTx.compile [|| wrap ||])
	where
		wrap = Scripts.wrapValidator @() @Integer
```

1. `typedValidator :: Scripts.TypedValidator Typed` - declare `typedValidator` function of type `Scripts.TypedValidator a` (`* -> *`) where `a` is of `Typed` type.
2. `typedValidator = Scripts.mkTypedValidator @Typed` - give `Typed` as an argument to `mkTypedValidator` function.
3. `$$(PlutusTx.compile [|| mkValidator ||])` - splice the Haskell expression to the Plutus one.
4. `$$(PlutusTx.compile [|| wrap ||])` - splice the typed expression to the untyped one using the specified `wrap` function, which converts `Data` arguments to *Datum* and *Redeemer* types.
5. `wrap = Scripts.wrapValidator @() @Integer` - specify `wrap` function as `wrapValidator` function with *Datum* and *Redeemer* type parameters.

```haskell
validator :: Validator
validator = Scripts.validatorScript typedValidator
```

* `Scripts.validatorScript typedValidator` - do conversion from `Scripts.TypedValidator a` to `Scripts.Validator`

```haskell
PlutusTx.unstableMakeIsData ''MyRedeemer
```

[[#Template Haskell]] function that takes the type of *Redeemer* and splices it at compile time to `Data`

* `''MyRedeemer` - quote `MyRedeemer` type as `Name`

***

## Haskell primer

### GHCi (REPL)

`:i` - display the definition of a function, typeclass, or type
`:t` - display the type of an expression
`:k` - display the *kind* of a type
`:l` - load a source
`:r` - reload the loaded sources

### Predefined types

* `()` - Unit type, similar to `void`

### Type variables

```haskell
foo :: a -> a -> Bool
```

* `a` - any type, similar to generics

### Kinds

* `*`  - `Data.Kind` of value types (`Int`, `String`)

### Data types

```haskell
data Name = 
	Constructor1 <type-args> |
	Constructor2 <type-args>
```

* `data` - define a new data type, everything after `=` are constructors. Multiple constructors are separated with `|`.

```haskell
type Palette = [Color]
```

* `type` - create an alias for a data type

Pattern matching constructors: 

```haskell
data Foo = Bar Int | Baz String

fromFoo :: Foo -> String
fromFoo (Bar i) = show i
fromFoo (Baz s) = s
```

### Typeclasses

```haskell
data Foo = Bar Integer | Baz Integer 
instance Eq Foo where
    (==) (Bar n) (Bar m) = n == m
    (==) (Baz n) (Baz m) = n == m
    (==) _ _ = False
```

* `typeclass` - defines behavior, similar to an interface
* `class` - the shorthand for `typeclass`
* `instance` - define the data type to be an instance of a `typeclass`, similar to an interface implementation

```haskell
data Foo = Bar | Baz
	deriving (Read, Show)
```

* `deriving`  - automatically implement the specified `typeclass` on the associated type

### Typeclass constraints

```haskell
foo :: (Eq a) => a -> String
```

* Everything before `=>` is a class constraint, which restricts the type of `a` to instances of class `Eq`

### Operators

* `$` - allows to avoid parentheses by separating expressions and giving *precedence* to anything after it. 
	* `show (1 + 1)` is equivalent to `show $ 1 + 1` 

### Where

`where` allows to define variables at the end of an expression

### Pragmas

* `{-# ... #-}` - [pragmas](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/pragmas.html) are instructions to the compiler (they affect the generated code)

### Template Haskell

See [Template Haskell](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/template_haskell.html)

* AST - Abstract Syntax Tree
* `[|| ... ||]` - typed expression quotation, quotes the expression and produces the AST of it (`Q Exp`)
* `$$(...)` - typed expression splice, converts from the AST to the expression
