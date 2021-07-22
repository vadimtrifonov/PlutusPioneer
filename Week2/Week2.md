# Week 2

For anything to happen on the blockhain, it has to be externally triggered. For example, in order to finish an auction, a transaction that will close it has to be submitted.

* Each Cardano epoch consists of a number of slots, where each slot lasts for one second. 
* Block is a slot that contains a set of recent transactions on the network. 
* Slots that are inhabited by blocks are called _active_ slots

## GHCi (REPL)

`:i` - display information (definition)  
`:t` - display the type of an expression  
`:k` - display the *kind* of a type  
`:l` - load a source  
`:r` - reload the loaded sources  

## Low-level untyped on-chain script

```haskell
mkValidator :: Data -> Data -> Data -> ()
```

 The parameters are *Datum*, *Redeemer* and *Context* (transaction)

* `()` - Unit type, similar to `void`
* `a` - any type, similar to generics

 ```haskell
{-# INLINABLE mkValidator #-}`
```

This macro makes the body of `mkValidator` function inlinable for `[|| ... ||]`

* `{-# ... #-}` - [pragmas](https://downloads.haskell.org/~ghc/7.0.3/docs/html/users_guide/pragmas.html) (compiler instructions)

```haskell
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])
```

The code above relies on [Template Haskell](https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/template-haskell.html):

* AST - Abstract Syntax Tree
* `[|| ... ||]` - typed expression quotation, quotes the expression and produces the AST of it (`Q Exp`)
* `$$(...)` - typed expression splice, converts from the AST to the expression

1. `[|| mkValidator ||]` - convert the `mkValidator` expression to the AST of it
2. `PlutusTx.compile` - compile the Haskell AST of the `mkValidator` and produce the Plutus Core AST
3. `$$(...)` - splice the Plutus Core AST to the Plutus Core expression
4. `mkValidatorScript` - transform the Plutus Core expression of `mkValidator`  to `Validator` 

```haskell
scrAddress = scriptAddress validator
```

The function `scriptLedger`  converts `Validator` (Plutus Core script) into a blockchain address

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
```

This macro excludes standard `Prelude` from being auto-imported to avoid name clashes with `PlutusTx.Prelude`

`error ()` -  Plutus error function (`PlutusTx.Prelude.error`) that takes `()` and causes the validator to always fail.

```haskell
{-# LANGUAGE OverloadedStrings #-}
```

This macro allows to produce Plutus strings from string literals.

`traceError "Burnt!"` -  Plutus error function that takes `String` and causes the validator to always fail.

### High level typed on-chain script

```haskell
mkValidator = _ r _ = traceIfFalse "wrong redeemer" $ r == 42
```

* `traceIfFalse` - `PlutusTx.Prelude` function with `String -> Bool -> Bool` signature
    * if the first `Bool` is `False`, the result is also `False` and the provided string will be logged
    * If the first `Bool` is `True`, the result is also `True` and the string is ignored
* `$` - allows to avoid parentheses by separating expressions and giving *precedence* to anything after it. 
    * `show (1 + 1)` is equivalent to `show $ 1 + 1` 

```haskell
data Typed
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed = ()
    type instance RedeemerType Typed = Integer
```

* `typeclass` - similar to an interface that defines some behavior
* `class` - shorthand for `typeclass`

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

* `*`  - `Data.Kind` of value types (`Int`, `String`)

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

[Template Haskell](https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/template-haskell.html) function that takes the type of *Redeemer* and splices it at compile time to `Data`

`''MyRedeemer` - quote `MyRedeemer` type as `Name`
