# Week 2

For anything to happen on the blockhain, it has to be externally triggered. For example, in order to finish an auction, a transaction that will close it has to be submitted.

* Each Cardano epoch consists of a number of slots, where each slot lasts for one second. 
* Block is a slot that contains a set of recent transactions on the network. 
* Slots that are inhabited by blocks are called _active_ slots

## Untyped on-chain script

### Data

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

Low-level type for Plutus Core script, analogous to JSON. See [[Haskell Primer#Data types]] and [[Haskell Primer#Typeclasses]].

* `Data` is defined in `PlutusCore.Data` module (re-exported from `PlutusTx` module).

### Gift contract

```haskell
mkValidator :: Data -> Data -> Data -> ()
mkValidator _ _ _ = ()
```

The most simple *Validator* that will always pass the validation. Any token amount at this script address can be consumed by anyone.

 * `mkValidator` function declaration and definition, see [[Haskell Primer#Functions]]. 
 * The parameters are *Datum*, *Redeemer* and *Context* (transaction info). 
 * The return type is [[Haskell Primer#Unit]].
 * The arguments are ignored with [[Haskell Primer#Wildcard]].
 
```haskell
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])
```

  Transform the Haskell expression to the Plutus expression, this relies on [[Haskell Primer#Template Haskell]].
 
*  `mkValidatorScript` is declared in  `Plutus.V1.Ledger.Scripts` module.
	*  `mkValidatorScript :: CompiledCode (Data -> Data -> Data -> ()) -> Validator`.

1. `[|| mkValidator ||]` - quote the `mkValidator` expression to get the abstract syntax tree (AST) of it.
2. `PlutusTx.compile` - compile the Haskell AST of the `mkValidator` and produce the Plutus Core AST.
3. `$$(...)` - splice the Plutus Core AST to the Plutus Core expression.
4. `mkValidatorScript` - transform the Plutus Core expression of `mkValidator`  to `Validator` .

 ```haskell
{-# INLINABLE mkValidator #-}`
```

This pragma makes the `mkValidator` function inlineable for `[|| ... ||]`. See [[Haskell Primer#Pragmas]].

```haskell
valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator
```

Transform `Validator` (Plutus Core script) into a validator hash represented by `ValidatorHash` type.

* `validatorHash` is declared in `Plutus.V1.Ledger.Scripts` module.
	* `validatorHash :: Validator -> ValidatorHash`

```haskell
scrAddress :: Ledger.Address
scrAddress = scriptAddress validator
```

Transform `Validator` into an actual blockchain address represented by `Address` type.

* `scriptAddress` is declared in `Plutus.V1.Ledger.Address` module.
	* `scriptAddress :: Validator -> Address`

### Burn contract

```haskell
mkValidator :: Data -> Data -> Data -> ()
mkValidator _ _ _ = error()
```

Such *Validator* will always fail validation.

* `error` is declared in `PlutusTx.Builtins` module.
	* `error :: () -> a` - where `a` is any type, see [[Haskell Primer#Type variables]]

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
```

This pragma excludes Haskell `Prelude` from being auto-imported to avoid name collisions with `PlutusTx.Prelude`.

```haskell
mkValidator :: Data -> Data -> Data -> ()
mkValidator _ _ _ = traceError "BURNT!"
```

This *Validator* will always fail with the specified error message.

* `traceError` is declared in `PlutusTx.Prelude` module.
	* `traceError :: Builtins.BuiltinString -> a`

```haskell
{-# LANGUAGE OverloadedStrings #-}
```

This pragma allows to produce Plutus strings from string literals.

### FortyTwo contract

```haskell
mkValidator :: Data -> Data -> Data -> ()
mkValidator _ r _
    | r == I 42 = ()
    | otherwise = traceError "wrong redeemer"
```

This *Validator* will pass validation only if *Redeemer* is `42`.

* [[Haskell Primer#Guards]] are used to test the redeemer argument.
* `r == I 42` - construct a new `Data` value as `Integer` of `42` and check whether it matches the *Redeemer* `Data` value.

## Typed on-chain script

### Typed contract

```haskell
mkValidator :: () -> Integer -> ScriptContext -> Bool
mkValidator = _ r _ = traceIfFalse "wrong redeemer" $ r == 42
```

This *Validator* requires specific types of *Datum*, *Redeemer*, and *Context*. It will also log the message, if *Redeemer* is not `42`.

* `traceIfFalse` is declared in `PlutusTx.Prelude` module.
	* `traceIfFalse :: Builtins.BuiltinString -> Bool -> Bool`.
	* This function returns the given `Bool` argument, and logs the specified string, if it is `False`.
* `$ r == 42` - is equivalent to `(r == 42)`, see [[Haskell Primer#Dollar operator]].

```haskell
data Typed
instance Scripts.ValidatorTypes Typed where
	type instance DatumType Typed = ()
	type instance RedeemerType Typed = Integer
```

Define a dummy data type `Type` and make it an instance of `ValidatorTypes` typeclass. See [[Haskell Primer#Data types]] and [[Haskell Primer#Typeclasses]].

* `ValidatorTypes` is declared in `Ledger.Typed.Scripts.Validators` module.

```haskell
class ValidatorTypes (a :: Type) where
    type RedeemerType a :: Type
    type DatumType a :: Type
```

* `type instance DatumType Typed = ()` - define the type synonym for *Datum* as `()` 
* `type instance RedeemerType Typed = Integer`  - define the type synonym for *Redeemer* as `Integer` 

```haskell
typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
		$$(PlutusTx.compile [|| mkValidator ||])
		$$(PlutusTx.compile [|| wrap ||])
	where
		wrap = Scripts.wrapValidator @() @Integer
```

Transform the Haskell type validator expression to the Plutus typed validator expression.

*  `TypedValidator (a :: Type)` type is declared in `Ledger.Typed.Scripts.Validators` module.
*  `mkTypedValidator` function is also declared in  `Ledger.Typed.Scripts.Validators` module.
	*  `mkTypedValidator :: CompiledCode (ValidatorType a) -> CompiledCode (ValidatorType a -> WrappedValidatorType) -> TypedValidator a`

1. `typedValidator :: Scripts.TypedValidator Typed` - declare `typedValidator` function with `TypedValidator Typed` return type (where `Typed` is the type argument for `TypedValidator`).
2. `typedValidator = Scripts.mkTypedValidator @Typed` - give `Typed` as the type argument to `mkTypedValidator` function.
3. `$$(PlutusTx.compile [|| mkValidator ||])` - splice the Haskell expression to the Plutus expression.
4. `$$(PlutusTx.compile [|| wrap ||])` - splice the typed expression to the untyped one using the `wrap` function definition.
5. `wrap = Scripts.wrapValidator @() @Integer` - define `wrap` function as `wrapValidator` function, which transforms  `Data` arguments to the typed arguments of *Datum* and *Redeemer* - `()` and `Integer` respectively.
	
```haskell
validator :: Validator
validator = Scripts.validatorScript typedValidator
```

Transform the typed validator to the untyped one.

* `validatorScript` is declared in `Ledger.Typed.Scripts.Validators` module.
	* `validatorScript :: TypedValidator a -> Scripts.Validator`

```haskell
valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typeValidator
```

Transform `TypedValidator Typed` (Plutus Core script) into a validator hash represented by `ValidatorHash` type.

* The typed version of `validatorHash` is declared in `Ledger.Typed.Scripts.Validators` module.
	* `validatorHash :: TypedValidator a -> Scripts.ValidatorHash`

### IsData contract

Conversion between `Data` and specific types is achieved by implementing an instance of `IsData` typeclass declared in `PlutusTx.IsData.Class` module.

```haskell
class IsData (a :: Type) where
    toData :: a -> Data
    fromData :: Data -> Maybe a
```

For some types like `Data`, `Integer`, `ByteString`, `[a]`, `Void` the instances are already predefined.

```haskell
newtype MySillyRedeemer = MySillyRedeemer Integer
```

Define a simple data type `MySillyRedeemer`.

```haskell
PlutusTx.unstableMakeIsData ''MySillyRedeemer
```

At compile time, implementation of `IsData` typeclass instance for  `MySillyRedeemer` will be generated.

* `unstableMakeIsData` is [[Haskell Primer#Template Haskell]] function declared in `PlutusTx.IsData.TH` module.
	* `unstableMakeIsData :: TH.Name -> TH.Q [TH.Dec]`
* `''MySillyRedeemer` - quote `MySillyRedeemer` type as `TH.Name`
