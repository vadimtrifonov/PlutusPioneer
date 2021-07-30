# Week 3

## Script context

`ScriptContext` is the 3rd parameter of *Validator*

```haskell
ScriptContext
	scriptContextTxInfo :: TxInfo
	scriptContextPurpose :: ScriptPurpose
```

`ScriptPurpose` constructors:

* `Minting CurrencySymbol` - describe the circumstances when a native token can be minted or burned
* `Spending TxOutRef` - validate the spending input for the transaction
* `Rewarding StakingCredential` - not explained, related to staking
* `Certifying DCert` - not explained, related to delegation certificates

`TxInfo` fields:

* `txInfoForge :: Value` - the amount of minted, or burned tokens (if the amount is negative)
* `txInfoCert :: [DCert]` - delegation certificates, etc
* `txInfoWdrl :: [(StakingCredential, Integer)]` - withdrawals from rewards
* `txInfoValidRange :: POSIXTimeRange` - the time range during which this transaction is valid (infinite by default)
* `txInfoSignatories :: [PubKeyHash]` - the list of public keys that have signed this transaction
* `txInfoData :: [(DatumHash, Datum)]`
	* Transactions that spend a script output need to include the *Datum* of it
	* Transactions that send value (have output) to a script address have to include only the hash.

The time checking (whether it is in the range of `txInfoValidRange`) is done **before** a validator script is run. This allows to keep the script execution fully deterministic. By default all transactions have an infinite time range.

*Slot* is the time unit in Cardano, but Plutus is using epoch time; therefore, a conversion is required. However, there is no guarantee what the *slot* length will be in future. Thus the maximum `txInfoValidRange` is limited to up to **36 hours** in future or can be indefinite.

`POSIXTimeRange` - alias of `Interval POSIXTime`

`Interval` fields:

* `ivFrom :: LowerBound a`
	* `LowerBound (Extended a) Closure`
		* `Closure` - alias for `Bool`, specified whether this boundary is inclusive
		* `Extended a`
			* `NefInf` - negative infinity
			* `Finite a` - finite value
			* `PosInf` - positive infinity
* `ivTo :: UpperBound a`

`Interval` functions:

* `to` - interval of all values that are lesser than or equal to `a` ^[The documentation of `to` had incorrectness stating that only values smaller than `a` are included in the interval; whereas in the actual implementation `a` is included in the upper boundary]
* `from` - interval of all values that are greater than or equal to `a`

## Vesting contract

### On-chain script

```haskell
data VestingDatum = VestingDatum
    { beneficiary :: PubKeyHash
    , deadline    :: POSIXTime
    } deriving Show
```

* `beneficiary :: PubKeyHash` - beneficiaries are identified by their public key hash
* `deriving P.Show` - automatically make `VestingDatum` an instance of `P.Show` `typeclass`, which makes it printable in REPL

```haskell
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkValidator dat () ctx = 
	traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
    traceIfFalse "deadline not reached" deadlineReached
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info $ beneficiary dat

    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline dat) $ txInfoValidRange info

```

The transaction has to signed by the beneficiary and the current time has to be after the deadline.

* `info = scriptContextTxInfo ctx` - retrieve `TxInfo` from the context
* `signedByBeneficiary = txSignedBy info $ beneficiary dat`
	* `txSignedBy :: TxInfo -> PubKeyHash -> Bool` - check whether the given `TxInfo` is signed by the given public key has
	* `beneficiary dat` - retrieve `beneficiary` from `VestingDatum`
* `deadlineReached = contains (from $ deadline dat) $ txInfoValidRange info`
	* `from $ deadline dat` - create a time interval that starts from `deadline` in `VestingDatum` and until infinity
	* `contains` - check whether the deadline interval (which stretches until infinity) contains the transaction validity interval

^[One thing that I've noticed is that fields are often prefixed with the name of the type they belong to:  `TxInfo` and `txInfoInputs`. It is likely done to avoid the confusion between field access and other function calls, as their syntax is the same (compared to other languages that have dot notation)]

### Off-chain script

```haskell
{-# LANGUAGE DeriveAnyClass #-}
```

Allows to use `deriving` from any class - only instance declaration will be generated.

```haskell
{-# LANGUAGE DeriveGeneric #-}
```

Allows to use [datatype-generic programming](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/generics.html)

```haskell
data GiveParams = GiveParams
    { gpBeneficiary :: !PubKeyHash
    , gpDeadline    :: !POSIXTime
    , gpAmount      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type VestingSchema =
            Endpoint "give" GiveParams
        .\/ Endpoint "grab" ()
```

`VestingSchema` defines the endpoints that will exposed to the user.

* `Endpoint "give" GiveParams`- an endpoint for setting up the vesting contract, which results in creation of UTXO at the vesting address
	* `gpBeneficiary` and `gpDeadline` are required for creation of `VestingDatum`
	* `gpAmount` - the amount of tokens the user wants to vest
* `Endpoint "grab" ()` - an endpoint for collecting the vested tokens
	* The beneficiary will check the UTXO at the vesting address, verify it is the expected beneficiary and the deadline has already passed, and will consume this UTXO

## Parameterized vesting contract

### On-chain script

```haskell
data VestingParam = VestingParam
    { beneficiary :: PubKeyHash
    , deadline    :: POSIXTime
    } deriving Show

mkValidator :: VestingParam -> () -> () -> ScriptContext -> Bool
```

Instead of requiring outputs to provide parameters via *Datum* to the validator, it can be changed to accept the parameters from an off-chain script.

```haskell
validator :: VestingParam -> Validator
validator = Scripts.validatorScript . typedValidator
```

`validator` is now a function that requires `VestingParam` to produce an actual `Validator`.

* `validator = Scripts.validatorScript . typedValidator` is equivalent to `validator p = Scripts.validatorScript $ typedValidator p`. See [[#Function composition]].

```haskell
valHash :: VestingParam -> Ledger.ValidatorHash
valHash = Scripts.validatorHash . typedValidator

scrAddress :: VestingParam -> Ledger.Address
scrAddress = scriptAddress . validator
```

Similarly to `validator`, `valHash` and  `scrAddress` are now also functions that require `VestingParam`.

```haskell
typedValidator :: VestingParam -> Scripts.TypedValidator Vesting
typedValidator p = Scripts.mkTypedValidator @Vesting
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @()
```

* `PlutusTx.liftCode p` - convert `p` to a Plutus Core compiled code (`PlutusTx.Code.CompiledCodeIn`), it possible because it is a data type and an instance of `PlutusTx.Lift`
*  `PlutusTx.applyCode` - apply compiled `p` to compiled `mkValidator`

```haskell
PlutusTx.makeLift ''VestingParam
```

By default `VestingParam` in not an instance of `PlutusTx.Lift`. By providing `TH.Name` of `VestingParam` to `makeLift` it makes it an instance of this typeclass. See [[Week 2#Template Haskell]].

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
```

Enables typeclasses with multiple parameters, which is required for `PlutusTx.Lift`, as it has two parameters.

### Off-chain script

```haskell
type VestingSchema =
            Endpoint "give" GiveParams
        .\/ Endpoint "grab" POSIXTime
```

`Endpoint "grab"` now has to take the deadline as a parameter, because it is no longer accessible from *Datum*.

## Emulator

Getting the public key hash of a wallet in the emulator:

```haskell
> l: src/Week03/Vesting.hs
> import Ledger
> import Wallet.Emulator
> pubKeyHash $ walletPubKey $ Wallet 1
```

* Wallet 1 - `21fe31dfa154a261626bf854046fd2271b7bed4b6abe45aa58877ef47f9721b9`
* Wallet 2 - `39f713d0a644253f04529421b9f51b9b08979d08295959c4f3990ee617f5139f`
* Wallet 3 - `dac073e0123bdea59dd9b3bda9cf6037f63aca82627d7abcd5c4ac29dd74003e`

Getting the POSIXTime based on the time slot:

```haskell
> import Ledger.TimeSlot
> import Data.Default
> slotToBeginPOSIXTime def 10

```

* `def` -  default `SlotConfig` from `Data.Default`
* `slotToBeginPOSIXTime` - get the starting `POSIXTime`of `slot`
* `slotToEndPOSIXTime` - get the ending `POSIXTime`of `slot`

***

## Haskell primer

### Infix syntax

```haskell
> add 2 2
4
> 2 `add` 2
4
```

### Guards

```haskell
describe :: Integer -> String
describe value 
    | value <= 0 = "None"
    | value == 1 = "One"
    | value == 2 = "Two"
    | otherwise = "Many"
```

### Let-in

```haskell
let a = 1; b = 2 in a + b
```

Similar to `where`, but variables defined by `let` are accessible only to the expression after `in`

### Function composition

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
```

Given a function `b` to `c`, and a function `a` to `b`, return a function `a` to `c` - allows to compose two steps into one.

```haskell
(f . g) x = f (g x)
```

Where `f` corresponds to `b -> c` and `g` to `a -> b`.

```haskell
> negate . sum $ [2,2]
-4
> (negate . sum) [2,2]
-4
```

Composition has a precedence of 9 (out of 10) and *right* associativity (whereas function application has a precedence of 10 and *left* associativity); thus, the parentheses or the dollar operator are needed.

### Lists

Add an element to the start of a list:

```haskell
element : list 
```

Add an element to the end of a list:

```haskell
list ++ [element]
```

Get the first element:

```haskell
> head [1,2,3]
1
```

Get all elements without the first one:

```haskell
> tail [1,2,3]
[2,3]
```

Get a new list without the last element:

```haskell
> init [1,2,3]
[1,2]
```

Check if a list is empty:

```haskell
> null []
True
> null [1,2,3]
False
```

Check all booleans in a list:

```haskell
> and [True, False, True]
False
> or [True, False, True]
True
```
