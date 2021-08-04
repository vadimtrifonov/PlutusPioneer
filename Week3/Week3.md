# Week 3

## Script context

```haskell
data ScriptContext = ScriptContext {
    scriptContextTxInfo :: TxInfo,
    scriptContextPurpose :: ScriptPurpose
}
```

`ScriptContext` is the 3rd parameter of *Validator*

* `ScriptContext`, `ScriptPurpose` and `TxInfo` are defined in `Plutus.V1.Ledger.Contexts` module.

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
	* Transactions that spend a *Validator* script output need to include the *Datum* defined by this script.
	* Transactions that send tokens (produce an output) to a *Validator* script address have to include only its hash.

## Handling time

The time checking (whether it is in the range of `txInfoValidRange`) is done **before** a *Validator* script is run. This allows to keep the script execution fully deterministic. By default all transactions have an infinite time range.

*Slot* is the time unit in Cardano, but Plutus is using epoch time; therefore, a conversion is required. However, there is no guarantee what the *slot* length will be in future. Thus the maximum `txInfoValidRange` is limited to up to **36 hours** in future (or can be indefinite).

`POSIXTimeRange` is a type synonym of `Interval POSIXTime` declared in `Plutus.V1.Ledger.Time` module.

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

Define a new data type for *Datum* that outputs will be required to provide to the *Validator*.

* `beneficiary :: PubKeyHash` - beneficiaries are identified by their public key hash.
	* `PubKeyHash` type is defined in `Plutus.V1.Ledger.Crypto` module. 
* `deriving P.Show` - automatically make `VestingDatum` an instance of `P.Show` typeclass to make it printable in REPL.

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

This *Validator* will check that the transaction is signed by the beneficiary and the current time is after the deadline.

* `info = scriptContextTxInfo ctx` - get `TxInfo` from the context
* `signedByBeneficiary = txSignedBy info $ beneficiary dat`
	* `txSignedBy :: TxInfo -> PubKeyHash -> Bool` - check whether the given `TxInfo` is signed by the given public key hash.
	* `beneficiary dat` - get `beneficiary` from `VestingDatum`
* `deadlineReached = contains (from $ deadline dat) $ txInfoValidRange info`
	* `from $ deadline dat` - create a time interval that starts from `deadline` in `VestingDatum` and until infinity
	* `contains` - check whether the deadline interval (which stretches until infinity) contains the transaction validity interval (which is )

^[One thing that I've noticed is that fields are often prefixed with the name of the type they belong to:  `TxInfo` and `txInfoInputs`. It is likely done to avoid the confusion between field access and other function calls, as their syntax is the same (compared to other languages that have dot notation)]

### Off-chain script

```haskell
{-# LANGUAGE DeriveAnyClass #-}
```

Allows to use `deriving` with any typeclass - only instance declaration will be generated.

```haskell
{-# LANGUAGE DeriveGeneric #-}
```

Allows to use `deriving` for `Generic` typeclass, see [datatype-generic programming](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/generics.html)

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
	* `gpAmount` - an amount of tokens the user wants to vest
* `Endpoint "grab" ()` - an endpoint for collecting the vested tokens
	* The beneficiary will check the UTXO at the vesting address, verify it is the expected beneficiary and the deadline has already passed, and will consume this UTXO.

## Parameterized vesting contract

### On-chain script

```haskell
data VestingParam = VestingParam
    { beneficiary :: PubKeyHash
    , deadline    :: POSIXTime
    } deriving Show

mkValidator :: VestingParam -> () -> () -> ScriptContext -> Bool
```

Instead of requiring outputs to provide parameters via *Datum*, the *Validator* can be changed to accept the parameters from an off-chain script.

```haskell
validator :: VestingParam -> Validator
validator = Scripts.validatorScript . typedValidator
```

`validator` is now a function that requires `VestingParam` to produce an actual `Validator`.

* `validator = Scripts.validatorScript . typedValidator` is equivalent to `validator p = Scripts.validatorScript $ typedValidator p`, see [[Haskell Primer#Composition operator]]

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

* `PlutusTx.liftCode p` - convert `p` to Plutus Core compiled code`CompiledCodeIn`, which is possible for instances of `Lift` typeclass.
	* `liftCode` function is declared in `PlutusTx.Lift` module.
	* `CompiledCodeIn` type is defined in `PlutusTx.Code` module.
	* `Lift` typeclass is declared in `PlutusTx.Lift.Class` module.
*  `PlutusTx.applyCode` - apply compiled `p` to compiled `mkValidator`.
	*  `applyCode` function is declared in `PlutusTx.Code` module.

```haskell
PlutusTx.makeLift ''VestingParam
```

By default `VestingParam` in not an instance of `Lift` typeclass. By providing `Name` of `VestingParam` to `makeLift`, it will generate implementation of `Lift` typeclass instance for  `VestingParam` at compile time.

* `makeLift` is [[Haskell Primer#Template Haskell]] function declared in `PlutusTx.Lift.Class` module.
	* `makeLift :: TH.Name -> TH.Q [TH.Dec]`

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
```

Enables typeclasses with multiple parameters, which is required for `Lift` typeclass, as it has two parameters.

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
