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

The time checking (whether it is in the range of `txInfoValidRange`) is done **before** a validator script is run. This allows to keep the script execution fully deterministic. By default all transactions have infinite time range.

*Slot* is the time unit in Cardano, but Plutus is using real time; therefore, a conversion is required. However, there is no guarantee what the *slot* length will be in future. Thus the maximum `txInfoValidRange` is limited to up to **36 hours** in future or can be infinite.

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

## Vesting contract

```haskell
data VestingDatum = VestingDatum
	{ beneficiary1 :: PubKeyHash
	, beneficiary2 :: PubKeyHash
	, deadline     :: POSIXTime
	} deriving P.Show
```

* `beneficiary1 :: PubKeyHash` - beneficiaries are identified by their public key hash
* `deriving P.Show` - make `VestingDatum` part of the `P.Show` `typeclass`, which makes it printable in REPL

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
