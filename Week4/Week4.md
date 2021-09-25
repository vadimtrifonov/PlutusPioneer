# Week 4

One of the reasons for `PlutusTx.Prelude` module existence is to be an *inlineable* (`{-# INLINABLE ... #-}`) drop-in replacement of Haskell Prelude, which is required for compilation to Plutus Core.

Most Haskell libraries are out of reach for Plutus Core, because they are not written to be *inlineable*.

Off-chain code doesn't have to be complied to Plutus Core and thus devoid of the associated limitations.

## Monads

See [[Haskell Primer#Functor]], [[Haskell Primer#Applicative]], [[Haskell Primer#Monad]]

### IO

See [[Haskell Primer#IO]]

```haskell
main :: IO ()
main = putStrln "Hello World!"
```

This `IO` monad will print "Hello World", but to do that is needs to be executed in `main` or REPL.

```haskell
executable hello
  hs-source-dirs:      app
  main-is:             hello.hs
  build-depends:       base ^>=4.14.1.0
  default-language:    Haskell2010
  ghc-options:         -Wall -O2
```

Executable stanza defined in a Cabal file for the `main` function contained in `hello.hs` file. 

* Haskell modules are required to have a matching file name, but executable modules are allowed to specify a different name.

### Maybe

See [[Haskell Primer#Maybe]], [[Haskell Primer#Pattern matching]]

```haskell
bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing  _ = Nothing
bindMaybe (Just x) f = f x
```

This custom implementation demonstrates mechanics of the bind operator (`>=`).

```haskell
foo' :: String -> String -> String -> Maybe Int
foo' x y z = readMaybe x `bindMaybe` \k ->
             readMaybe y `bindMaybe` \l ->
             readMaybe z `bindMaybe` \m ->
             Just (k + l + m)
```

See [[Haskell Primer#Anonymous functions]], [[Haskell Primer#Infix syntax]]

`bindMaybe` is used as an operator (with infix syntax) to chain multiple `readMaybe`. If any `readMaybe` returns `Nothing`, the result will be `Nothing`. Otherwise the result of `readMaybe` is passed as an argument to an anonymous function which does the same until all three values are acquired. As the last anonymous function is in the scope of the preceding functions it has access to all three values, which it returns combined.

### Either

See [[Haskell Primer#Either]]

```haskell
bindEither :: Either String a -> (a -> Either String b) -> Either String b
bindEither (Left err) _ = Left err
bindEither (Right x)  f = f x
```

Same as with `bindMaybe`, this is a custom implementation of the bind operator (`>=`) for `Either`.

```haskell
foo' :: String -> String -> String -> Either String Int
foo' x y z = readEither x `bindEither` \k ->
             readEither y `bindEither` \l ->
             readEither z `bindEither` \m ->
             Right (k + l + m)
```

The application of `bindEither` is the same as `bindMaybe`, but for `Either.`

### Writer

```haskell
bindWriter :: Writer a -> (a -> Writer b) -> Writer b
bindWriter (Writer a xs) f =
  let
    Writer b ys = f a
  in
    Writer b $ xs ++ ys
```

See [[Haskell Primer#Let]]

* `(Writer a xs)` - pattern match the first argument to make `a` and `xs` available for further computation.
* `Writer b ys = f a` - apply `f` to the result of the first computation `a` to get `Writer b` with the log message `ys`.
* `Writer b $ xs ++ ys` - concatenate the log message of the first computation `xs`  with the log message of the second computation `ys`, and create a new `Writer b` with the concatenated log.
	
```haskell
instance Monad Writer where
    return a = Writer a []
    (>>=) = bindWriter
```

Make `Writer` an instance of `Monad` typeclass by implementing `return` function and `>>=` operator.

```haskell
import Control.Monad
```

```haskell
instance Functor Writer where
    fmap = liftM
```

```haskell
instance Applicative Writer where
    pure = return
    (<*>) = ap
```

This import allows to use `liftM` and `ap` functions to automatically derive implementation of `fmap` and `<*>` from `Monad` implementation.

```haskell
threeInts' :: Monad m => m Int -> m Int -> m Int -> m Int
threeInts' mx my mz = do
    k <- mx
    l <- my
    m <- mz
    let s = k + l + m
    return s
```

See [[Haskell Primer#Do notation]]

A generic function that works with any `Monad` including `Writer`, which implements `Monad` typeclass.

## runEmulatorTrace

```haskell
runEmulatorTrace
    :: EmulatorConfig
    -> FeeConfig
    -> EmulatorTrace ()
    -> ([EmulatorEvent], Maybe EmulatorErr, EmulatorState) 
```

Runs an emulator trace without side effects by returning a tuple of the final state.

*  Defined in `Plutus.Trace.Emulator` module.

### EmulatorConfig

```haskell
newtype EmulatorConfig =
    EmulatorConfig
        { _initialChainState :: InitialChainState
        } deriving (Eq, Show)
```

State of the blockchain at the beginning of the simulation. Can be given as a map of funds to wallets, or as a block of transactions.

* Defined in `Wallet.Emulator.Stream` module

```haskell
instance Default EmulatorConfig where
  def = EmulatorConfig
          { _initialChainState = Left defaultDist
          }
```

`EmulatorConfig` implements an instance of `Default` typeclass, which means it can provide a default value.

```haskell
type InitialChainState = Either InitialDistribution EM.TxPool
```

Initial chain state is defined as either a distribution of initial funds, or a list of initial transaction to be validated.

* Defined in `Wallet.Emulator.Stream` module

#### InitialDistribution

```haskell
type InitialDistribution = Map Wallet Value
```

Initial distribution of tokens specified by key-value pairs of wallet and value. `Value` can be ADA or a native token. 

* Defined in `Plutus.Contract.Trace` module.

```haskell
allWallets :: [EM.Wallet]
allWallets = EM.Wallet <$> [1 .. 10]
```

```haskell
defaultDist :: InitialDistribution
defaultDist = defaultDistFor allWallets
```

This convenience function that creates 10 wallets with 100_000_000 ADA each.

```haskell
defaultDistFor :: [EM.Wallet] -> InitialDistribution
defaultDistFor wallets = Map.fromList $ zip wallets (repeat (Ada.lovelaceValueOf 100_000_000))
```

This convenience function allows to specify wallets for allocation of 100_000_000 ADA each.

#### TxPool

```haskell
type TxPool = [Tx]
```

An initial list of transactions to be validated. 

* Defined in `Wallet.Emulator.Chain` module

### FeeConfig

```haskell
data FeeConfig =
    FeeConfig
        { fcConstantFee :: Ada
        , fcScriptsFeeFactor :: Double
        }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)
```

* Defined in `Ledger.Fee` module.
* `fcConstantFee` - a constant fee per transaction in ADA.
* `fcScriptsFeeFactor` - a factor by which to multiply the size-dependent scripts fee, which depends on memory consumption at runtime (the number of execution steps).

```haskell
instance Default FeeConfig where
  def = FeeConfig { fcConstantFee = Ada.fromValue $ minFee mempty
                  , fcScriptsFeeFactor = 1.0
                  }
```

`FeeConfig` implements an instance of `Default` typeclass, which means it can provide a default value.

### EmulatorTrace

```haskell
type EmulatorTrace a =
        Eff
            '[ RunContract
            , Waiting
            , EmulatorControl
            , EmulatedWalletAPI
            , LogMsg String
            , Error EmulatorRuntimeError
            ] a 
```

`EmulatorTrace` is a monad defined using the effect system. As it is a monad, it can be constructed even without knowing its details, i.e., `return ()`.

*  Defined in `Plutus.Trace.Emulator` module.

## runEmulatorTraceIO

```haskell
runEmulatorTraceIO
    :: EmulatorTrace ()
    -> IO ()
runEmulatorTraceIO = runEmulatorTraceIO' def def def
```

A variation of `runEmulatorTrace` that runs with the default configuration and prints key events to `stdout`.

*  Defined in `Plutus.Trace.Emulator` module.

## runEmulatorTraceIO'

```haskell
runEmulatorTraceIO'
    :: TraceConfig
    -> EmulatorConfig
    -> FeeConfig
    -> EmulatorTrace ()
    -> IO ()
runEmulatorTraceIO' tcfg cfg feeCfg trace
  = runPrintEffect (outputHandle tcfg) $ runEmulatorTraceEff tcfg cfg feeCfg trace
```

A variation of `runEmulatorTraceIO` that allows to specify the configuration for the trace, emulator and fee.

*  Defined in `Plutus.Trace.Emulator` module.

### TraceConfig

```haskell
data TraceConfig = TraceConfig
  { showEvent    :: EmulatorEvent' -> Maybe String
  , outputHandle :: Handle
  }
```
	
A configuration for how and where to print the trace events.

*  Defined in `Plutus.Trace.Emulator` module.
*  `EmulatorEvent' -> Maybe String` - a function that decides whether a specific event should be suppressed or passed through.
*  `Handle` - defined in `System.IO` module, determines where to print the output, defaults to `stdout`.

```haskell
instance Default TraceConfig where
  def = TraceConfig
            { showEvent     = defaultShowEvent
            , outputHandle  = stdout
            }
```

`TraceConfig` implements an instance of `Default` typeclass, which means it can also provide a default value.

#### EmulatorEvent

```haskell
data EmulatorEvent' =
    ChainEvent Chain.ChainEvent
    | ClientEvent Wallet.Wallet NC.NodeClientEvent
    | WalletEvent Wallet.Wallet Wallet.WalletEvent
    | ChainIndexEvent Wallet.Wallet ChainIndex.ChainIndexEvent
    | NotificationEvent Notify.EmulatorNotifyLogMsg
    | SchedulerEvent Scheduler.SchedulerLog
    | InstanceEvent ContractInstanceLog
    | UserThreadEvent UserThreadMsg
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
```

* Defined in `Wallet.Emulator.MultiAgent` module.
 
## BuiltinData

`BuiltinData` is a Plutus Core equivalent of `Data`.  It is required for representing data in on-chain code and can be converted to `Data` for off-chain code.

* Defined in `PlutusTx.Builtins.Internal` module.

## Trace 

```haskell
test :: IO ()
test = runEmulatorTraceIO myTrace
```

A function which uses `runEmulatorTraceIO` to run `myTrace` monad with the default configuration.

```haskell 
myTrace :: EmulatorTrace ()
myTrace = do
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    callEndpoint @"give" h1 $ GiveParams
        { gpBeneficiary = pubKeyHash $ walletPubKey $ Wallet 2
        , gpDeadline    = slotToBeginPOSIXTime def 20
        , gpAmount      = 10000000
        }
    void $ waitUntilSlot 20
    callEndpoint @"grab" h2 ()
    s <- waitNSlots 1
    Extras.logInfo $ "reached " ++ show s
```

* `activateContractWallet (Wallet 1) endpoints` - `endpoints` is the name of the contract in the playground
* `void $ waitUntilSlot 20` - `waitUntilSlot` returns a value indicating that the slot has been reached, which is ignored to avoid a compilation warning.
* `Extras.logInfo $ "reached " ++ show s` - log a message into the trace output

## Contract

```haskell
newtype Contract w (s :: Row *) e a = Contract { unContract :: Eff (ContractEffs w e) a }
  deriving newtype (Functor, Applicative, Monad)
```

```haskell
type ContractEffs w e =
    '[ Error e
    ,  LogMsg Value
    ,  Writer w
    ,  Checkpoint
    ,  Resumable PABResp PABReq
    ]
```

`Contract` monad defines code that will run in a wallet (off-chain).

* Defined in `Plutus.Contract.Types` module.
* `w` - writer for accumulating state, which is used to communicate with other contracts
* `s` - schema (contract endpoints)
* `e` - error type
* `a` - result type

### Contract 1

```haskell
myContract1 :: Contract () Empty Text ()
myContract1 = Contract.logInfo @String "hello from the contract"
```

A simple contract that just logs a message.

* `Contract () Empty Text ()`
	* `()` - no writer for messages
	* `Empty` - no endpoints available
	* `Text` - error type which more efficient that `String`
	* `()` - no result
* `Contract.logInfo @String "hello from the contract"`
	* `Contract.logInfo` - tells the compiler to use `logInfo` of `Contract`
	* `@String` - specifies the type of the polymorphic `ToJSON a` parameter.

```haskell
{-# LANGUAGE TypeApplication #-}
```

Allows to specify the type of polymorphic parameters.

```haskell
logInfo :: ToJSON a => a -> Contract w s e ()
logInfo = Contract . L.logInfo . toJSON
```

Logs a message at the *Info* level.

```haskell
myTrace1 :: EmulatorTrace ()
myTrace1 = void $ activateContractWallet (Wallet 1) myContract1
```

Activate the contract `myContract1` for `Wallet 1`.

```haskell
test1 :: IO ()
test1 = runEmulatorTraceIO myTrace1
```

Run the emulator trace for `myTrace1` to see the log message of the contract `logInfo`.

```haskell
myContract1 :: Contract () Empty Text ()
myContract1 = do
	void $ Contract.throwError "BOOM!" 
	Contract.logInfo @String "hello from the contract"
```

Throws an error during contract validation, before the logging can take place.

### Contract 2 (error type parameter)

```haskell
myContract2 :: Contract () Empty Void ()
myContract2 = Contract.handleError
    (\err -> Contract.logError $ "caught: " ++ unpack err)
    myContract1
```

A contact that cannot produce an error, thus it is being caught and logged. See [[Haskell Primer#Void]]

* `Contract () Empty Void ()`
	* `Void` - a type of error that cannot be constructed, which means no error can be thrown.
* `Contract.handleError`
	* `(\err -> Contract.logError $ "caught: " ++ unpack err)` - transform the error value from `Text` to `Void` by just logging it.
	* `unpack err` - transform `Text` to `String`.
	* `myContract1` - run the contract, which will throw an error

```haskell
handleError ::
  forall w s e e' a.
  (e -> Contract w s e' a)
  -> Contract w s e a
  -> Contract w s e' a
handleError f (Contract c) = Contract c' where
  c' = E.handleError @e (raiseUnderN @'[E.Error e'] c) (fmap unContract f)
```

Transforms the error value `e` to another value `e'`, which can also be an error.

### Contract 3 (schema parameter)

```haskell
type MySchema = Endpoint "foo" Int
```

Define a type synonym for the schema of endpoints.

* `"foo"` - endpoint label, which is a type level string
* `Int` - endpoint parameter type

```haskell
{-# LANGUAGE DataKinds #-}
```

Promotes every suitable datatype to be a kind, and its value constructor to be type constructor, which in our case is required to promote the endpoint label string to a type.

```haskell
myContract3 :: Contract () MySchema Text ()
myContract3 = do
    n <- endpoint @"foo"
    Contract.logInfo n
```

* `n <- endpoint @"foo"` - block the contract execution and wait for the value `n` (`Int`) to be provided

```haskell
myTrace3 :: EmulatorTrace ()
myTrace3 = do
    h <- activateContractWallet (Wallet 1) myContract3
    callEndpoint @"foo" h 42
```

* `callEndpoint @"foo" h 42` - simulates calling the endpoint by its type name `"foo"` and the contract handle `h` with the parameter `42`

```haskell
type MySchema = Endpoint "foo" Int .\/ Endpoint "bar" String
```

Add another endpoint to `MySchema`.

* `.\/` - a type operator which combines our two endpoint types.

```haskell
{-# LANGUAGE TypeOperators #-}
```

Allows to use type operators, in our case to combine two endpoint types.

```haskell
myContract3 :: Contract () MySchema Text ()
myContract3 = do
    n <- endpoint @"foo"
    Contract.logInfo n
    s <- endpoint @"bar"
    Contract.logInfo s
```

Now the contract will wait for two inputs.

```haskell
myTrace3 :: EmulatorTrace ()
myTrace3 = do
    h <- activateContractWallet (Wallet 1) myContract3
    callEndpoint @"foo" h 42
    callEndpoint @"bar" h "Haskell"
```

Simulate two endpoint inputs.

### Contract 4 (writer parameter)

```haskell
myContract4 :: Contract [Int] Empty Text ()
myContract4 = do
    void $ Contract.waitNSlots 10
    tell [1]
    void $ Contract.waitNSlots 10
    tell [2]
    void $ Contract.waitNSlots 10
```

This contract specifies the writer type as `[Int]`, which should be a `Monoid` instance. See [[Haskell Primer#Monoid]].

* `tell [1]` - updates the contract'a accumulating state using `mappend` of the specified `Monoid` instance (the initial value of the state is `mempty`)

```haskell
tell :: w -> Contract w s e ()
tell = Contract . W.tell
```

* Defined in `Plutus.Contract` module

```haskell
myTrace4 :: EmulatorTrace ()
myTrace4 = do
    h <- activateContractWallet (Wallet 1) myContract4

    void $ Emulator.waitNSlots 5
    xs <- observableState h
    Extras.logInfo $ show xs

    void $ Emulator.waitNSlots 10
    ys <- observableState h
    Extras.logInfo $ show ys

    void $ Emulator.waitNSlots 10
    zs <- observableState h
    Extras.logInfo $ show zs
```

`xs <- observableState h` - reads the contract state using the contract handle `h`