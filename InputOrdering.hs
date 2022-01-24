{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module InputOrdering where

import Ledger
import Ledger.Typed.Scripts as Scripts
import PlutusTx
import PlutusTx.Prelude hiding (Ordering)

{-# INLINEABLE validateOrdering #-}
validateOrdering :: BuiltinData -> Integer -> ScriptContext -> Bool
validateOrdering _ supposedIndex ctx =
  traceIfFalse "Index supplied is not pointing to the spent output" correctIndexSupplied
  where
    spendingInput :: TxInInfo
    spendingInput = case findOwnInput ctx of
      Just input -> input
      Nothing -> traceError "Spending input not found"

    allInputs :: [TxInInfo]
    allInputs = txInfoInputs $ scriptContextTxInfo ctx

    correctIndexSupplied :: Bool
    correctIndexSupplied = (allInputs !! supposedIndex) == spendingInput

data Ordering

instance Scripts.ValidatorTypes Ordering where
  type DatumType Ordering = BuiltinData
  type RedeemerType Ordering = Integer

typedOrderingValidator :: Scripts.TypedValidator Ordering
typedOrderingValidator =
  Scripts.mkTypedValidator @Ordering
    $$(PlutusTx.compile [||validateOrdering||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @BuiltinData @Integer

orderingValidator :: Validator
orderingValidator = Scripts.validatorScript typedOrderingValidator
