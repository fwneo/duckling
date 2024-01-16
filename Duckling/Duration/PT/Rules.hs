{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Duration.PT.Rules
  ( rules
  ) where

import Data.Text (Text)
import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers (isGrain, duration, isNatural)
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Regex.Types
import Duckling.Time.Helpers
import Duckling.Time.Types (TimeData(..))
import Duckling.Types
import qualified Duckling.Time.Types as TTime
import qualified Duckling.TimeGrain.Types as TG

ruleNCycle :: Rule
ruleNCycle = Rule
  { name = "n <cycles>"
  , pattern =
    [   Predicate isNatural  
    ,   dimension TimeGrain
    ]
  , prod = \case
    (token:_:Token TimeGrain grain:_) -> do
        n <- getIntValue token
        Just . Token Duration $ duration grain n
    _ -> Nothing
  }

ruleCycle :: Rule
ruleCycle = Rule
  { name = "<cycle>"
  , pattern =
    [
      dimension TimeGrain
    ]
  , prod = \case
    (Token TimeGrain grain:_) -> Just . Token Duration $ duration grain 1
    _ -> Nothing
  }


rules :: [Rule]
rules = 
    [   ruleNCycle
    ,   ruleCycle
    ]