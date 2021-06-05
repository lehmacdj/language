module TestPrelude
  ( module ClassyPrelude,
    module Test.Tasty,
    module Test.Tasty.HUnit,
  )
where

import ClassyPrelude hiding (assert)
import Test.Tasty
import Test.Tasty.HUnit
