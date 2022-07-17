{-# OPTIONS_GHC -fplugin GhcPluginNonEmpty #-}

{-# LANGUAGE OverloadedLists #-}

module Test.Data.Overloaded
   ( overloadedListEmpty
   , overloadedListInt
   , overloadedNonEmptyInt
   , overloadedEmptyNonEmpty
   ) where

import Data.List.NonEmpty (NonEmpty)

overloadedListEmpty :: [Int]
overloadedListEmpty = []

overloadedListInt :: [Int]
overloadedListInt = [3, 1, 2]

overloadedNonEmptyInt :: NonEmpty Int
overloadedNonEmptyInt = [15, 6, 7]

overloadedEmptyNonEmpty :: NonEmpty Int
overloadedEmptyNonEmpty = []
