{-# OPTIONS_GHC -fplugin=GhcPluginNonEmpty #-}

module Test.Data.Plain
    ( emptyList
    , listInt
    , nonEmptyListInt
    , nonEmptyListBool
    , nonEmptyListSingleton
    , nonEmptyListExplicit
    ) where

import Data.List.NonEmpty (NonEmpty (..))


emptyList :: [Int]
emptyList = []

listInt :: [Int]
listInt = [3, 1, 2]

nonEmptyListInt :: NonEmpty Int
nonEmptyListInt = [5, 10, 7]

nonEmptyListBool :: NonEmpty Bool
nonEmptyListBool = [True, False]

nonEmptyListSingleton :: NonEmpty Int
nonEmptyListSingleton = [42]

nonEmptyListExplicit :: NonEmpty Int
nonEmptyListExplicit = 42 :| [50, 100]
