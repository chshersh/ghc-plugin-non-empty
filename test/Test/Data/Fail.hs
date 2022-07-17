{-# OPTIONS_GHC -fplugin=GhcPluginNonEmpty #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Test.Data.Fail
    ( emptyNonEmpty
    ) where

import Data.List.NonEmpty (NonEmpty)


emptyNonEmpty :: NonEmpty Int
emptyNonEmpty = []
