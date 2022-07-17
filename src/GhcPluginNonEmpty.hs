{-# LANGUAGE DeriveDataTypeable #-}

{- |
Copyright: (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier: MPL-2.0
Maintainer: Dmitrii Kovanikov <kovanikov@gmail.com>

GHC Plugin for non-empty lists
-}

module GhcPluginNonEmpty
    ( plugin

      -- * Internal typeclass for the plugin work
    , GhcPlugnNonEmptyClass (..)
    , cons
    ) where

import GHC.Driver.Plugins (CommandLineOption, Plugin (..), defaultPlugin, purePlugin)
import GHC.Hs.Expr (HsWrap (..), XXExprGhcTc (WrapExpr))
import GHC.Hs.Extension (GhcRn, GhcTc)
import GHC.Iface.Env (lookupOrig)
import GHC.Parser.Annotation (EpAnn (..), SrcSpanAnn' (..))
import GHC.Plugins (Name, mkVarOcc)
import GHC.Tc.Types (TcGblEnv (tcg_binds), TcM)
import GHC.Tc.Types.Evidence (HsWrapper, pprHsWrapper)
import GHC.Tc.Utils.Monad (getTopEnv)
import GHC.Types.SrcLoc (GenLocated (L), SrcSpan (..), UnhelpfulSpanReason (..))
import GHC.Types.TyThing (MonadThings (lookupId))
import GHC.Types.Var (Id)
import GHC.Unit.Finder (FindResult (..), findImportedModule)
import GHC.Unit.Module.ModSummary (ModSummary)
import GHC.Utils.Outputable (defaultSDocContext, renderWithContext, sdocPrintTypecheckerElaboration,
                             text)
import Language.Haskell.Syntax.Decls (HsGroup)
import Language.Haskell.Syntax.Expr (HsExpr (..), LHsExpr)
import Language.Haskell.Syntax.Extension (NoExtField (..))

import Control.Monad.IO.Class (MonadIO (..))
import Data.Generics.Aliases (mkM, mkT)
import Data.Generics.Schemes (everywhere, everywhereM)
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))

import qualified GHC


plugin :: Plugin
plugin = defaultPlugin
    { pluginRecompile = purePlugin

    -- 1. Identify list constructors and make them uniform
    , renamedResultAction = wrapLists

    -- 2. Replace list literals with NonEmpty list constructors
    , typeCheckResultAction = replaceNonEmpty
    }

wrapLists
    :: [CommandLineOption]
    -> TcGblEnv
    -> HsGroup GhcRn
    -> TcM (TcGblEnv, HsGroup GhcRn)
wrapLists _options tcGblEnv hsGroup = do
    hscEnv <- getTopEnv

    Found _ ghcPluginNonEmptyModule <- liftIO $ findImportedModule
        hscEnv
        (GHC.mkModuleName "GhcPluginNonEmpty")
        Nothing

    ghcPluginNonEmptyFromListName <- lookupOrig
        ghcPluginNonEmptyModule
        (mkVarOcc "_xxx_ghc_plugin_nonEmpty_fromList")

    nonEmptyCtor <- lookupOrig
        ghcPluginNonEmptyModule
        (mkVarOcc "cons")

    let newHsGroup = everywhere
            (mkT $ rewriteListLiterals ghcPluginNonEmptyFromListName nonEmptyCtor)
            hsGroup

    pure (tcGblEnv, newHsGroup)

rewriteListLiterals
    :: Name
    -> Name
    -> LHsExpr GhcRn
    -> LHsExpr GhcRn
rewriteListLiterals ghcPluginNonEmptyFromListName nonEmptyCtor = \case
    -- rewrite only explicit list literals
    l@(L _ ExplicitList{}) ->
        -- this becomes:
        -- _xxx_ghc_plugin_nonEmpty_fromList cons [3, 1, 2]
        app (app (var ghcPluginNonEmptyFromListName) (var nonEmptyCtor)) l

    -- don't touch other expresions
    expr ->
        expr

mkSpan :: a -> GenLocated (SrcSpanAnn' (EpAnn ann)) a
mkSpan = L $ SrcSpanAnn EpAnnNotUsed $ UnhelpfulSpan UnhelpfulGenerated

app :: LHsExpr GhcRn -> LHsExpr GhcRn -> LHsExpr GhcRn
app l r = mkSpan $ HsApp EpAnnNotUsed l r

var :: Name -> LHsExpr GhcRn
var name = mkSpan $ HsVar NoExtField $ mkSpan name

replaceNonEmpty
    :: [CommandLineOption]
    -> ModSummary
    -> TcGblEnv
    -> TcM TcGblEnv
replaceNonEmpty _options _modSummary tcGblEnv = do
    hscEnv <- getTopEnv

    Found _ ghcPluginNonEmptyModule <- liftIO $ findImportedModule
        hscEnv
        (GHC.mkModuleName "GhcPluginNonEmpty")
        Nothing

    ghcPluginNonEmptyFromListName <- lookupOrig
        ghcPluginNonEmptyModule
        (mkVarOcc "_xxx_ghc_plugin_nonEmpty_fromList")

    ghcPluginNonEmptyFromListId <- lookupId ghcPluginNonEmptyFromListName

    newTcgBinds <- everywhereM
            (mkM $ rewriteToNonEmpty ghcPluginNonEmptyFromListId)
            (tcg_binds tcGblEnv)

    pure tcGblEnv{ tcg_binds = newTcgBinds }

rewriteToNonEmpty
    :: Id
    -> LHsExpr GhcTc
    -> TcM (LHsExpr GhcTc)
rewriteToNonEmpty ghcPluginNonEmptyFromListId = \case
    L
        _
        (HsApp
            _
            (L
                _
                (HsApp
                    _
                    (L
                        _
                        (XExpr
                            (WrapExpr
                                (HsWrap
                                    varType
                                    (HsVar _ (L _ varName))
                                )
                            )
                        )
                    )
                    nonEmptyCtor
                )
            )
            r@(L
                _
                (ExplicitList listType items)
            )
        )
      | varName == ghcPluginNonEmptyFromListId ->
        if isNonEmptyWrapper varType
        -- transform non-empty lists
        then case items of
            -- if the list is empty, we just remove our wrapper and let GHC deal with it
            [] -> pure r

            -- otherwise, we use ctor to create NonEmpty
            x : xs -> pure $ mkSpan $ HsApp EpAnnNotUsed
                (mkSpan $ HsApp EpAnnNotUsed nonEmptyCtor x)
                (mkSpan $ ExplicitList listType xs)

        -- remove the wrapper for ordinary lists
        else pure r

    expr -> pure expr

{- | This function uses a dirty hack to check if the inferred type for
'__xxx_ghc_plugin_nonEmpty_fromList' is for 'NonEmpty'.
-}
isNonEmptyWrapper :: HsWrapper -> Bool
isNonEmptyWrapper hsWrapper = "@NonEmpty" `isInfixOf` strWrapper
  where
    -- HsWrapper pretty-printed as 'String'
    strWrapper :: String
    strWrapper = renderWithContext
        defaultSDocContext { sdocPrintTypecheckerElaboration = True }
        $ pprHsWrapper hsWrapper (\_ -> text "wtf?")

--------------------------
-- List wrapper
--------------------------

class GhcPlugnNonEmptyClass listOf where
    _xxx_ghc_plugin_nonEmpty_fromList
        :: (a -> [a] -> NonEmpty a)
        -- ^ Typechecked non-empty constructor
        -> [a]
        -- ^ List literal we're going to rewrite
        -> listOf a
        -- ^ Resulting list

instance GhcPlugnNonEmptyClass [] where
    _xxx_ghc_plugin_nonEmpty_fromList :: (a -> [a] -> NonEmpty a) -> [a] -> [a]
    _xxx_ghc_plugin_nonEmpty_fromList _ l = l
    {-# INLINE _xxx_ghc_plugin_nonEmpty_fromList #-}

instance GhcPlugnNonEmptyClass NonEmpty where
    _xxx_ghc_plugin_nonEmpty_fromList :: (a -> [a] -> NonEmpty a) -> [a] -> NonEmpty a
    _xxx_ghc_plugin_nonEmpty_fromList = error $ unlines
        [ "Panic! At The 'ghc-plugin-non-empty'"
        , "    Remained usage of: _xxx_ghc_plugin_nonEmpty_fromList :: [a] -> NonEmpty a"
        , ""
        , "If you see this error, please open an issue in the plugin with your code example:"
        , ""
        , "    * https://github.com/chshersh/ghc-plugin-non-empty/issues/new"
        ]
    {-# NOINLINE _xxx_ghc_plugin_nonEmpty_fromList #-}

-- | Constructor for 'NonEmpty'
cons :: a -> [a] -> NonEmpty a
cons = (:|)
