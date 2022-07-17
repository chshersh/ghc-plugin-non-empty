{-# LANGUAGE DeriveDataTypeable #-}

{- |
Module                  : Iris
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Stable
Portability             : Portable

GHC Plugin for rewriting list literals of non-empty list to 'NonEmpty'
type. Enable the plugin in a module where you want to use it like on
the example below:

@
\{\-\# __OPTIONS_GHC__ -fplugin=GhcPluginNonEmpty \#\-\}

__import__ "Data.List.NonEmpty" ('NonEmpty')

portsToListen :: 'NonEmpty' 'Int'
portsToListen = [8000, 8080, 8081]
@

You can also enable the plugin globally in your entire project from
the @.cabal@ file:

@
library
   ...
   ghc-options: -fplugin=GhcPluginNonEmpty
@

It guarantees that only non-empty lists will be automatically
rewritten. Otherwise, if you use an empty list:

@
portsToListen :: 'NonEmpty' 'Int'
portsToListen = []
@

You'll see ordinary compiler error:

@
src\/Path\/To\/My\/Module:34:17: error:
    • Couldn't match expected type: NonEmpty Int
                  with actual type: [a0]
    • In the expression: []
      In an equation for ‘portsToListen’: portsToListen = []
   |
34 | portsToListen = []
   |                 ^^
@

@since 0.0.0.0
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



{- | Main compiler plugin. Use the following GHC option to enable it:

@
\{\-\# OPTIONS_GHC -fplugin=GhcPluginNonEmpty \#\-\}
@

Implemented in two steps:

1. Rewrites @[3, 1, 2]@ to @'_xxx_ghc_plugin_nonEmpty_fromList' 'cons' [3, 1, 2]@
2. Find applications of '_xxx_ghc_plugin_nonEmpty_fromList' and rewrites them to
   @'cons' 3 [1, 2]@

@since 0.0.0.0
-}
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

{- | ⚠️ __WARNING! Don't use this typeclass!__ ⚠️

This is an internal typeclass for the plugin to work correctly but it
must be imported from this module. Don't use methods of this typeclass
in your code as it may result in incorrect compilation of your code.

@since 0.0.0.0
-}
class GhcPlugnNonEmptyClass listOf where
    -- | @since 0.0.0.0
    _xxx_ghc_plugin_nonEmpty_fromList
        :: (a -> [a] -> NonEmpty a)
        -- ^ Typechecked non-empty constructor
        -> [a]
        -- ^ List literal we're going to rewrite
        -> listOf a
        -- ^ Resulting list

-- | @since 0.0.0.0
instance GhcPlugnNonEmptyClass [] where
    _xxx_ghc_plugin_nonEmpty_fromList :: (a -> [a] -> NonEmpty a) -> [a] -> [a]
    _xxx_ghc_plugin_nonEmpty_fromList _ l = l
    {-# INLINE _xxx_ghc_plugin_nonEmpty_fromList #-}

-- | @since 0.0.0.0
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

{- | Constructor for 'NonEmpty'. Named alias to ':|'.

@since 0.0.0.0
-}
cons :: a -> [a] -> NonEmpty a
cons = (:|)
