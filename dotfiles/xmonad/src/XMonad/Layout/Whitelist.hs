{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, TupleSections #-}
{-|
Module      : XMonad.Layout.Whitelist
Description : 'LayoutModifier' that will filter 'Window's
Copyright   : (c) Cj.bc-sd a.k.a Cj-bc
License     : BSD3
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental
Portability : non-portable

A 'LayoutModifier' that will hide some 'Window's based on 'Query'.

-}
module XMonad.Layout.Whitelist where

import Graphics.X11.Xlib
import Control.Monad ( foldM )
import XMonad (modify, gets)
import XMonad.Core
    ( runQuery, LayoutClass(runLayout), Query, X, XState(windowset), Message
    , fromMessage)
import XMonad.Layout.LayoutModifier
    ( LayoutModifier(modifyLayoutWithUpdate, pureMess), ModifiedLayout(..) )
import XMonad.StackSet
    ( Stack(focus, up, down), Workspace(stack), delete
    , Screen(workspace), StackSet(current), filter)
import qualified XMonad.StackSet as S

instance Show a => Show (Query a)
instance Read a => Read (Query a)

data Whitelist a = Whitelist { query :: [Query Bool] -- ^ list of 'Query'
                             , hidden :: [Window]    -- ^ 'Window's that are filtered and hidden by 'Whitelist'
                             , active   :: Bool -- ^ 'True' if whitelist functionality is active
                             } deriving (Show, Read)

-- | Messages to activate/deactivate whitelist functionality
data WhitelistMsg = ToggleWhitelist
                  | ActivateWhitelist
                  | DeactivateWhitelist

instance Message WhitelistMsg

-- | Builds 'Whitelist' Layout modifier.
whitelist :: [Query Bool] -> l a -> ModifiedLayout Whitelist l a
whitelist qs = ModifiedLayout (Whitelist qs [] True)

instance LayoutModifier Whitelist Window where
  modifyLayoutWithUpdate (Whitelist qs hidden False) w r = do
    (,Nothing) <$> runLayout (w<>hidden) r
  modifyLayoutWithUpdate (Whitelist qs hidden True) w r =
    case stack w of
      Nothing -> (,Nothing) <$> runLayout w r
      Just stack' -> do
        -- Filter all 'Window's by 'Query'
        windows' <- matchQueries qs (focus stack':up stack' ++ down stack') :: X (MatchResult Window)

        -- Remove filtered 'Window's from 'WindowSet' to hide them
        windows (\w -> foldl (flip delete) w $ didn't windows')

        s' <- gets $ stack.workspace.current.windowset
        -- Be sure to store 'didn\'t' items in Layout so that they can be retrieved later
        underlyingResult <- runLayout (w { stack = s'}) r
        return (underlyingResult, Just (Whitelist qs (hidden <> didn't windows') True))

  pureMess w@(Whitelist qs hidden isActive) msg =
    case fromMessage msg of
      Nothing                  -> Nothing
      Just ToggleWhitelist     -> Just $ w {active = not isActive }
      Just ActivateWhitelist   -> Just $ w {active = True }
      Just DeactivateWhitelist -> Just $ w {active = False }


-- | Represents result of running 'Query' on Windows.
data MatchResult a = MatchResult { matched :: [a] -- ^ Items that matched
                                 , didn't  :: [a] -- ^ Items that didn't matched
                                 }

-- | Smart constructor 
matchResult a True = MatchResult [a] [] 
matchResult a False = MatchResult [] [a] 

-- | Return 'True' if there are at least one 'a' that was 'matched'
haveMatched :: MatchResult a -> Bool
haveMatched (MatchResult [] _) = False
haveMatched mathced            = True
  
instance Functor MatchResult where
  fmap f (MatchResult m d) = MatchResult (fmap f m) (fmap f d)

instance Semigroup (MatchResult a) where
  (MatchResult m d) <> (MatchResult m' d') = MatchResult (m <>  m') (d <> d')

instance Monoid (MatchResult a) where
  mempty = MatchResult mempty mempty

-- | Split given list of 'Window's into two groups by applying 'Query'
--
-- > matchQuery (title =? "test-title") [] == (return $ MatchResult [] [])
matchQuery :: Query Bool -> [Window] -> X (MatchResult Window)
matchQuery q = foldM (\r w -> mappend r . matchResult w <$> runQuery q w) mempty

-- roam:2021-08-25

-- | Run all 'Query'es given, and returns one 'MatchReslt'
matchQueries :: [Query Bool] -> [Window] -> X (MatchResult Window)
matchQueries qs ws = foldM (\results q -> mappend results <$> matchQuery q ws) mempty qs
