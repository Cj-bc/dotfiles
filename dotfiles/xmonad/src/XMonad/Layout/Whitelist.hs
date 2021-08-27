{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}
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

import Data.Bool (bool)
import Data.Maybe (isJust, catMaybes, listToMaybe, isNothing, fromMaybe)
import Data.List (find)
import Graphics.X11.Xlib
import Control.Monad ( foldM )
import XMonad.Core
import XMonad.Layout.LayoutModifier
import XMonad.StackSet

instance Show a => Show (Query a)
instance Read a => Read (Query a)

data Whitelist a = Whitelist {
  query :: [Query Bool] -- ^ list of 'Query'
  } deriving (Show, Read)

whitelist :: [Query Bool] -> l a -> ModifiedLayout Whitelist l a
whitelist qs = ModifiedLayout (Whitelist qs)

instance LayoutModifier Whitelist Window where
  modifyLayout (Whitelist qs) w r = do
    case stack w of
      Nothing -> runLayout w r
      Just stack -> do
        -- Filter all 'Window's by 'Query'
        focused' <- join . listToMaybe <$> mapM (`matchQueryOneWindow` focus stack) qs :: X (Maybe Window)
        up' <- matchQueries (up stack) qs
        down' <- matchQueries (down stack) qs

        -- Reconstruct 'Stack' from filtered 'Window's
        --
        -- Notice that 'focused' 'Window' could be filtered
        let stack' = case focused' of
                       Just w' -> Just $ Stack w' up' down'
                       Nothing -> case up' of
                                    (w':ws) -> Just $ Stack w' ws down'
                                    [] -> case down' of
                                      (w':ws) -> Just $ Stack w' up' ws
                                      [] -> Nothing
  
        runLayout (w {stack = stack'}) r

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x:_) = Just x


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
