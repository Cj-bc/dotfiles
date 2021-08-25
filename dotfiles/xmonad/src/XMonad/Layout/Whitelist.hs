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

import Control.Monad (join)
import Data.Bool (bool)
import Data.Maybe (isJust, catMaybes, listToMaybe, isNothing, fromMaybe)
import Data.List (find)
import Graphics.X11.Xlib
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

-- | Run all 'Query'ies and return all windows which matches any of
-- them.
matchQueries :: [Window] -> [Query Bool] -> X [Window]
matchQueries ws qs = catMaybes <$> mapM (`matchQuery` ws) qs


-- | Run 'Query' to find out one 'Window'
--
-- It will return 'Nothing' when no 'Window' satisfies that 'Query'
--
-- > matchQuery ((== "test-title") <$> title) []
matchQuery :: Query Bool -> [Window] -> X (Maybe Window)
matchQuery q ws = join . find isJust <$> mapM (matchQueryOneWindow q) ws

matchQueryOneWindow :: Query Bool -> Window -> X (Maybe Window)
matchQueryOneWindow q w = f w <$> runQuery q w
  where
    f = bool Nothing . Just

-- roam:2021-08-25
