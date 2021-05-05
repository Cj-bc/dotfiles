{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses #-}
{-|
Module      : XMonad.Layout.Half
Description : Half size layout
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2021
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental

This only eats Left/Right half of the screen and show only focused window.
You can toggle which side to use by 'ChangeSide' message

画面の右・左半分のみにウィンドウを表示します。
'ChangeSide' メッセージで左右を反転することができます。

-}
module XMonad.Layout.Half where

import XMonad.Core
import Data.Default (Default(..))
import XMonad.StackSet (focus)
import Graphics.X11.Xlib (Rectangle(..))

data LR = L | R deriving (Show, Read, Eq)

instance Default LR where
    def = L

-- | Show Window in half side of screen
--
--
data Half a = Half LR deriving (Show, Read)


instance LayoutClass Half a where
    pureLayout (Half lr) r s | lr == L = [(focus s, halfL r)]
                             | lr == R = [(focus s, halfR r)]
        where
            halfL r = Rectangle (rect_x r) (rect_y r) halfW (rect_height r)
            halfR r = Rectangle (rect_x r + fromIntegral halfW)
                                (rect_y r)
                                halfW (rect_width r)
            halfW = rect_width r`div`2
    description (Half direct) = "Half " ++ show direct
    pureMessage (Half lr) m | fromMessage m == (Just ChangeSide) = Just $ Half (flipLR lr)
                            | otherwise                          = Nothing
        where
            flipLR L = R
            flipLR R = L

data HalfMessage = ChangeSide deriving (Eq, Typeable)

instance Message HalfMessage


