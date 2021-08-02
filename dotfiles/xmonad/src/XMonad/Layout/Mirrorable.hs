{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-|
Module : XMonad.Layout.Mirrorable
Description : Togglable 'Mirror' Layout
Copyright : (c) Cj.bc_sd a.k.a Cj-bc, 2021
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental

内包するレイアウトに対して, 'ToggleMirror' 'Message' で
'Mirror' の効果を有効化・無効化します。
'Mirror' で囲むことも考えましたが出来なかったため, そのまま
参考にして実装しています。
-}
module XMonad.Layout.Mirrorable where


import XMonad
import XMonad.Layout
import XMonad.StackSet (Workspace(..))
import XMonad.Layout.LayoutModifier
import Control.Lens ((&), (%~), each, _2, _1)


-- | 'LayoutModifier' that should be
newtype Mirrorable a = Mirrorable Bool -- ^ is it mirrored?
                     deriving (Show, Read)

-- | Message that is required for this Layout
data MirroableMsg = ToggleMirror

instance Message MirroableMsg

-- | Smart constructor for 'Mirrorable'
mirrorable :: l a -> ModifiedLayout Mirrorable l a
mirrorable = ModifiedLayout (Mirrorable False)

instance LayoutModifier Mirrorable a where
  modifyLayout (Mirrorable isMirrored) ws rect = (_1.each._2%~modifier) <$> runLayout ws (modifier rect)
    where
      modifier = if isMirrored then mirrorRect else id

  pureMess (Mirrorable b) m = do
    msg <- fromMessage m
    case msg of
      ToggleMirror -> Just . Mirrorable $ not b
      _ -> Nothing
