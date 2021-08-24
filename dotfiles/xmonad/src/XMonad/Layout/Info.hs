{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-- | Layout specialized for 'Info' 'Workspace'
module XMonad.Layout.Info where

import Graphics.X11.Xlib
import XMonad.Core
import XMonad.ManageHook
import XMonad.StackSet (focus, up, down)
import Data.Maybe (isJust, catMaybes)
import Data.List (find)
import Control.Monad (join)

data InfoWorkspace a = InfoWorkspace deriving (Show, Read)

instance LayoutClass InfoWorkspace Window where
  doLayout InfoWorkspace (Rectangle x y w h) s = do
    agendaWindow <- getWindowOf "*Org Agenda*"
    dzenBatteryWindow <- getWindowOf "dzen-battery"
    
    return (catMaybes [(,) <$> agendaWindow <*> pure orgAgenda
                      ,(,) <$> dzenBatteryWindow <*> pure dzenBattery
                      ], Nothing)
    where
      allWindows = focus s:up s ++ down s
      getWindowOf :: String -> X (Maybe Window)
      getWindowOf name = fmap (join . find isJust) . sequence . flip fmap allWindows $ \w -> flip runQuery w $ do
        n <- w`hasTitle`name
        if n then return (Just w) else return Nothing

      hasTitle :: Window -> String -> Query Bool
      hasTitle w name = (== name) <$> title

  
      orgAgenda = Rectangle (fromIntegral $ w`div`5 * 3) y (w`div`5 * 2) h
      dzenBattery = Rectangle x y (w`div`5) (h`div`4)
