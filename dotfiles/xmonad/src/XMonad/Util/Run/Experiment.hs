{-# LANGUAGE FlexibleContexts #-}
module XMonad.Util.Run.Experiment where
import Codec.Binary.UTF8.String ( encodeString )
import System.Posix.IO
    ( closeFd,
      createPipe,
      dupTo,
      fdToHandle,
      setFdOption,
      stdInput,
      FdOption(CloseOnExec) )
import System.Posix.Process ( executeFile )
import System.IO
    ( hSetBuffering, hPutStrLn, BufferMode(LineBuffering), Handle )
import XMonad
    ( xK_b,
      io,
      xfork,
      sendMessage,
      KeyMask,
      KeySym,
      Window,
      MonadIO,
      Layout,
      LayoutClass,
      XConfig(XConfig, layoutHook, logHook, keys, modMask) )
import Control.Monad ( liftM2 )
import qualified Data.Map as M ( singleton, union )
import XMonad.Layout.LayoutModifier ( ModifiedLayout )
import XMonad.Hooks.ManageDocks
    ( avoidStruts, docks, AvoidStruts, ToggleStruts(ToggleStruts) )
import XMonad.Hooks.DynamicLog
    ( dynamicLogWithPP, xmobarPP, PP(ppOutput) )
 
 

xmobar' :: LayoutClass l Window
       => XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
xmobar' conf = statusBar "xmobar" xmobarPP toggleStrutsKey conf

statusBar :: LayoutClass l Window
          => String    -- ^ the command line to launch the status bar
          -> PP        -- ^ the pretty printing options
          -> (XConfig Layout -> (KeyMask, KeySym))
                       -- ^ the desired key binding to toggle bar visibility
          -> XConfig l -- ^ the base config
          -> IO (XConfig (ModifiedLayout AvoidStruts l))
statusBar cmd pp k conf = do
    h <- spawnPipe cmd
    return $ docks $ conf
        { layoutHook = avoidStruts (layoutHook conf)
        , logHook = do
                        logHook conf
                        dynamicLogWithPP pp { ppOutput = hPutStrLn h }
        , keys       = liftM2 M.union keys' (keys conf)
        }
 where
    keys' = (`M.singleton` sendMessage ToggleStruts) . k

spawnPipe :: MonadIO m => String -> m Handle
spawnPipe x = io $ do
    (rd, wr) <- createPipe
    setFdOption wr CloseOnExec True
    h <- fdToHandle wr
    hSetBuffering h LineBuffering
    _ <- xfork $ do
          _ <- dupTo rd stdInput
          executeFile "/bin/bash" True ["-c", encodeString x] (Just [("LANG", "ja_JP.UTF-8")])
    closeFd rd
    return h

  

toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b)
