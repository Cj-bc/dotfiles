{-# LANGUAGE Rank2Types, KindSignatures, FlexibleContexts #-}
import XMonad
import XMonad.Operations (withFocused)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Actions.SpawnOn (spawnOn, manageSpawn)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Layout.PerWorkspace
import XMonad.Layout (Full)
import Data.Bool (bool)
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Bits ((.|.))
import Data.List (isPrefixOf)
import Data.Monoid (Endo(..))
import qualified Data.Map as M
import XMonad.Util.NamedScratchpad (NamedScratchpad(NS), customFloating, namedScratchpadManageHook, namedScratchpadAction)
-- import XMonad.Util.EZConfig.Reexport (readKeymap)
-- Looks and feel
import XMonad.Layout.Gaps (gaps, Direction2D(..), GapMessage(ToggleGaps))
import XMonad.Layout.Spacing
import XMonad.Layout.Half (Half(..), HalfMessage(..))
import XMonad.Layout.TwoPane
import XMonad.Layout.Mirrorable
import XMonad.Layout.BoringWindows
import XMonad.Layout.Info
import XMonad.Actions.Minimize
import XMonad.Actions.NoBorders
import XMonad.Layout.Minimize
import XMonad.Prompt.Pass
import XMonad.Prompt
import XMonad.Prompt.XMonad
import XMonad.Prompt.FuzzyMatch

import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Util.Run.Experiment (xmobar')
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Eww (ewwToggle, EwwVisibility)

-- Need to specify LANG in order to show UTF-8 properly
mySB = statusBarProp "LANG=ja_JP.UTF-8 xmobar" (pure $ xmobarPP {ppCurrent = bool "綠" "" . (==) "NSP"
                                                                , ppHidden = bool "祿" "" . (==) "NSP"
                                                                , ppHiddenNoWindows = bool "祿" "" . (==) "NSP"
                                                                })
main = xmonad $ withEasySB mySB defToggleStrutsKey cfg

cfg = def
    { terminal = "LANG=ja_JP.UTF-8 ~/.local/bin/st"
    , workspaces = myWorkspaces
    , manageHook = myManageHook
    , startupHook = myStartuphook
    , layoutHook = myLayoutHook
    , mouseBindings = myMouseBinds
    , modMask = mod4Mask
    }
    `removeKeysP` ["M-p"]
    `additionalKeysP` keybinds


data WorkspaceNames = Info | Editor | Web | Communication | Media deriving (Show, Eq, Ord, Enum)
myWorkspaces = map show $ enumFrom Info

-- I want to create this function to show notification
-- Want to split this into other module, but I don't know how to read it along with xmonad
--
-- isMuted :: IO Bool
-- isMuted = "pactl list sinks | head -n 9 | tail -n 1 | tail --bytes 4"

myXpconfig :: XPConfig
myXpconfig = def { font = "xft:Cica:"
                 , height = 32
                 , searchPredicate = fuzzyMatch
                 , sorter = fuzzySort
                 }

keybinds :: [(String, X ())]
keybinds =
    [("M-w p", spawn "LANG=C rofi -show run -config ~/.config/rofi/mia-chan-alike!.rasi")
    ,("M-w v", spawn "LANG=C clipmenu")
    ,("M-w q", spawn "LANG=C rofi -show power-manager")
    ,("M-<XF86PowerOff>", spawn "LANG=C rofi -show power-manager")
    ,("M-w f", sendMessage ChangeLR)
    ,("M-w h", withFocused minimizeWindow)
    ,("M-w M-h", withLastMinimized maximizeWindow)
    ,("M-w S-1",  passPrompt myXpconfig)
    ,("M-S-e", spawn "emacsclient -c")
    ,("M-S-s", ewwToggle)
    ,("M-S-/", spawn "dunstify \"This will be command for showing all keybinds, but currently I can't provide it\"")
    ,("M-C-3", spawn "~/.local/bin/screenshot -r ~/Picture/screenshots/$(date +%Y%m%d%H%M%S).png")
    ,("M-C-4", spawn "~/.local/bin/screenshot ~/Picture/screenshots/$(date +%Y%m%d%H%M%S).png")
    ,("M-C-5", spawn "~/.local/bin/screenshot -c")
    ,("M-C-g", (mapM sendMessage [ModifyWindowBorderEnabled not, ModifyScreenBorderEnabled not]) >>
               sendMessage ToggleGaps >> pure ())
    ,("M-f", namedScratchpadAction myScratchpads "floating terminal")
    ,("M-S-;", xmonadPromptC myCustomCommandList myXpconfig)
    ,("<XF86AudioLowerVolume>",  spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%; pactl set-sink-mute @DEFAULT_SINK@ false")
    ,("<XF86AudioRaiseVolume>",  spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%; pactl set-sink-mute @DEFAULT_SINK@ false")
    ,("<XF86AudioMute>",         spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    ,("<XF86MonBrightnessUp>",   spawn "brightnessctl set +1%")
    ,("<XF86MonBrightnessDown>", spawn "brightnessctl set 1%-")
    ]

myManageHook :: ManageHook
myManageHook = composeAll [
      manageSpawn
    , className =? "Brave-browser" --> doShift (show Web)
    , className =? "qutebrowser"   --> doShift (show Web)
    , className =? "Slack"             --> doShift (show Communication)
    , className =? "discord"           --> doShift (show Communication)
    , title     =? "Discord -- Brave"  --> doShift (show Communication)
    , className =? "Dunst"             --> doFloat
    , className =? "pinentry-qt" --> doFloat
    , className =? "Emacs" <&&> title =? "org-agenda-fixed" --> doShift (show Info)
    , className =? "Emacs" <&&> title =? "qutebrowser.edit-url"
      --> (ask >>= placeAt (W.RationalRect (6%10) (4%10) (4%10) (1%10))) <+> doFloat
    , namedScratchpadManageHook myScratchpads
    , className =? "jetbrains-studio" --> doFloat
    , className =? "zoom "  <&&> title =? "Chat" --> doFloat
    , fmap (isPrefixOf "ol-mpv mpv -- ") title --> doFloat
    ]

placeAt :: W.RationalRect -> Window -> ManageHook
placeAt r w = 
  return . Endo $ \currentWindowSet ->
        currentWindowSet { W.floating = M.update (const $ Just r) w (W.floating currentWindowSet) }

myStartuphook = do
    -- This doesn't work...
    -- Consider using 'XMonad.Actions.TagWindows' instead.
    checkKeymap cfg keybinds
    spawnOnce "LANG=ja_JP.UTF-8 ~/.local/bin/st"
    spawnOnce "~/.local/bin/rofi-qutem Cj-bc"
    spawnOnce "slack"
    spawnOnce "discord"
    spawnOnce "picom"
    spawn "~/.fehbg"

    spawnOnce "~/.local/bin/eww daemon"
    spawnOnce "~/.local/bin/eww open status"
    XS.put (def :: EwwVisibility)
myLayoutHook = minimize . boringWindows . addSpacing
               . onWorkspace (show Web) fullWithGap
               $ defaultLayout
    where
        defaultLayout = fullWithGap
                        ||| (mirrorable $ Tall def def def)
                        ||| (mirrorable . addGapOnewindow $ Half def)
                        ||| (mirrorable $ TwoPane (1/100) (1/2))
        addGapOnewindow = gaps [(U, 20),(D, 20),(R, 20),(L, 20)]
        fullWithGap = addGapOnewindow Full
        addSpacing = spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True

myScratchpads = [
    NS "floating terminal" "LANG=ja_JP.UTF-8 ~/.local/bin/st -t 'floating-terminal'" (title =? "floating-terminal")
    (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
    ]

-- | Mouse bindings: Extending default
myMouseBinds :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBinds (XConfig {XMonad.modMask = modMask}) = M.fromList
    -- mod-button1 %! Set the window to floating mode and move by dragging
    [ ((modMask, button1), \w -> focus w >> mouseMoveWindow w
                                          >> windows W.shiftMaster)
    -- mod-button2 %! Raise the window to the top of the stack
    , ((modMask, button2), windows . (W.shiftMaster .) . W.focusWindow)
    -- mod-button3 %! Set the window to floating mode and resize by dragging
    , ((modMask .|. shiftMask, button1), resizing)
    , ((modMask, button3), resizing)
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
    where
        resizing = \w -> focus w >> mouseResizeWindow w
                                 >> windows W.shiftMaster

-- | List of commands for 'xmonadPrompt'
myCustomCommandList :: [(String, X ())]
myCustomCommandList = [("toggle info", sendMessage InfoWorkspaceToggleShowAll)
                      ,("rebuild", spawn "if type xmonad; then xmonad --recompile && xmonad --restart; fi")
                      ,("toggle mirror", sendMessage ToggleMirror)
                      ,("toggle border visibility", withFocused toggleBorder)
                      ,("clipmenu", spawn "LANG=C clipmenu")
                      ]
