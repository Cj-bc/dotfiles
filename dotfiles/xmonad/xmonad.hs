import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Hooks.DynamicLog
import XMonad.Actions.SpawnOn (spawnOn, manageSpawn)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Layout.PerWorkspace
import XMonad.Layout (Full)
import Data.Text (Text)
import Data.Bits ((.|.))
import qualified Data.Map as M
import XMonad.Util.NamedScratchpad (NamedScratchpad(NS), customFloating, namedScratchpadManageHook, namedScratchpadAction)
-- import XMonad.Util.EZConfig.Reexport (readKeymap)
-- Looks and feel
import XMonad.Layout.Gaps (gaps, Direction2D(..), GapMessage(ToggleGaps))
import XMonad.Layout.Spacing
import XMonad.Layout.Half (Half(..), HalfMessage(..))
import XMonad.Layout.Mirrorable
import XMonad.Prompt.Pass
import XMonad.Prompt

main = xmonad =<< xmobar cfg

cfg = def
    { terminal = "termite"
    , workspaces = my_workspaces
    , manageHook = my_manageHook
    , startupHook = my_startuphook
    , layoutHook = my_layoutHook
    , mouseBindings = my_mouseBinds
    , modMask = mod4Mask
    }
    `removeKeysP` ["M-p"]
    `additionalKeysP` keybinds


data WorkspaceNames = Info | Editor | Web | Communication | Media deriving (Show, Eq, Ord, Enum)
my_workspaces = map show $ enumFrom Info

-- I want to create this function to show notification
-- Want to split this into other module, but I don't know how to read it along with xmonad
--
-- isMuted :: IO Bool
-- isMuted = "pactl list sinks | head -n 9 | tail -n 1 | tail --bytes 4"

my_xpconfig :: XPConfig
my_xpconfig = def { font = "xft:Cica:" }
  
keybinds :: [(String, X ())]
keybinds =
    [("M-w p", spawn "LANG=C rofi -show run")
    ,("M-w v", spawn "LANG=C clipmenu")
    ,("M-w q", spawn "LANG=C rofi -show power-manager")
    ,("M-<XF86PowerOff>", spawn "LANG=C rofi -show power-manager")
    ,("M-w f", sendMessage ChangeLR)
    ,("M-w S-1",  passPrompt my_xpconfig)
    ,("M-S-/", spawn "dunstify \"This will be command for showing all keybinds, but currently I can't provide it\"")
    ,("M-C-3", spawn "import -window root ~/Picture/screenshots/$(date +%Y%m%d%H%M%S).png")
    ,("M-C-4", spawn "import ~/Picture/screenshots/$(date +%Y%m%d%H%M%S).png")
    ,("M-C-g", sequence (fmap sendMessage [(ModifyWindowBorderEnabled not), (ModifyScreenBorderEnabled not)]) >>
               sendMessage ToggleGaps >> pure ())
    ,("M-f", namedScratchpadAction my_scratchpads "floating terminal")
    ,("<XF86AudioLowerVolume>",  spawn "pactl set-sink-volume 0 -5%; pactl set-sink-mute 0 false")
    ,("<XF86AudioRaiseVolume>",  spawn "pactl set-sink-volume 0 +5%; pactl set-sink-mute 0 false")
    ,("<XF86AudioMute>",         spawn "pactl set-sink-mute 0 toggle")
    ,("M-<XF86AudioMute>",       spawn "pactl set-source-mute 0 toggle")
    ,("<XF86MonBrightnessUp>",   spawn "brightnessctl set +1%")
    ,("<XF86MonBrightnessDown>", spawn "brightnessctl set 1%-")
    ]

my_manageHook :: ManageHook
my_manageHook = composeAll $ [
      manageSpawn
    , className =? "Brave-browser" --> doShift (show Web)
    , className =? "qutebrowser"   --> doShift (show Web)
    , className =? "Slack"             --> doShift (show Communication)
    , className =? "discord"           --> doShift (show Communication)
    , title     =? "Discord -- Brave"  --> doShift (show Communication)
    , className =? "Dunst"             --> doFloat
    , className =? "Conky"             --> doShift (show Info)
    , namedScratchpadManageHook my_scratchpads
    ]

my_startuphook = do
    -- This doesn't work...
    -- Consider using 'XMonad.Actions.TagWindows' instead.
    return () >> checkKeymap cfg keybinds
    spawnOn (show . fromEnum $ Info) "conky"
    spawnOnce "termite"
    spawnOnce "qutebrowser -B ~/.local/share/qutem/profiles/Cj-bc"
    spawnOnce "slack"
    spawnOnce "discord"

my_layoutHook = addSpacing $ onWorkspace (show Web) fullWithGap defaultLayout
    where
        defaultLayout = fullWithGap
                        ||| (mirrorable $ Tall def def def)
                        ||| (mirrorable . addGapOnewindow $ Half def)
        addGapOnewindow = gaps [(U, 20),(D, 20),(R, 20),(L, 20)]
        fullWithGap = addGapOnewindow Full
        addSpacing = spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True

my_scratchpads = [
    NS "floating terminal" "termite -t 'floating-terminal'" (title =? "floating-terminal")
    (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
    ]

-- | Mouse bindings: Extending default
my_mouseBinds :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
my_mouseBinds (XConfig {XMonad.modMask = modMask}) = M.fromList
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
