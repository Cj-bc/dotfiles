import XMonad
import XMonad.Util.EZConfig
import XMonad.Hooks.DynamicLog
import XMonad.Actions.SpawnOn (spawnOn, manageSpawn)
import XMonad.Layout.PerWorkspace
import XMonad.Layout (Full)
import Data.Text (Text)
-- import XMonad.Util.EZConfig.Reexport (readKeymap)
import XMonad.Layout.Gaps (gaps, Direction2D(..), GapMessage(ToggleGaps))
import XMonad.Layout.Spacing

main = xmonad =<< xmobar cfg

cfg = def
    { terminal = "termite"
    , workspaces = my_workspaces
    , manageHook = my_manageHook
    , startupHook = my_startuphook
    , layoutHook = my_layoutHook
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

keybinds :: [(String, X ())]
keybinds =
    [("M-w p", spawn "LANG=C rofi -show run")
    ,("M-w v", spawn "LANG=C clipmenu && xsel -o")
    ,("M-S-/", spawn "dunstify \"This will be command for showing all keybinds, but currently I can't provide it\"")
    ,("M-C-3", spawn "import -window root ~/Picture/screenshots/$(date +%Y%m%d%H%M%S).png")
    ,("M-C-4", spawn "import ~/Picture/screenshots/$(date +%Y%m%d%H%M%S).png")
    ,("M-C-g", sequence (fmap sendMessage [(ModifyWindowBorderEnabled not), (ModifyScreenBorderEnabled not)]) >>
               sendMessage ToggleGaps >> pure ())
    ,("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume 0 -5%; pactl set-sink-mute 0 false")
    ,("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume 0 +5%; pactl set-sink-mute 0 false")
    ,("<XF86AudioMute>", spawn "pactl set-sink-mute 0 toggle")
    ,("M-<XF86AudioMute>", spawn "pactl set-source-mute 0 toggle")
    ]

my_manageHook :: ManageHook
my_manageHook = composeAll $ [
      manageSpawn
      -- className =? "Brave-browser" --> doShift (show Web)
    , className =? "Slack"             --> doShift (show Communication)
    , className =? "discord"           --> doShift (show Communication)
    , title     =? "Discord -- Brave"  --> doShift (show Communication)
    , className =? "Dunst"             --> doFloat
    , className =? "Conky"             --> doShift (show Info)
    ]

-- grep :: [Text] -> Sh Text
-- grep = run "grep"
-- 
-- ps :: [Text] -> Sh Text
-- ps = run "ps"
-- 
-- -- | True if given program is already running
-- isProgramRunning :: Text -> IO Bool
-- isProgramRunning programName = catch (shelly $ ps ["x"] -|- run "awk" ["{print $5}"] -|- grep ["^" ++ programName] -|- return True)
--                                      (const $ return False :: SomeException -> IO Bool)
-- 
my_startuphook = do
    -- This doesn't work...
    -- Consider using 'XMonad.Actions.TagWindows' instead.
    spawnOn (show . fromEnum $ Info) "conky"
    spawnOn (show . fromEnum $ Editor) "termite"
    spawnOn (show . fromEnum $ Web) "brave"
    spawnOn (show . fromEnum $ Communication) "slack"
    spawnOn (show . fromEnum $ Communication) "discord"
    return () >> checkKeymap cfg keybinds
    -- where
    --     spawnIfNotRunning :: WorkspaceNames -> Text -> X ()
    --     spawnIfNotRunning workspace program = do
    --         running <- liftIO $ isProgramRunning program
    --         when (not running) $ spawnOn (show $ fromEnum workspace) program

my_layoutHook = addSpacing $ onWorkspace (show Web) fullWithGap defaultLayout
    where
        defaultLayout = fullWithGap ||| Tall def def def ||| Mirror (Tall def def def)
        fullWithGap = gaps [(U, 20),(D, 20),(R, 20),(L, 20)] Full 
        addSpacing = spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True
