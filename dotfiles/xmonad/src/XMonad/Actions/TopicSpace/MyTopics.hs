{-# LANGUAGE FlexibleInstances #-}
module XMonad.Actions.TopicSpace.MyTopics
  ( MyTopic(..)
  , Blender(..)
  , Web(..)
  , Note(..)
  , Communication(..)
  , Project(Project)
  , Krita(..)
  , topics
  ) where

import XMonad
import XMonad.Actions.TopicSpace (TopicItem (TI))
import XMonad.Util.SpawnOnce (spawnOnce)

ghqRootPath = "~/Documents/ghq/"

topics :: [TopicItem]
topics = [mkTopicItem Blender
         , mkTopicItem Krita
         , mkTopicItem Web
         , mkTopicItem Note
         , mkTopicItem Communication
         , mkTopicItem (Project "dotfiles" (Ghq "github.com/Cj-bc/dotfiles"))
         , mkTopicItem (Project "blog" (Ghq "github.com/Cj-bc/blog"))
         ]

class MyTopic a where
  topicName :: a -> String
  topicDir :: a -> String
  topicAction :: a -> X () 
  
  mkTopicItem :: a -> TopicItem
  mkTopicItem a = TI (topicName a) (topicDir a) (topicAction a)


-- | Blender
data Blender = Blender

instance MyTopic Blender where
  topicName = const "blender"
  topicDir = const "~/Documents/blender"
  topicAction = const $ spawnOnce "blender"

-- | Web
data Web = Web

instance MyTopic Web where
  topicName = const "web"
  topicDir = const "~/"
  topicAction = const $ spawnOnce "~/.local/bin/rofi-qutem Cj-bc"

-- | Note
data Note = Note

instance MyTopic Note where
  topicName = const "note"
  topicDir = const "~/Dropbox/roam"
  topicAction = const $ spawn "emacsclient -c"

-- | Communication
data Communication = Communication

instance MyTopic Communication where
  topicName = const "communication"
  topicDir  = const "~/"
  topicAction = const $ spawn "emacsclient -c"

-- | Project
data ProjectType = Raw String
                 | Ghq String
data Project = Project { _name :: String
                       , _pType :: ProjectType
                       }

instance MyTopic Project where
  topicName = _name
  topicDir project = case _pType project of
                       Raw s -> s
                       Ghq s -> ghqRootPath ++ s
  topicAction = const $ spawnOnce "LANG=ja_JP.UTF-8 ~/.local/bin/st"
                >> spawnOnce "emacsclient -c"

-- | Krita
data Krita = Krita

instance MyTopic Krita where
  topicName = const "krita"
  topicDir _ = "~/"
  topicAction = const $ spawnOnce "krita"
