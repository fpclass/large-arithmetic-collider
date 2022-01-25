--------------------------------------------------------------------------------
-- Functional Programming - Large Arithmetic Collider Project
--------------------------------------------------------------------------------
-- Copyright (c) 2022 Michael B. Gale (michael@fpclass.online)
--
-- This source code is subject to the terms and conditions found in the LICENSE
-- file in the root directory of this source tree.
--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module contains types to represent levels and campaigns along with
-- functions to load/save levels and campaigns.
module Level (
    Level(..),
    loadLevel,
    saveLevel,
    Campaign(..),
    listCampaigns,
    loadLevels
) where

--------------------------------------------------------------------------------

import Control.Monad (forM, mzero)

import Data.Aeson
import Data.Function
import Data.HashMap.Strict as HM (lookup)
import Data.List (sortBy)

import System.Directory
import System.FilePath

import Game

--------------------------------------------------------------------------------
-- Orphan instances to avoid cluttering Game.hs with Aeson instances

instance ToJSON Action where
    toJSON (Add n) = object [ "action"  .= ("add" :: String)
                            , "operand" .= n
                            ]
    toJSON (Sub n) = object [ "action"  .= ("sub" :: String)
                            , "operand" .= n
                            ]

instance FromJSON Action where
    parseJSON = withObject "Action" $ \obj ->
        case HM.lookup "action" obj of
            Just (String "add") -> Add <$> obj .: "operand"
            Just (String "sub") -> Sub <$> obj .: "operand"
            _ -> mzero

instance ToJSON Cell where
    toJSON (MkCell _ a) = toJSON a

instance FromJSON Cell where
    parseJSON v = MkCell False <$> parseJSON v

instance ToJSON Row where
    toJSON (MkRow t cs) =
        object [ "target" .= t
               , "cells"  .= cs
               ]

instance FromJSON Row where
    parseJSON = withObject "Row" $ \obj ->
        MkRow <$> obj .: "target" <*> obj .: "cells"

instance ToJSON Grid where
    toJSON (MkGrid cts rows) =
        object [ "columns" .= cts
               , "rows"    .= rows
               ]

instance FromJSON Grid where
    parseJSON = withObject "Grid" $ \obj ->
        MkGrid <$> obj .: "columns" <*> obj .: "rows"

--------------------------------------------------------------------------------

-- | Represents a level consisting of a par number of rotations and a grid.
data Level = MkLevel {
    -- | The par number of rotations required to solve this level.
    levelPar  :: Int,
    -- | The grid that makes up the level.
    levelGrid :: Grid
} deriving (Eq, Show)

instance ToJSON Level where
    toJSON (MkLevel par grid) =
        object [ "par"  .= par
               , "grid" .= grid
               ]

instance FromJSON Level where
    parseJSON = withObject "Level" $ \obj ->
        MkLevel <$> obj .: "par" <*> obj .: "grid"

-- | `saveLevel` @filepath level@ saves @level@ to @filepath@.
saveLevel :: FilePath -> Level -> IO ()
saveLevel = encodeFile

-- | `loadLevel` @filepath@ tries to load a level from @filepath@.
loadLevel :: FilePath -> IO (Either String Level)
loadLevel = eitherDecodeFileStrict

--------------------------------------------------------------------------------

-- | Represents a campaign (a collection of levels).
data Campaign = MkCampaign {
    -- | The title of the campaign.
    campaignTitle :: String,
    -- | The description of the campaign.
    campaignDesc  :: String,
    -- | The path to the levels folder for this campaign.
    campaignPath  :: FilePath
} deriving (Eq, Show)

instance FromJSON Campaign where
    parseJSON = withObject "Campaign" $ \obj ->
        MkCampaign <$> obj .: "title"
                   <*> obj .: "description"
                   <*> obj .: "path"

-- | `listCampaigns` is a computation which loads the list of campaigns.
listCampaigns :: IO (Maybe [Campaign])
listCampaigns = decodeFileStrict "levels/campaigns.json"

-- | `sortP` @fp1 fp2@ compares @fp1@ and @fp2@ numerically based on their
-- filenames. This assumes that the base filenames of @fp1@ and @fp2@ are
-- numbers.
sortP :: FilePath -> FilePath -> Ordering
sortP = compare `on` ((read :: FilePath -> Int) . takeBaseName)

-- | `loadLevels` @directory@ attempts to load all levels from @directory@.
loadLevels :: FilePath -> IO (Either String [Level])
loadLevels dir = do
    -- list all the files in the target directory
    fs <- listDirectory dir

    -- filter out any files that do not have a .level extension.
    let levels = filter ((==) ".level" . takeExtension) fs

    -- try to load each level in numeric order (by filename)
    ls <- forM (sortBy sortP levels) $ \fp -> loadLevel (dir </> fp)

    -- return the result of trying to load each level: if there was a
    -- failure somewhere, that failure will be returned. otherwise,
    -- all the levels will be returned.
    pure $ sequence ls

--------------------------------------------------------------------------------