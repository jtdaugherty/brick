{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Vty

import Data.Array

import Control.Applicative
import Control.Monad

import System.Random

import Brick.Main
import Brick.Core
import Brick.AttrMap
import Brick.Widgets.Core
import Brick.Widgets.Border
import Brick.Widgets.Center

data Player = Player
    { playerCoord :: Coord
    } deriving (Show,Eq)

data World = World
    { player :: Player
    , level :: Level
    }
    deriving (Show,Eq)

data Level = Level
    { levelStart :: Coord
    , levelEnd :: Coord
    , levelGeo :: Geo
    -- building the geo image is expensive. Cache it. Though VTY should go
    -- through greater lengths to avoid the need to cache images.
    , levelGeoImage :: Image
    }
    deriving (Show,Eq)

data LevelPiece
    = EmptySpace
    | Rock
    deriving (Show, Eq)

type Geo = Array Coord LevelPiece
type Coord = (Int, Int)

main :: IO ()
main = do
    level0 <- mkLevel 1
    let world0 = World (Player (levelStart level0)) level0
        app = App { appDraw = updateDisplay
                  , appHandleEvent = processEvent
                  , appStartEvent = return
                  , appChooseCursor = neverShowCursor
                  , appMakeVtyEvent = id
                  , appAttrMap = const $ attrMap defAttr []
                  }
    void $ defaultMain app world0

-- |Generate a level randomly using the specified difficulty.  Higher
-- difficulty means the level will have more rooms and cover a larger area.
mkLevel :: Int -> IO Level
mkLevel difficulty = do
    let size = 80 * difficulty
    [levelWidth, levelHeight] <- replicateM 2 $ randomRIO (size,size)
    let randomP = (,) <$> randomRIO (2, levelWidth-3) <*> randomRIO (2, levelHeight-3)
    start <- randomP
    end <- randomP
    -- first the base geography: all rocks
    let baseGeo = array ((0,0), (levelWidth-1, levelHeight-1))
                        [((x,y),Rock) | x <- [0..levelWidth-1], y <- [0..levelHeight-1]]
    -- next the empty spaces that make the rooms
    -- for this we generate a number of center points
    centers <- replicateM (2 ^ difficulty + difficulty) randomP
    -- generate rooms for all those points, plus the start and end
    geo <- foldM (addRoom levelWidth levelHeight) baseGeo (start : end : centers)
    return $ Level start end geo (buildGeoImage geo)

-- |Add a room to a geography and return a new geography.  Adds a
-- randomly-sized room centered at the specified coordinates.
addRoom :: Int
        -> Int
        -- ^The width and height of the geographical area
        -> Geo
        -- ^The geographical area to which a new room should be added
        -> Coord
        -- ^The desired center of the new room.
        -> IO Geo
addRoom levelWidth levelHeight geo (centerX, centerY) = do
    size <- randomRIO (5,15)
    let xMin = max 1 (centerX - size)
        xMax = min (levelWidth - 1) (centerX + size)
        yMin = max 1 (centerY - size)
        yMax = min (levelHeight - 1) (centerY + size)
    let room = [((x,y), EmptySpace) | x <- [xMin..xMax - 1], y <- [yMin..yMax - 1]]
    return (geo // room)

pieceA, dumpA :: Attr
pieceA = defAttr `withForeColor` blue `withBackColor` green
dumpA = defAttr `withStyle` reverseVideo

processEvent :: World -> Event -> EventM (Next World)
processEvent world k = do
    case k of
        EvKey KEsc [] -> halt world
        EvKey KLeft  [] -> continue $ movePlayer world (-1) 0
        EvKey KRight [] -> continue $ movePlayer world 1 0
        EvKey KUp    [] -> continue $ movePlayer world 0 (-1)
        EvKey KDown  [] -> continue $ movePlayer world 0 1
        _               -> continue world

movePlayer :: World -> Int -> Int -> World
movePlayer world dx dy = do
    let Player (x, y) = player world
        x' = x + dx
        y' = y + dy
    case levelGeo (level world) ! (x',y') of
        EmptySpace -> world { player = Player (x',y') }
        _          -> world

updateDisplay :: World -> [Widget]
updateDisplay world = [ info, playerLayer, geoLayer ]
    where
        info = vBox [ hCenter $ txt "Move with the arrows keys. Press ESC to exit."
                    , hBorder
                    ]
        (px, py) = playerCoord $ player world
        playerLoc = Location (px, py)
        theLevel = level world
        playerLayer = center $ raw (char pieceA '@')
        geoLayer = centerAbout playerLoc $ raw $ levelGeoImage theLevel

imageForGeo :: LevelPiece -> Image
imageForGeo EmptySpace = char (defAttr `withBackColor` green) ' '
imageForGeo Rock = char defAttr 'X'

buildGeoImage :: Geo -> Image
buildGeoImage geo =
    let (geoWidth, geoHeight) = snd $ bounds geo
    in vertCat [ geoRow
               | y <- [0..geoHeight-1]
               , let geoRow = horizCat [ i
                                       | x <- [0..geoWidth-1]
                                       , let i = imageForGeo (geo ! (x,y))
                                       ]
               ]

playerX :: Player -> Int
playerX = fst . playerCoord

playerY :: Player -> Int
playerY = snd . playerCoord
