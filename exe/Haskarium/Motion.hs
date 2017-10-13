{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Haskarium.Motion
    ( Interactive (..)
    ) where

import           Graphics.Gloss (Point)
import           Graphics.Gloss.Geometry.Angle (normalizeAngle)
import           Graphics.Gloss.Geometry.Line (intersectSegHorzLine,
                                               intersectSegVertLine)
import           Graphics.Gloss.Interface.Pure.Game (Event)
import           System.Random (StdGen, randomR)

import           Haskarium.Const
import           Haskarium.Types (Angle, Ant, Centipede (..), Creature (..),
                                  Distance, Flea (..), Fly, Speed, Time,
                                  World (..))
import           Haskarium.Util (distance)

class Interactive a where
    onTick :: Time -> a -> StdGen -> (a, StdGen)
    onTick _ a g = (a, g)

    onEvent :: Event -> a -> StdGen -> (a, StdGen)
    onEvent _ a g = (a, g)

instance Interactive World where
    onTick dt World{ants, centipedes, fleas, flies} g0 = (world, g4)
      where
        world = World
            { ants        = ants'
            , centipedes  = centipedes'
            , fleas       = fleas'
            , flies       = flies'
            }
        (ants', g1)       = mapWithRandom (onTick dt) ants g0
        (centipedes', g2) = mapWithRandom (onTick dt) centipedes g1
        (fleas', g3)      = mapWithRandom (onTick dt) fleas g2
        (flies', g4)      = mapWithRandom (onTick dt) flies g3

mapWithRandom :: (a -> StdGen -> (b, StdGen)) -> [a] -> StdGen -> ([b], StdGen)
mapWithRandom _    []    g0 = ([], g0)
mapWithRandom f (x : xs) g0 = let
    (y, g1) = f x g0
    (ys, g2) = mapWithRandom f xs g1
    in (y : ys, g2)

instance Interactive (Creature Ant) where
    onTick dt = creatureTurn dt . run 20 dt

instance Interactive (Creature Fly) where
    onTick dt = creatureTurn dt . run 200 dt

instance Interactive (Creature Flea) where
    onTick dt creature@Creature{species = Flea{idleTime}} =
        creatureTurn dt $
        if idleTime < fleaMaxIdleTime then
            creature{species = Flea{idleTime = idleTime + dt}}
        else
            (creatureMovedCheckCollisions creature fleaJumpDistance)
                {species = Flea{idleTime = idleTime + dt - fleaMaxIdleTime}}
      where
        fleaJumpDistance = 100

run :: Speed -> Time -> Creature s -> Creature s
run speed dt creature = creatureMovedCheckCollisions creature dist
  where
    dist = dt * speed

instance Interactive (Creature Centipede) where
    onTick dt creature@Creature{species} =
        creatureTurn dt $ runHead{species = Centipede{segments=newSegments}}
      where
        Centipede{segments} = species

        runHead = run 10 dt creature

        newSegments = runChain (position runHead) segments

        runChain _ [] = []
        runChain p@(px, py) (c@(cx, cy) : next) = let
            dist = distance p c
            c' =
                if dist < maxNeck then
                    c
                else let
                    dist' = maxNeck
                    squeezeFactor = dist / dist'
                    cx' = px - (px - cx) / squeezeFactor
                    cy' = py - (py - cy) / squeezeFactor
                    in (cx', cy')
            in c' : runChain c' next

        maxNeck = 1.5 * centipedeSegmentRadius

creatureTurn :: Time -> Creature s -> StdGen -> (Creature s, StdGen)
creatureTurn dt creature g =
    (creature{targetDir = targetDir', currentDir = newCurrentDir}, g')
  where
    Creature{targetDir, currentDir, turnRate} = creature
    (targetDir', g') =
        if   (currentDir  <= targetDir && targetDir <= currentDir')
          || (currentDir' <= targetDir && targetDir <= currentDir ) then
            randomR (0, 2 * pi) g
        else
            (targetDir, g)
    newCurrentDir
        | currentDir' < 0       = currentDir' + 2 * pi
        | currentDir' > 2 * pi  = currentDir' - 2 * pi
        | otherwise             = currentDir'
    currentDir' = currentDir + trSign * turnRate * dt
    trSign =
        if (-pi < delta && delta < 0) || delta > pi then
            -1
        else
            1
    delta = targetDir - currentDir

creatureMovedCheckCollisions :: Creature s -> Distance -> Creature s
creatureMovedCheckCollisions creature dist
    | dist <= 0 = creature
    | otherwise = creature{position = advance creature dist}

pointMoved :: Point -> Distance -> Angle -> Point
pointMoved (x, y) dist direction = (x + dx, y + dy)
  where
    dx = dist * cos direction
    dy = dist * sin direction

advance :: Creature s -> Distance -> Point
advance Creature{position = p0, currentDir, size} dist =
    checkDirs pU pD pL pR
  where
    checkDirs (Just pu) _ _ _ | isMovedUp    = pu
    checkDirs _ (Just pd) _ _ | isMovedDown  = pd
    checkDirs _ _ (Just pl) _ | isMovedLeft  = pl
    checkDirs _ _ _ (Just pr) | isMovedRight = pr
    checkDirs _ _ _ _                        = p1
    normDir = normalizeAngle currentDir
    isMovedUp = normDir < pi
    isMovedDown = normDir > pi
    isMovedLeft = normDir > pi/2 && normDir < 3 * pi / 2
    isMovedRight = normDir < pi/2 || normDir > 3 * pi / 2
    p1 = pointMoved p0 dist currentDir
    pU = intersectSegHorzLine p0 p1 ((fromIntegral height / 2) - size / 2)
    pD = intersectSegHorzLine p0 p1 ((- fromIntegral height / 2) + size / 2)
    pL = intersectSegVertLine p0 p1 ((- fromIntegral width / 2) + size / 2)
    pR = intersectSegVertLine p0 p1 ((fromIntegral width / 2) - size / 2)
