{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, PatternSynonyms #-}
module Lib where

import ChaosBox
import Linear.Metric
import Linear.Vector
import Control.Monad.Random.Class (getRandomR, getRandomRs)
import Control.Monad.IO.Class
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as V

type Pt = P2
type Node = Pt
type Edge = (Int, Int)
type System = (Vector Node, Vector Edge, Vector Vec)

type Vec = V2 Double

instance Draw Node where
  draw pt = draw (circle pt 0.1)

pattern Node :: Double -> Double -> Node
pattern Node x y = P2 x y

infix 5 :&
pattern (:&) x y = P2 x y

newNode :: P2 -> Double -> Double -> Double -> Generate Node
newNode (x :& y) r θ maxΔθ = do
  dθ <- getRandomR (-maxΔθ, maxΔθ)
  return $ Node (x + r * cos (θ + dθ)) (y + r * sin (θ + dθ))

start :: P2 -> Double -> Int -> Generate System
start c r n = do
  let maxΔθ = pi * r / (fromIntegral n)
  nodes <- pure V.fromList <*> sequence [newNode c r θ maxΔθ | i <- [1..n], let θ = (fromIntegral i) * 2 * pi / (fromIntegral n)]

  return (nodes, connect nodes, V.replicate n 0)

-- Returns the edges that connect the given nodes
-- in order. Last point is connected to the first.
connect :: Vector Node -> Vector Edge
connect pts = V.fromList [(i, (i+1) `rem` n) | i <- [0..n-1]]
  where n = V.length pts

drawSystem :: System -> Render ()
drawSystem system = drawEdges system *> drawNodes system

drawEdges :: System -> Render ()
drawEdges (nodes, edges, _) = do
  setSourceHSV (HSV (180/360) 1 1)
  V.mapM_ (\(i, j) -> draw (Line (nodes ! i) (nodes ! j)) *> stroke) edges

drawNodes :: System -> Render ()
drawNodes (nodes, _, _) = do
  setSourceRGB white
  V.mapM_ (\n -> draw n *> fill) nodes

updateSystem :: System -> Generate System
updateSystem s = do
  s' <- attractNodes s
  return s'

attractNodes :: System -> Generate System
attractNodes (nodes, edges, vels) = do
  let attract (i, j) = [(i, ni), (j, nj)]
        where (ni, nj) = attractPair ((nodes ! i), (nodes ! j))

  -- attractionForces :: [(Int, Vec)]
  let attractionForces = concatMap attract . V.toList $ edges
  let forces = M.fromListWith (+) attractionForces
  let vels' = V.imap (\i v -> v + forces M.! i) vels
  let nodes' = V.imap (\i pt -> pt + vels' ! i) nodes

  return (nodes', edges, vels')

attractionForce :: Double
attractionForce = 0.1

attractPair :: (Node, Node) -> (Vec, Vec)
attractPair (n1, n2) = ((n2 - n1) ^* attractionForce, (n1 - n2) ^* attractionForce)