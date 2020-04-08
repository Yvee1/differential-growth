{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, PatternSynonyms #-}
module Lib where

import ChaosBox
import Linear.Metric
import Linear.Vector
import Control.Monad.Random.Class (getRandomR, getRandomRs)
import Control.Monad.IO.Class
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import qualified Data.KdTree.Static as KD
import Data.Foldable (toList)

type Pt = P2
type Node = Pt
type Adjecencies = (Int, [Int])
type System = (U.Vector Node, V.Vector Adjecencies, U.Vector Vec)

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
  nodes <- pure U.fromList <*> sequence [newNode c r θ maxΔθ | i <- [1..n], let θ = (fromIntegral i) * 2 * pi / (fromIntegral n)]

  return (nodes, connect nodes, U.replicate n 0)

-- Returns the edges that connect the given nodes
-- in order. Last point is connected to the first.
connect :: U.Vector Node -> V.Vector Adjecencies
connect pts = V.fromList [(i, [(i+1) `rem` n, (i-1) `mod` n]) | i <- [0..n-1]]
  where n = U.length pts

drawSystem :: System -> Render ()
drawSystem system = drawEdges system *> drawNodes system

drawEdges :: System -> Render ()
drawEdges (nodes, edges, _) = do
  setSourceHSV (HSV (180/360) 1 1)
  V.mapM_ (\(i, js) -> mapM_ (\j -> draw (Line (nodes U.! i) (nodes U.! j)) *> stroke) js) edges

drawNodes :: System -> Render ()
drawNodes (nodes, _, _) = do
  setSourceRGB white
  U.mapM_ (\n -> draw n *> fill) nodes

drawRejectionRadius :: System -> Render ()
drawRejectionRadius (nodes, _, _) = do
  setSourceHSVA $ WithAlpha (HSV 120 1 1) 0.4
  U.mapM_ (\n -> draw (circle n rejectionRadius) *> fill) nodes

updateSystem :: System -> Generate System
updateSystem s = do
  s' <- attractNodes s
  return s'

attractNodes :: System -> Generate System
attractNodes (nodes, edges, vels) = do
  let attract (i, js) = attractNeighbours (nodes U.! i) (map (\j -> (j, nodes U.! j)) js)

  --  attractionForces :: [(Int, Vec)]
      attractionForces = concatMap attract . V.toList $ edges
      indexedNodeList = zip [0..] (U.toList nodes)
      kdt = KD.build (\(_, v) -> toList v) indexedNodeList
      
      reject ni@(i, n) = rejectNodes n toReject
        where toReject = filter (\(j, _) -> i /= j && not (j `elem` (snd (edges V.! i)))) (KD.inRadius kdt rejectionRadius ni)

  --  rejectionForces :: [(Int, Vec)]
      rejectionForces = concatMap reject indexedNodeList

      friction = zip [0..] $ map (\v -> -(normalize v) ^* (quadrance v) ^* frictionForce) (U.toList vels)
  
      forces = M.fromListWith (+) (attractionForces ++ rejectionForces ++ friction)
      vels' = U.imap (\i v -> v + forces M.! i) vels
      nodes' = U.imap (\i pt -> pt + vels' U.! i) nodes

  return (nodes', edges, vels')

attractionForce :: Double
attractionForce = 0.01

restLength :: Double
restLength = 0.8

rejectionForce :: Double
rejectionForce = 0.01

rejectionRadius :: Double
rejectionRadius = 2

frictionForce = 10 :: Double

attractNeighbours :: Node -> [(Int, Node)] -> [(Int, Vec)]
attractNeighbours n ns = map (\(i, m) -> (i, attractPair n m)) ns

attractPair :: Node -> Node -> Vec
attractPair n1 n2 = normalize (n1 - n2) ^* attractionForce ^* displacement
  where displacement = (norm (n1 - n2)) - restLength

rejectNodes :: Node -> [(Int, Node)] -> [(Int, Vec)]
rejectNodes n = map (rejectPair n)
  where rejectPair n1 (i, n2) = (i, normalize (n2 - n1) ^* rejectionForce ^/ (quadrance (n2-n1)))