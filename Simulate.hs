module Simulate where

import qualified Data.Map as M
import Control.Monad.State
import LinearLaplace
import Data.Maybe
import Data.Complex
import Data.List
import Data.Ord

data Exp = Variable Name
         | Sum NodeId NodeId
         | Conv NodeId Laplace
         deriving (Ord, Eq, Show)

type NodeId = Int

type Equations = M.Map NodeId Exp

data St = St { next :: Int, seen :: [(Signal, NodeId)], equations :: M.Map NodeId Exp }

-- The zero:th equation corresponds to the actual output of the signal.
toEquations :: Signal -> Equations
toEquations s = equations $ execState (go s) (St 0 [] M.empty)
  where
    go :: Signal -> State St ()
    go s = do
      St next seen eqns <- get
      unless (s `elem` map fst seen) $ do
        case contents s of
          (Var n, [])   -> do
            put $ St (next + 1) ((s, next):seen) (M.insert next (Variable n) eqns)
          (Add, [l, r]) -> do
            put $ St (next + 1) ((s, next):seen) eqns
            go l
            go r
            modify $ \(St n s e) -> St n s (M.insert next (Sum (fromJust (lookup l s)) (fromJust (lookup r s))) e)
          (Transfer l, [y]) -> do
            put $ St (next + 1) ((s, next):seen) eqns
            go y
            modify $ \(St n s e) -> St n s (M.insert next (Conv (fromJust (lookup y s)) l) e)

{- When we have the set of equations which relate the output signal (NodeId 0) with the variables
 - we can compute the transfer function Y(s) = X0(s)F0(s) + X1(s)F1(s) + ...
 -
 - If we only have one input variable (which we will assume to begin with) we have an equation on the
 - form Y(s) = X(s)F(s). If we also have a sequence xs of real values which denote the values of x
 - in the time domain we can compute the discrete fourier transform of x. Using the DFT we can compute
 - each individual frequency response F(jw) for all w, using this we can compute the frequency response
 - (phase shift + amplitude change) for each w, using this we can then simulate the whole system by first
 - element-wise multiplying the DFT with the frequency responses and then running the inverse DFT on the
 - result -}
-- Scetch of the implementation
equationsToTransforms :: Equations -> [(Name, Laplace)]
equationsToTransforms = undefined

responses :: Laplace -> Double -> Int -> [Complex Double]
responses (up, dwn) w k = [ evalAt (0 :+ (i * w)) up / evalAt (0 :+ (i * w)) dwn | index <- [0..k-1], let i = fromInteger . toInteger $ index ]
  where
    evalAt c poly = sum [ (p :+ 0) * c ^ i | (i, p) <- zip [0..] poly ]

dft :: [Double] -> [Complex Double]
dft = undefined

idft :: [Complex Double] -> [Double]
idft = undefined

simulate :: Signal -> Double -> [(Name, [Double])] -> [Double]
simulate signal time signals =
  let transforms    = equationsToTransforms $ toEquations signal
      w             = 1 / time
      k             = length (head signals)
      resps         = [ (name, responses lap w k) | (name, lap) <- transforms ]
      superposition = zipWith (zipWith (*)) (snd <$> sortBy (comparing fst) resps) (dft . snd <$> sortBy (comparing fst) signals)
  in foldr1 (zipWith (+)) $ (idft <$> superposition)
