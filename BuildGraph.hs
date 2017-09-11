module BuildGraph where

import LinearLaplace
import Data.IORef
import System.IO.Unsafe
import Control.Monad.State
import Data.List

makePoly :: [Double] -> String
makePoly (x:xs) =
  show x ++ foldl (\xs ys -> xs ++ "+" ++ ys) "" [ show x ++ if i == 1 then "*s" else "*s^" ++ show i | (i, x) <- zip [1..] xs ]

prettyOp :: Operation -> String
prettyOp (Transfer (up, down)) =
  let pup = makePoly up
      pdwn = makePoly down
  in replicate ((length pdwn - length pup) `div` 2) ' ' ++ pup ++ "\n" ++
     replicate (max (length pdwn) (length pup)) '-' ++ "\n" ++
     replicate ((length pup - length pdwn) `div` 2) ' ' ++ pdwn
prettyOp Add = "(+)"

showSig :: Signal -> String
showSig (Var x) = x
showSig (Comp ref) =
  unsafePerformIO $ do
    (op, _) <- readIORef ref
    return (prettyOp op)

type Vertex = (Signal, Int)
type Edge   = (Vertex, Vertex)
type Graph  = ([Vertex], [Edge])

makeGraph :: Signal -> Graph
makeGraph s = fst $ execState (go s) (([], []), 0)
  where
    go :: Signal -> State (Graph, Int) ()
    go (Var x) = do
      (g, n) <- get
      unless ((Var x) `elem` (map fst (fst g))) $
        put (merge g ([(Var x, n)], []), n + 1)
    go (Comp ref) = do
      (g, n) <- get
      unless (Comp ref `elem` (map fst (fst g))) $ do
        put (merge g ([(Comp ref, n)], []), n + 1)
        let (_, ss) = unsafePerformIO $ readIORef ref
        mapM_ go ss
        (g, n') <- get
        put (foldr merge ([], []) $ g : [([], [((s, i), (Comp ref, n))]) | (s, i) <- fst g, s `elem` ss], n')

merge :: Graph -> Graph -> Graph
merge (vsl, esl) (vsr, esr) = (union vsl vsr, union esl esr)

prettyGraph :: Graph -> String
prettyGraph (vs, es) = show ([ (showSig s, i) | (s, i) <- vs ], [ (snd v1, snd v2) | (v1, v2) <- es ])
