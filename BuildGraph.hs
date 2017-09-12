module BuildGraph where

import LinearLaplace
import Data.IORef
import System.IO.Unsafe
import Control.Monad.State
import Data.List
import qualified Data.Graph.Inductive.Graph as GR
import qualified Data.Graph.Inductive.PatriciaTree as GR
import Data.GraphViz
import qualified Data.Text.Lazy as FuckingText

makePolynomial :: [Double] -> String
makePolynomial (x:xs) =
  show x ++ foldl (\xs ys -> xs ++ "+" ++ ys) "" [ show x ++ if i == 1 then "*s" else "*s^" ++ show i | (i, x) <- zip [1..] xs ]

prettyExp :: LaplaceExp -> String
prettyExp e = case e of
  Transfer (up, down) ->
    let pup = makePolynomial up
        pdwn = makePolynomial down
    in replicate ((length pdwn - length pup) `div` 2) ' ' ++ pup ++ "\n" ++
       replicate (max (length pdwn) (length pup)) '-' ++ "\n" ++
       replicate ((length pup - length pdwn) `div` 2) ' ' ++ pdwn
  Add   -> "(+)"
  Var n ->  n

showSig :: Signal -> String
showSig (Comp ref) =
  unsafePerformIO $ do
    (op, _) <- readIORef ref
    return (prettyExp op)

type Vertex = (Signal, Int)
type Edge   = (Vertex, Vertex)
type Graph  = ([Vertex], [Edge])

makeGraph :: Signal -> Graph
makeGraph s = fst $ execState (go s) (([], []), 0)
  where
    go :: Signal -> State (Graph, Int) ()
    go sig@(Comp ref) = do
      (g, n) <- get
      unless (sig `elem` (map fst (fst g))) $ do
        put (merge g ([(sig, n)], []), n + 1)
        let (_, ss) = unsafePerformIO $ readIORef ref
        mapM_ go ss
        (g, n') <- get
        put (foldr merge ([], []) $ g : [([], [((s, i), (sig, n))]) | (s, i) <- fst g, s `elem` ss], n')

merge :: Graph -> Graph -> Graph
merge (vsl, esl) (vsr, esr) = (union vsl vsr, union esl esr)

prettyGraph :: Graph -> String
prettyGraph (vs, es) = show ([ (showSig s, i) | (s, i) <- vs ], [ (snd v1, snd v2) | (v1, v2) <- es ])

makeFGL :: Graph -> GR.Gr Signal ()
makeFGL (vs, es) = GR.mkGraph [ (r, l) | (l, r) <- vs ] [ (l, r, ()) | ((_, l), (_, r)) <- es ]

instance Labellable Signal where
  toLabelValue = textLabelValue . FuckingText.pack . showSig

instance Labellable () where
  toLabelValue _ = textLabelValue FuckingText.empty

showGraph :: Signal -> IO ()
showGraph = preview . makeFGL . makeGraph
