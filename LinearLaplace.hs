module LinearLaplace where

import Data.Complex
import Data.IORef 
import System.IO.Unsafe

type Name = String

data LaplaceExp = Transfer Laplace
                | Add
                | Var Name
                deriving (Ord, Eq, Show)

data Signal = Comp (IORef (LaplaceExp, [Signal]))
            deriving (Eq)

comp :: LaplaceExp -> [Signal] -> Signal
comp op ss = Comp (unsafePerformIO $ newIORef (op, ss))

contents :: Signal -> (LaplaceExp, [Signal])
contents (Comp r) = unsafePerformIO $ readIORef r

type Polynomial = [Double]

type Laplace = (Polynomial, Polynomial)

(//) :: Polynomial -> Polynomial -> Laplace
xs // ys = (xs, ys)

var :: String -> Signal
var x = comp (Var x) []

scale :: Double -> Signal -> Signal 
scale fac sig = comp (Transfer $ [fac] // [1]) [sig]

integ :: Signal -> Signal
integ s = comp (Transfer $ [1] // [0, 1]) [s]

deriv :: Signal -> Signal
deriv s = comp (Transfer $ [0, 1] // [1]) [s]

add :: Signal -> Signal -> Signal
add l r = comp Add [l, r]

sub :: Signal -> Signal -> Signal
sub l r = add l (scale (-1) r)
