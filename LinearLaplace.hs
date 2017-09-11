module LinearLaplace where

import Data.Complex
import Data.IORef 
import System.IO.Unsafe

type Name = String

data Operation = Transfer Laplace
               | Add
               deriving (Ord, Eq, Show)

data Signal = Var  Name
            | Comp (IORef (Operation, [Signal]))
            deriving (Eq)

comp :: Operation -> [Signal] -> Signal
comp op ss = Comp (unsafePerformIO $ newIORef (op, ss))

type Polynomial = [Double]

type Laplace = (Polynomial, Polynomial)

(//) :: Polynomial -> Polynomial -> Laplace
xs // ys = (xs, ys)

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
