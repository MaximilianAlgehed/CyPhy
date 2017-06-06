module LinearLaplace where
import Data.Complex

{- Boring aux stuff -}
liftF :: (a -> a -> a) -> (b -> a) -> (b -> a) -> (b -> a)
liftF op f g = \b -> f b `op` g b

instance Num a => Num (b -> a) where
  (+)         = liftF (+)
  (*)         = liftF (*)
  negate      = (negate .)
  abs         = (abs .)
  signum      = (signum .)
  fromInteger = const . fromInteger

{- Where the interesting stuff begins -}
type Laplace a = Complex a -> Complex a

delta :: RealFloat a => a -> Laplace a 
delta a s = exp (negate (s * (a :+ 0)))

type ID        = Int

data Node a    = Node { identity :: ID, content :: Content a }

data Content a = Input
               | Output
               | Nil
               | Add ID ID
               | Transfer ID (Laplace a) ID

type BlockDiagram a = [Node a]

input, output :: ID
input  = 0
output = 1

empty :: BlockDiagram a
empty = [Node input Input, Node output Output]
