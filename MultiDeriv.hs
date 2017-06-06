{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

type Name = String

type Point a = [(Name, a)]

newtype DVal a = DVal { runDVal :: Point a -> (a, Name -> DVal a) }

instance Num a => Num (DVal a) where
  l + r    = DVal $ \pt -> let (lv, lv') = runDVal l pt
                               (rv, rv') = runDVal r pt
                           in  (lv + rv, \s -> lv' s + rv' s)

  l * r    = DVal $ \pt -> let (lv, lv') = runDVal l pt
                               (rv, rv') = runDVal r pt
                           in  (lv * rv, \s -> lv' s * r
                                           +  l * rv' s)
  
  negate v = DVal $ \pt -> let (val, val') = runDVal v pt
                           in  (negate val, \s -> negate (val' s))

  fromInteger = constant . fromInteger

  abs    = error "abs undefined for DVal"
  signum = error "signum undefined for DVal"

constant :: Num a => a -> DVal a
constant a = DVal $ \_ -> (a, \_ -> 0)

variable :: Num a => String -> DVal a
variable s = DVal $ \pt -> ( head [ v | (s', v) <- pt, s == s' ],
                             \s' -> if s' == s then
                                      DVal (\_ -> (1, const 0))
                                    else DVal (\_ -> (0, const 0)))

x, y :: DVal Double
x = variable "x"
y = variable "y"

example :: DVal Double
example = (x^2) * (y^2)

variables :: Point a -> [String]
variables = map fst

value :: DVal a -> Point a -> a
value dval pt = fst $ runDVal dval pt

derivative :: DVal a -> Name -> DVal a
derivative (DVal f) n = DVal $ \pt -> runDVal ((snd (f pt)) n) pt

gradient :: DVal a -> Point a -> Point a
gradient dval pt = [ (s, value (derivative dval s) pt) | s <- variables pt ]

hessian :: DVal a -> Point a -> [Point a]
hessian dval pt = (\n -> gradient (derivative dval n) pt) <$> variables pt

descend :: Num a => Point a -> DVal a -> a -> Point a
descend pt dval alpha =
  zipWith (\(s, v) (s', v') -> (s, v - alpha * v'))
    pt
    (gradient dval pt)

gradientDescent :: (Num a, Ord a)
                => Int -> a -> a -> DVal a -> Point a -> Point a
gradientDescent 0 _ _ _ pt = pt
gradientDescent n eps alpha f pt =
  let pt'       = descend pt f alpha
      diffValue = value f pt - value f pt'
      diffPts   = sum $ zipWith (\a b -> (snd a - snd b)^2)
                                       pt pt'
  in
  if diffValue < eps && diffPts < eps then
    pt'
  else
    gradientDescent (n - 1) eps alpha f pt'
