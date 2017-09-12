
import LinearLaplace
import BuildGraph

piLoop :: Signal -> (Signal -> Signal) -> Signal
piLoop x system =
  let err = sub x y
      u   = add (integ err) (scale 2 err)
      y   = system u
  in y
