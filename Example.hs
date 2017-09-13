
import LinearLaplace
import PrintGraph
import SolveTransferFunction

piLoop :: Double -> Double -> Signal -> (Signal -> Signal) -> Signal
piLoop p i x system =
  let err = sub x y
      u   = add (scale i (integ err)) (scale p err)
      y   = system u
  in y
