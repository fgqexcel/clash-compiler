module Identity where

import Prelude as P

import Clash.Prelude
import Clash.Netlist.Types

import Test.Tasty.Clash
import Test.Tasty.Clash.NetlistTest

topEntity :: Signal System Int -> Signal System Int
topEntity = id

testPath :: FilePath
testPath = "tests/shouldwork/Netlist/Identity.hs"

assertAssignsInOut :: Component -> IO ()
assertAssignsInOut (Component _ [i] [o] ds) =
  case ds of
    [Assignment oName (Identifier iName Nothing)]
      | iName == fst i && oName == fst (snd o) -> return ()
      | otherwise -> P.error "Incorrect input/output names"

    _ -> P.error "Identity circuit performs more than just one assignment"

assertAssignsInOut _ = error "Unexpected number of inputs and outputs"

getComponent :: (a, b, c, d) -> d
getComponent (_, _, _, x) = x

mainVHDL :: IO ()
mainVHDL = do
  netlist <- runToNetlistStage SVHDL [] testPath
  mapM_ (assertAssignsInOut . getComponent) netlist

mainVerilog :: IO ()
mainVerilog = do
  netlist <- runToNetlistStage SVerilog [] testPath
  mapM_ (assertAssignsInOut . getComponent) netlist

mainSystemVerilog :: IO ()
mainSystemVerilog = do
  netlist <- runToNetlistStage SSystemVerilog [] testPath
  mapM_ (assertAssignsInOut . getComponent) netlist

