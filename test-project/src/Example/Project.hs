{-# OPTIONS_GHC -Wno-orphans #-}

module Example.Project where

import Clash.Prelude
import qualified Clash.Explicit.Signal.Delayed as D

-- Create a domain with the frequency of your input clock. For this example we used
-- 50 MHz.
createDomain vSystem{vName="Dom50", vPeriod=hzToPeriod 50e6}

-- | A simple accumulator that works on unsigned numbers of any size.
-- It has hidden clock, reset, and enable signals.
accum ::
  (HiddenClockResetEnable dom, KnownNat n) =>
  Signal dom (Unsigned n) ->
  Signal dom (Unsigned n)
accum = mealy accumT 0
  where
    accumT s i = (s + i, s)

-- | @topEntity@ is Clash's equivalent of @main@ in other programming languages.
-- Clash will look for it when compiling "Example.Project" and translate it to
-- HDL. While polymorphism can be used freely in Clash projects, a @topEntity@
-- must be monomorphic and must use non-recursive types. Or, to put it
-- hand-wavily, a @topEntity@ must be translatable to a static number of wires.
--
-- Top entities must be monomorphic, meaning we have to specify all type variables.
-- In this case, we are using the @Dom50@ domain, which we created with @createDomain@
-- and we are using 8-bit unsigned numbers.
topEntity ::
  Clock Dom50 ->
  Reset Dom50 ->
  Enable Dom50 ->
  Vec 8 (DSignal Dom50 0 (Unsigned 16)) ->
  DSignal Dom50 3 (Unsigned 16)
topEntity clk rst ena vec = 1 + (pipelinedSum clk ena (fmap (*2) vec))

-- To specify the names of the ports of our top entity, we create a
-- @Synthesize@ annotation.
{-# ANN topEntity
  (Synthesize
    { t_name = "accum"
    , t_inputs = [ PortName "CLK"
                 , PortName "RST"
                 , PortName "EN"
                 , PortName "DIN"
                 ]
    , t_output = PortName "DOUT"
    }) #-}

{-# OPAQUE topEntity #-}

-- | A simple monomorphic function that adds two signed numbers
-- This is an example of a function we want to synthesize
plusSigned :: Signed 8 -> Signed 8 -> Signed 8
plusSigned a b = a + b

-- | A polymorphic function - this cannot be directly synthesized
-- This demonstrates the difference we need to detect
plusPoly :: (Num a) => a -> a -> a
plusPoly a b = a + b

-- | Another monomorphic function - multiplies two unsigned numbers
multUnsigned :: Unsigned 16 -> Unsigned 16 -> Unsigned 32
multUnsigned a b = resize a * resize b

pipelinedSum :: Clock Dom50 -> Enable Dom50 -> Vec 8 (DSignal Dom50 0 (Unsigned 16)) -> DSignal Dom50 3 (Unsigned 16)
pipelinedSum clk ena = D.delayedFold d1 0 (+) ena clk

{-# ANN pipelinedSum
  (Synthesize
    { t_name = "pipelinedSum"
    , t_inputs = [ PortName "CLK"
                 , PortName "EN"
                 , PortName "Elements"
                 ]
    , t_output = PortName "DOUT"
    }) #-}

{-# OPAQUE pipelinedSum #-}

-- | A deliberately deep combinational chain: a left-fold sum over a
-- 16-element vector.  Unlike a tree reduction, this expresses an explicit
-- linear data-dependency (each addition consumes the previous result),
-- so Yosys's `ltp` reports a non-trivial combinational depth — useful
-- for exercising logic-depth reporting in the Elaboration / Synthesis
-- stages.
deepChain :: Vec 16 (Unsigned 8) -> Unsigned 8
deepChain vec = foldl (+) (head vec) (tail vec)

{-# ANN deepChain
  (Synthesize
    { t_name = "deepChain"
    , t_inputs = [ PortName "XS" ]
    , t_output = PortName "Y"
    }) #-}

{-# OPAQUE deepChain #-}

deepChain2 :: Vec 16 (Unsigned 8) -> Unsigned 8
deepChain2 = fold (+)

{-# ANN deepChain2
  (Synthesize
    { t_name = "deepChain"
    , t_inputs = [ PortName "XS" ]
    , t_output = PortName "Y"
    }) #-}

{-# OPAQUE deepChain2 #-}
