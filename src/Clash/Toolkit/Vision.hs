module Clash.Toolkit.Vision where

import Clash.Prelude hiding (Exp)

import Language.Haskell.TH
import Codec.Picture
import GHC.Word

import qualified Data.Vector as V

type Width = 360
type Height = 240
type PixelChannels = 4

-- | MemBlob containing an image in RGBA format.
imageBlob :: MemBlob (Width * Height * PixelChannels) 8
imageBlob = $(do
  eimg <- runIO $ readImage "data/images/image_240.jpg"
  case eimg of
    Left err -> fail err
    Right img -> do
      let
        imgData = V.convert $ imageData (convertRGBA8 img) :: V.Vector Word8
      memBlobTH Nothing imgData)

-- | Video stream of pixels coming from a blockRam, also exposes the linear index of the pixel.
-- Allows for back pressure by controlling the acknowledge signal going into this circuit.
imageStream ::
  HiddenClockResetEnable dom =>
  -- | Acknowledgement that the exposed pixel is consumed this cycle.
  Signal dom Bool ->
  -- | Exposed Pixel along with its linear index.
  Signal dom (Maybe (Index (Width * Height * PixelChannels + 1), BitVector 8))
imageStream ack = mux outputValid (Just <$> bundle (rd, bramOut)) (pure Nothing)
 where
  bramOut = blockRamBlob imageBlob rd' (pure Nothing)
  running = register False (pure True)
  outputValid = running .&&. (rd ./=. pure maxBound)
  rd = register (0 :: Index (Width * Height * PixelChannels + 1)) rd'
  rd' = mux (outputValid .&&. ack) (satSucc SatError <$> rd) rd

