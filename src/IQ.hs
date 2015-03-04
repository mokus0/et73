module IQ where

import Data.Complex
import qualified Data.Vector.Unboxed as U
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

data IQ = IQ !Word8 !Word8

instance Storable IQ where
    alignment   _ = 2
    sizeOf      _ = 2
    peek p = do
        i <- peekByteOff p 0
        q <- peekByteOff p 1
        return (IQ i q)
    poke p (IQ i q) = do
        pokeByteOff p 0 i
        pokeByteOff p 1 q

toC :: Num t => IQ -> Complex t
toC (IQ i q) = (fromIntegral i - 128) :+ (fromIntegral q - 128)

packBuf :: Ptr CUChar -> Int -> IO (U.Vector (Complex Float))
packBuf p_uchar len = do
    let p_iq    = castPtr p_uchar :: Ptr IQ
        elems   = len `div` sizeOf (IQ 0 0)
    
    U.generateM elems (fmap toC . peekElemOff p_iq)
