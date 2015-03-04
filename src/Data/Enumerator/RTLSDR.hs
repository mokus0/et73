module Data.Enumerator.RTLSDR where

import qualified Control.Exception as E
import Data.Complex
import Data.Enumerator (Iteratee(..), Step(..), Stream(..))
import Data.IORef
import qualified Data.Vector.Unboxed as U
import Data.Word
import IQ
import qualified RTLSDR

{-# INLINE readIter #-}
readIter :: RTLSDR.RTLSDR -> Word32 -> Word32 -> Iteratee (U.Vector (Complex Float)) IO a -> IO (Maybe a)
readIter rtl bufNum bufLen iter = do
    next <- runIteratee iter
    
    case next of
        Error e       -> E.throw e
        Yield a _     -> do
            return (Just a)
        Continue{}    -> do
            r_next <- newIORef next
            
            ok <- RTLSDR.readAsync rtl bufNum bufLen $ \p_buf len -> do
                step <- readIORef r_next
                case step of
                    Continue k    -> do
                        chunk <- packBuf p_buf len
                        runIteratee (k (Chunks [chunk])) >>= writeIORef r_next
                        
                    _               -> do
                        RTLSDR.cancelAsync rtl
                        writeIORef r_next step
            
            if ok
                then do
                    end <- readIORef r_next
                    case end of
                        Error e       -> E.throw e
                        Yield a _     -> return (Just a)
                        Continue{}    -> return Nothing
                else return Nothing

