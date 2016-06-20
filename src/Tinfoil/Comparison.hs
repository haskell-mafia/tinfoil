{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Tinfoil.Comparison(
    ConstEq(..)
  , safeEq
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BSU

import           Foreign (Ptr, Word8, castPtr)
import           Foreign.C

import           P

import           System.IO (IO)

-- | Constant-time equality class; '==#' compares values in 'IO'
-- for situations where comparison timing is a side-effect. The other
-- method 'renderConstEq' converts the value to a 'ByteString'; the
-- timing of '==#' is independent of whether its operands share a
-- substring prefix.
--
-- In all cases where an 'Eq' instance is present, @x ==# y <=> x == y@.
--
-- Laws:
--
-- * @a ==# a@
-- * @a ==# b && b ==# c ==> a ==# c@
class ConstEq a where
  renderConstEq :: a -> ByteString

  (==#) :: a -> a -> IO Bool
  x ==# y = safeEq (renderConstEq x) (renderConstEq y)
infix 4 ==#

-- | Constant-time comparison. Will reveal if the length of the inputs differ
-- by exiting early, but will provide no other information.
safeEq :: ByteString -> ByteString -> IO Bool
safeEq a b =
  BSU.unsafeUseAsCStringLen a $ \(aPtr, aLen) ->
    BSU.unsafeUseAsCStringLen b $ \(bPtr, bLen) ->
      cBool <$> tinfoil_const_cmp (castPtr aPtr) (fromIntegral aLen) (castPtr bPtr) (fromIntegral bLen)
  where
    cBool 1 = True
    cBool _ = False

foreign import ccall safe tinfoil_const_cmp
  :: Ptr Word8
  -> CSize
  -> Ptr Word8
  -> CSize
  -> IO Word8
