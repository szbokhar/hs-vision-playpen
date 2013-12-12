{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Prelude                          as P

import Data.Array.Repa                  as R
import Data.Array.Repa.IO.DevIL
import Data.Array.Repa.Repr.ForeignPtr ( )
import Data.Convertible

import Control.Monad        ( when )
import Data.Word
import System.Directory     ( doesFileExist, removeFile )
import System.Environment   ( getArgs )

import Debug.Trace

main :: IO ()
main = do
    [fname, kw, kh, ksig] <- getArgs
    let newFname = "new-" P.++ fname

    img' <- (runIL $ readImage fname) >>= manipulateImage (read kw) (read kh) (read ksig)

    delete <- doesFileExist newFname
    when delete $ removeFile newFname
    runIL $ writeImage newFname img'

manipulateImage :: Int -> Int -> Float -> Image -> IO Image
manipulateImage kw kh ksig img = do
    img' <- computeP $ (greyBlur kw kh ksig) $ grey (delay imgA)
    return (Grey img')
  where
    (_,imgA) = sep img
    sep (RGB x) = (RGB, x)
    sep (RGBA x) = (RGBA, x)
    sep _ = undefined

grey :: Array D DIM3 Word8 -> Array D DIM2 Word8
grey img = traverse img reduceShape (toGreyPixel)
  where
    {-# INLINE reduceShape #-}
    reduceShape (listOfShape->[_,w,h]) = ix2 h w
    reduceShape _ = undefined
    {-# INLINE toGreyPixel #-}
    toGreyPixel fn (Z :. y :. x) = word8
                                 $ 0.2989*(float $ fn $ ix3 y x 0)
                                 + 0.5870*(float $ fn $ ix3 y x 1)
                                 + 0.1140*(float $ fn $ ix3 y x 2)

greyBlur :: Int -> Int -> Float -> Array D DIM2 Word8 -> Array D DIM2 Word8
greyBlur kWidth kHeight sigma img = traverse img id (blur kWidth kHeight)
  where
    (Z :. h :. w) = extent img
    {-# INLINE blur #-}
    blur kw kh fn (listOfShape->[x,y]) = (word8 . sum)
                                 $ P.map (\(i,c) -> c * float (fn i)) (spots kw kh x y)
    blur _ _ _ _ = undefined
    {-# INLINE spots #-}
    spots kw kh x y = filter inBounds normalized
      where factors = [(ix2 (y-y') (x-x'), gauss x' y' sigma) | x' <- [-kw..kw], y' <- [-kh..kh]]
            total = sum $ P.map snd factors
            normalized = P.map (\(a,b) -> (a,b/total)) factors

    {-# INLINE inBounds #-}
    inBounds (listOfShape->[x,y],_) = x >= 0 && x < w && y >= 0 && y < h
    inBounds _ = undefined


{-# INLINE gauss #-}
gauss :: (Convertible f1 Float, Convertible f2 Float) =>
         f1 -> f2 -> Float -> Float
gauss (float->a) (float->b) sig = exp $ (-a*a-b*b)/(2*sig*sig)

{-# INLINE float #-}
{-# INLINE word8 #-}
{-# INLINE int #-}
float :: Convertible a Float => a -> Float
word8 :: Convertible a Word8 => a -> Word8
int :: Convertible a Int => a -> Int

float = convert
word8 = convert
int = convert

instance Convertible a a where safeConvert = Right
