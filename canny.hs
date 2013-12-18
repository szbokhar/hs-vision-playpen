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

-- |Main function
main :: IO ()
main = do
    -- Parse Arguments
    [fname] <- getArgs
    let newFname = "new-" P.++ fname

    -- Run algorithm
    img' <- (runIL $ readImage fname) >>= manipulateImage

    -- Write new file
    delete <- doesFileExist newFname
    when delete $ removeFile newFname
    runIL $ writeImage newFname img'

-- |Runs the image algorithm
manipulateImage :: Image -> IO Image
manipulateImage img = do
    img' <- computeP (delay imgA)               -- Prepare image
        >>= (computeP . grey)                   -- To Grayscale
        >>= (computeP . greyBlur 1 1 2)         -- Blur
        >>= (computeP . gradient)
        >>= (computeP . nonMaximaSuppress)
        >>= (computeP . doubleThreshold)
        -- >>= (computeP . R.map (word8 . fst))
    return (Grey img')
  where
    (_,imgA) = sep img
    sep (RGB x) = x `deepSeqArray` (RGB, x)
    sep (RGBA x) = (RGBA, x)
    sep _ = undefined
{-# NOINLINE manipulateImage #-}

-- |Converts array to grayscale
grey :: Array U DIM3 Word8 -> Array D DIM2 Word8
grey img = img `deepSeqArray` img'
  where
    img' = traverse img reduceShape (toGreyPixel)
    reduceShape (listOfShape->[_,w,h]) = ix2 h w
    reduceShape _ = undefined
    toGreyPixel fn (Z :. y :. x) = word8
                                 $ 0.2989*(float $ fn $ ix3 y x 0)
                                 + 0.5870*(float $ fn $ ix3 y x 1)
                                 + 0.1140*(float $ fn $ ix3 y x 2)
    {-# INLINE reduceShape #-}
    {-# INLINE toGreyPixel #-}
{-# NOINLINE grey #-}

-- Blurs a grayscale image
greyBlur :: Int -> Int -> Float -> Array U DIM2 Word8 -> Array D DIM2 Word8
greyBlur kWidth kHeight sigma img = img `deepSeqArray` img'
  where
    img' = traverse img id (blur kWidth kHeight)
    (Z :. h :. w) = extent img

    blur kw kh fn (listOfShape->[x,y]) =
        (word8 . sum) $ P.map (\(i,c) -> c * float (fn i)) (spots kw kh x y)
    blur _ _ _ _ = undefined

    spots kw kh x y = filter (inBounds w h . fst) normalized
      where factors = [ (ix2 (y-y') (x-x'), gauss x' y' sigma)
                      | x' <- [-kw..kw], y' <- [-kh..kh]]
            total = sum $ P.map snd factors
            normalized = P.map (\(a,b) -> (a,b/total)) factors

    {-# INLINE blur #-}
    {-# INLINE spots #-}
{-# NOINLINE greyBlur #-}

-- |Computes the gradient magnitude and direction
gradient :: Array U DIM2 Word8 -> Array D DIM2 (Int, Int)
gradient img = img `deepSeqArray` img'
  where
    img' = traverse img id gMag
    (Z :. h :. w) = extent img

    gMag fn (listOfShape->[x,y]) = (abs gX + abs gY, theta gY gX)
      where vals = P.map (int . fn) (spots x y)
            gX = (int . (`div` 2) . sum) $ P.zipWith (*) kX vals
            gY = (int . (`div` 2) . sum) $ P.zipWith (*) kY vals
    gMag _ _ = undefined

    spots x y = filter (inBounds w h) factors
      where factors = P.map (\(x',y') -> ix2 (y-y') (x-x'))
                      [ (-1,-1), ( 0,-1), ( 1,-1)
                      , (-1, 0), ( 0, 0), ( 1, 0)
                      , (-1, 1), ( 0, 1), ( 1, 1) ]

    theta (float->dy) (float->dx) = round(4*rad/pi) `mod` 4
        where rad | r < 0       = r+pi
                  | otherwise   = r
                where r = atan2 dy dx

    kX :: [Int]
    kX = [ -1, 0, 1
         , -2, 0, 2
         , -1, 0, 1 ]
    kY :: [Int]
    kY = [ -1,-2,-1
         ,  0, 0, 0
         ,  1, 2, 1 ]

    {-# INLINE gMag #-}
    {-# INLINE spots #-}
    {-# INLINE theta #-}
    {-# INLINE kX #-}
    {-# INLINE kY #-}
{-# NOINLINE gradient #-}

-- |Computes the gradient magnitude and direction
nonMaximaSuppress :: Array U DIM2 (Int,Int) -> Array D DIM2 (Int, Int)
nonMaximaSuppress img = img `deepSeqArray` img'
  where
    img' = traverse img id suppress
    (Z :. h :. w) = extent img

    suppress fn (listOfShape->[x,y])
        | dir c == 0    = suppressPoint $ mag c > mag w  && mag c > mag e
        | dir c == 1    = suppressPoint $ mag c > mag nw && mag c > mag se
        | dir c == 2    = suppressPoint $ mag c > mag n  && mag c > mag s
        | dir c == 3    = suppressPoint $ mag c > mag ne && mag c > mag sw
      where [nw,n,ne,w,c,e,sw,s,se] = spots x y
            mag = maybe 0 (fst . fn)
            dir = maybe (error "This is wrong") (snd . fn)
            suppressPoint False = (0,0)
            suppressPoint True = (mag c, dir c)
            {-# INLINE suppressPoint #-}
    suppress _ _ = undefined

    spots x y = P.map toMaybeIndex
                      [ (-1,-1), ( 0,-1), ( 1,-1)
                      , (-1, 0), ( 0, 0), ( 1, 0)
                      , (-1, 1), ( 0, 1), ( 1, 1) ]
      where toMaybeIndex (x',y')
                | inBounds w h idx  = Just idx
                | otherwise         = Nothing
              where idx = ix2 (y-y') (x-x')

    {-# INLINE suppress #-}
    {-# INLINE spots #-}
{-# NOINLINE nonMaximaSuppress #-}

doubleThreshold :: Array U DIM2 (Int,Int) -> Array D DIM2 Word8
doubleThreshold img = img `deepSeqArray` img'
  where
    img' = R.map (thresh' 200 100) img

    thresh' upper lower (m,_)
        | m > upper     = 255
        | m > lower     = 128
        | otherwise     = 0

-- Simple gaussian function
gauss :: (Convertible f1 Float, Convertible f2 Float) =>
         f1 -> f2 -> Float -> Float
gauss (float->a) (float->b) sig = exp $ (-a*a-b*b)/(2*sig*sig)
{-# INLINE gauss #-}

inBounds :: Shape sh => Int -> Int -> sh -> Bool
inBounds w h (listOfShape->[x,y]) = x >= 0 && x < w && y >= 0 && y < h
inBounds _ _ _ = undefined
{-# INLINE inBounds #-}

------------------------- Conversion Functions --------------------------------
float :: Convertible a Float => a -> Float
word8 :: Convertible a Word8 => a -> Word8
int :: Convertible a Int => a -> Int

float = convert
word8 = convert
int = convert
{-# INLINE float #-}
{-# INLINE word8 #-}
{-# INLINE int #-}

instance Convertible a a where safeConvert = Right
