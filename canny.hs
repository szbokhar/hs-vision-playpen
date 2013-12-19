{-# LANGUAGE ViewPatterns,
             FlexibleContexts,
             FlexibleInstances,
             MultiParamTypeClasses,
             QuasiQuotes #-}

module Main where

import Prelude                  as P
import Data.Array.Repa          as R

import Control.Monad            ( when )
import Control.Monad.ST         ( runST, ST )
import Data.Array.Repa.Algorithms.Convolve
                                ( convolveP )
import Data.Array.Repa.IO.DevIL ( runIL, readImage, writeImage,
                                  Image(RGB, RGBA, Grey) )
import Data.Array.Repa.Stencil  ( Boundary(BoundConst) )
import Data.Array.Repa.Stencil.Dim2
                                ( stencil2, makeStencil2, mapStencil2)
import Data.Convertible         ( Convertible(safeConvert), convert )
import Data.Word                ( Word8 )
import System.Directory         ( doesFileExist, removeFile )
import System.Environment       ( getArgs )

-- |Main function
main :: IO ()
main = do
    -- Parse Arguments
    [filename] <- getArgs
    let newFilename = "new-" P.++ filename

    -- Run algorithm
    initialImage <- (runIL $ readImage filename)
    let finalImage = manipulateImage initialImage

    -- Write new file
    delete <- doesFileExist newFilename
    when delete $ removeFile newFilename
    runIL $ writeImage newFilename finalImage


-- |Runs the image algorithm
manipulateImage :: Image -> Image
manipulateImage image = runST $ do
    finalArray <- computeP (delay imageArray)   -- Prepare image
        >>= (computeP . grey)                   -- To Grayscale
        >>= (computeP . greyBlur 1 1 2)         -- Blur
        >>= (computeP . gradient)               -- Gradient Mag and Dir
        >>= (computeP . nonMaximaSuppress)      -- Thin Edges
        >>= (computeP . doubleThreshold)        -- Draw good edges
    return (Grey finalArray)
  where
    (_,imageArray) = sep image
    sep (RGB x) = x `deepSeqArray` (RGB, x)
    sep (RGBA x) = (RGBA, x)
    sep _ = undefined
{-# NOINLINE manipulateImage #-}


-- |Converts array to grayscale
grey :: Array U DIM3 Word8 -> Array D DIM2 Word8
grey img = img `deepSeqArray` img'
  where
    img' = traverse img (\(Z:.h:.w:._) -> ix2 h w) toGreyPixel
    toGreyPixel fn (Z :. y :. x) = word8 (0.2989*r + 0.5870*g + 0.1140*b)
      where [r,g,b] = P.map (float . fn . ix3 y x) [0,1,2]
    {-# INLINE toGreyPixel #-}
{-# NOINLINE grey #-}

-- Blurs a grayscale image
greyBlur :: Int -> Int -> Float -> Array U DIM2 Word8 -> Array D DIM2 Int
greyBlur kW kH sigma img = img `deepSeqArray` (delay img')
  where
    img' = runST $ do
        stencil <- computeP $ fromFunction (ix2 3 3) (g sigma)
        imgFloat <- computeP $ R.map float img
        blur <- convolveP (const 0) stencil imgFloat
        computeP $ R.map int blur :: ST a (Array U DIM2 Int)

    g sig (listOfShape->[x,y]) = gauss (x-(2*kW+1)) (y-(2*kH+1)) sig
    g _ _ = undefined
    {-# INLINE g #-}
{-# NOINLINE greyBlur #-}

-- |Computes the gradient magnitude and direction
gradient :: Array U DIM2 Int -> Array D DIM2 (Int, Int)
gradient img = img `deepSeqArray` img'
  where
    img' = R.zipWith magAndDir mapX mapY

    magAndDir dx dy = (abs dx + abs dy, theta dx dy)
    theta (float->dy) (float->dx) = round(4*rad/pi) `mod` 4
        where rad | r < 0       = r+pi
                  | otherwise   = r
                where r = atan2 dy dx

    mapX = mapStencil2 (BoundConst 0) kX img
    mapY = mapStencil2 (BoundConst 0) kY img
    kX = [stencil2| -1  0  1
                    -2  0  2
                    -1  0  1 |]
    kY = [stencil2| -1 -2 -1
                     0  0  0
                     1  2  1 |]

    {-# INLINE magAndDir #-}
    {-# INLINE theta #-}
    {-# INLINE kX #-}
    {-# INLINE kY #-}
{-# NOINLINE gradient #-}

-- |Computes the gradient magnitude and direction
nonMaximaSuppress :: Array U DIM2 (Int,Int) -> Array D DIM2 (Int, Int)
nonMaximaSuppress img = img `deepSeqArray` img'
  where
    img' = traverse img id suppress
    (Z :. h' :. w') = extent img

    suppress fn (listOfShape->[x,y])
        | dir c == 0    = suppressPoint $ mag c > mag n  && mag c > mag s
        | dir c == 1    = suppressPoint $ mag c > mag nw && mag c > mag se
        | dir c == 2    = suppressPoint $ mag c > mag w  && mag c > mag e
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
                | inBounds w' h' idx  = Just idx
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
    {-# INLINE thresh' #-}

-- |Simple gaussian function
gauss :: (Convertible f1 Float, Convertible f2 Float) =>
         f1 -> f2 -> Float -> Float
gauss (float->a) (float->b) sig = exp $ (-a*a-b*b)/(2*sig*sig)
{-# INLINE gauss #-}

inBounds :: Int -> Int -> DIM2 -> Bool
inBounds w h sh = inShape (ix2 h w) sh
{-# INLINE inBounds #-}

------------------------- Conversion Functions --------------------------------
instance Convertible a a where safeConvert = Right
float :: Convertible a Float => a -> Float
word8 :: Convertible a Word8 => a -> Word8
int :: Convertible a Int => a -> Int

float = convert
word8 = convert
int = convert
{-# INLINE float #-}
{-# INLINE word8 #-}
{-# INLINE int #-}
