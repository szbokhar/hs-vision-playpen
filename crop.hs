{-# LANGUAGE ViewPatterns,
             FlexibleInstances,
             FlexibleContexts,
             MultiParamTypeClasses,
             QuasiQuotes #-}
module Main where

import qualified Data.Array.Repa                as R
import qualified Data.Map                       as M
import qualified Data.Prizm.Color.CIE.LAB as C ( fromRGB )
import qualified Data.Prizm.Types         as C ( RGB(..), CIELAB(..) )

import Control.Monad            ( when )
import Control.Monad.ST         ( runST )
import Data.Array.Repa          ( DIM2, DIM3, D, U, Array, Z(..), (:.)(..) )
import Data.Array.Repa.IO.DevIL ( runIL, readImage, writeImage,
                                  Image(RGB, RGBA, Grey) )
import Data.Array.Repa.Shape    ( inShape )
import Data.Convertible         ( Convertible(safeConvert), convert )
import Data.List                ( intercalate, sort, foldl' )
import Data.Ord                 ( Down(..) )
import Data.Word                ( Word8 )
import System.Directory         ( doesFileExist, removeFile )
import System.Environment       ( getArgs )

import Debug.Trace

default (Int)

-- |Main function
main :: IO ()
main = do
    -- Parse Arguments
    [file] <- getArgs
    let newFilename = generateNewFilename "back-" file

    -- Run algorithm
    initialImage <- runIL $ readImage file
    let finalImage = manipulateImage initialImage

    -- Write new file
    delete <- doesFileExist newFilename
    when delete $ removeFile newFilename
    runIL $ writeImage newFilename finalImage


-- |Runs the image algorithm
manipulateImage ::  Image -> Image
manipulateImage  image = Grey finalArray
  where
    finalArray = runST $ R.computeP (R.delay imageArray)
        >>= (R.computeP . blur 1 1 2)
        >>= (R.computeP . cutDown)
        >>= (R.computeP . blur 1 1 2)
        >>= (R.computeP . cutDown)
        >>= (R.computeP . blur 1 1 2)
        >>= (R.computeP . quantize 10)
        >>= (R.computeP . subtractBackground
                            10
                            (runST $ R.computeP $ R.delay imageArray))
    (_,imageArray) = sep image
    sep (RGB x) = (RGB, x)
    sep (RGBA x) = (RGBA, x)
    sep _ = undefined
{-# NOINLINE manipulateImage #-}


-- Blurs a grayscale image
blur :: Int -> Int -> Float -> Array U DIM3 Word8 -> Array D DIM3 Word8
blur 0 0 _ img = R.delay img
blur kW kH sigma img = R.traverse img id blurPixelChannel
  where
    blurPixelChannel fn (Z :. y :. x :. c)  =
        word8 $ sum $ map (\(ix,coe) -> coe*(float $ fn ix)) blist
      where blist = [(R.ix3 (y-y') (x-x') c, g sigma (Z:.y':.x'))
                        | y' <- [-kH..kH], x' <- [-kW..kW]
                        , inShape (R.extent img) (Z:.(y-y'):.(x-x'):.c)]
    g sig (Z:.y:.x) = gauss sig x y / sum tot
    tot = [gauss sigma y' x' | y' <- [-kH..kH], x' <- [-kW..kW]]
    {-# INLINE g #-}
    {-# INLINE tot #-}
{-# NOINLINE blur #-}

cutDown :: Array U DIM3 Word8 -> Array D DIM3 Word8
cutDown img = R.traverse img quarter roundDown
  where quarter (Z :. h :. w :. c) = Z :. (div h 2) :. (div w 2) :. c
        roundDown fn (Z :. y :. x :. c) = fn (Z :. (2*y) :. (2*x) :. c)
{-# NOINLINE cutDown #-}

-- |Converts array to grayscale
quantize :: Word8 -> Array U DIM3 Word8 -> Array D DIM3 Word8
quantize numBuckets img =
    R.map (\a -> let (d,m) = (divMod a size) in d + boolInt (2*m >= size)) img
  where
    boolInt True = 1
    boolInt False = 0
    size = div 255 numBuckets
{-# NOINLINE quantize #-}

subtractBackground ::
    Word8 -> Array U DIM3 Word8 -> Array U DIM3 Word8 -> Array D DIM2 Word8
subtractBackground numBuckets img histData =
    traceShow backCol $ R.traverse img3Tup id colorDistance
  where
    makeTup fn ix = (fn (ix:.0), fn (ix:.1), fn (ix:.2))
    hist = foldl' (\mp val -> M.insertWith (+) val 1 mp) M.empty
         $ R.toList
         $ R.traverse histData (\(Z:.h:.w:._)->Z:.h:.w) makeTup
    img3Tup = R.traverse img (\(Z:.h:.w:._)->Z:.h:.w) makeTup
    backCol = (\(r,g,b) -> (r*size, g*size, b*size))
            . (\(Down a) -> snd a)
            . head
            . sort
            . map (\(a,b) -> Down (b,a))
            . M.toList $ hist
    colorDistance fn ix  = word8 $ 255 * distance (fn ix) backCol
    distance c1@(int_->r,int_->g,int_->b) c2@(int_->r',int_->g',int_->b') =
        (sqrt $ (l-l')**2 + (a-a')**2 + (b2-b2')**2) / 170
      where
        (C.CIELAB l a b2) = C.fromRGB (C.RGB r g b)
        (C.CIELAB l' a' b2') = C.fromRGB (C.RGB r' g' b')
    size = div 255 numBuckets
    {-# INLINE colorDistance #-}
{-# NOINLINE subtractBackground #-}

-- |Converts array to grayscale
grey :: Array U DIM3 Word8 -> Array D DIM2 Word8
grey img = img'
  where
    img' = R.traverse img (\(Z:.h:.w:._) -> R.ix2 h w) toGreyPixel
    toGreyPixel fn (Z :. y :. x) = word8 (0.2989*r + 0.5870*g + 0.1140*b)
      where [r,g,b] = map (float . fn . R.ix3 y x) [0,1,2]
    {-# INLINE toGreyPixel #-}
{-# NOINLINE grey #-}

------------------------ Some Utility Functions -------------------------------
-- |Simple gaussian function
gauss :: (Convertible f1 Float, Convertible f2 Float) =>
    Float -> f1 -> f2 -> Float
gauss sig (float->a) (float->b) = exp $ (-a*a-b*b)/(2*sig*sig)
{-# INLINE gauss #-}

-- |Check if a pair of indices are within bounds
inBounds :: Int -> Int -> DIM2 -> Bool
inBounds w h = R.inShape (R.ix2 h w)
{-# INLINE inBounds #-}

-- |Make a new filename by appending the prefix to the input filename
generateNewFilename :: String -> String -> String
generateNewFilename prefix filename = intercalate "/" $ path ++ [prefix ++ name]
  where parts = foldr (\a (x:xs) -> if a == '/' then []:x:xs else (a:x):xs)
                    [[]]
                    filename
        (path,name) = (init parts, last parts)


------------------------- Conversion Functions --------------------------------
instance Convertible a a where safeConvert = Right

float :: Convertible a Float => a -> Float
word8 :: Convertible a Word8 => a -> Word8
int :: Convertible a Int => a -> Int
int_ :: Convertible a Integer => a -> Integer

float = convert
word8 = convert
int = convert
int_ = convert
{-# INLINE float #-}
{-# INLINE word8 #-}
{-# INLINE int #-}
