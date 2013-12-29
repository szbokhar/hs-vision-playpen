{-# LANGUAGE ViewPatterns,
             FlexibleInstances,
             FlexibleContexts,
             MultiParamTypeClasses,
             QuasiQuotes #-}
module Main where

import qualified Data.Array.Repa                as R
import qualified Data.Vector.Unboxed            as V
import qualified Data.Vector.Unboxed.Mutable    as MV

import Control.Monad            ( when, foldM )
import Control.Monad.ST         ( runST )
import Data.Array.Repa          ( DIM2, DIM3, D, U, Array, Z(..), (:.)(..), (!) )
import Data.Array.Repa.Algorithms.Convolve
                                ( convolveP )
import Data.Array.Repa.IO.DevIL ( runIL, readImage, writeImage,
                                  Image(RGB, RGBA, Grey) )
import Data.Array.Repa.Stencil  ( Boundary(BoundConst) )
import Data.Array.Repa.Stencil.Dim2
                                ( stencil2, makeStencil2, mapStencil2)
import Data.Convertible         ( Convertible(safeConvert), convert )
import Data.List                ( intercalate )
import Data.Word                ( Word8 )
import System.Directory         ( doesFileExist, removeFile )
import System.Environment       ( getArgs )
import System.Exit              ( exitSuccess )

import Debug.Trace

-- |Usage Message
usageMessage :: String
usageMessage = "canny fname [-b kW kH] [-s kS] [-t u l]\n"
            ++ "    fname   - Input filename\n"
            ++ "    kW      - Blur kernel width (default 1)\n"
            ++ "    kH      - Blur kernel height (default 1)\n"
            ++ "    kS      - Blur kernel sigma (default 2)\n"
            ++ "    u       - Upper threshold (default 300)\n"
            ++ "    l       - Lower threshold (default 200)\n"

-- |Main function
main :: IO ()
main = do
    -- Parse Arguments
    args <- getArgs
    let (Opts file
              (blurX,blurY) blurSig
              (uThresh,lThresh)
              help) = getOptions args
        newFilename = generateNewFilename "edges-" file

    when help $ do
        putStrLn usageMessage
        exitSuccess

    -- Run algorithm
    initialImage <- runIL $ readImage file
    let blur = (blurX, blurY, blurSig)
        thresh = (uThresh, lThresh)
        finalImage = manipulateImage blur thresh initialImage

    -- Write new file
    delete <- doesFileExist newFilename
    when delete $ removeFile newFilename
    runIL $ writeImage newFilename finalImage


-- |Runs the image algorithm
manipulateImage :: (Int, Int, Float) -> (Int, Int) -> Image -> Image
manipulateImage (kW, kH, sig) (l,u) image = runST $ do
    finalArray <- R.computeP (R.delay imageArray)       -- Prepare image
        >>= (R.computeP . grey)                         -- To Grayscale
        >>= (R.computeP . greyBlur kW kH sig)           -- Blur
        >>= (R.computeP . gradient)                     -- Gradient Mag and Dir
        >>= (R.computeP . nonMaximaSuppress)            -- Thin Edges
        >>= (R.computeP . doubleThreshold l u)          -- Draw good edges
    return (Grey finalArray)
  where
    (_,imageArray) = sep image
    sep (RGB x) = (RGB, x)
    sep (RGBA x) = (RGBA, x)
    sep _ = undefined
{-# NOINLINE manipulateImage #-}


-- |Converts array to grayscale
grey :: Array U DIM3 Word8 -> Array D DIM2 Word8
grey img = img'
  where
    img' = R.traverse img (\(Z:.h:.w:._) -> R.ix2 h w) toGreyPixel
    toGreyPixel fn (Z :. y :. x) = word8 (0.2989*r + 0.5870*g + 0.1140*b)
      where [r,g,b] = map (float . fn . R.ix3 y x) [0,1,2]
    {-# INLINE toGreyPixel #-}
{-# NOINLINE grey #-}

-- Blurs a grayscale image
greyBlur :: Int -> Int -> Float -> Array U DIM2 Word8 -> Array D DIM2 Int
greyBlur 0 0 _ img = R.map int img
greyBlur kW kH sigma img = runST $ do
    stencil <- R.computeP $ R.fromFunction (R.ix2 (2*kH+1) (2*kW+1)) (g sigma)
    imgFloat <- R.computeP $ R.map float img
    blur <- convolveP (const 0) stencil imgFloat
    return (R.map int blur)
  where
    g sig (Z:.y:.x) = gauss sig (x-kW) (y-kH) / tot
    tot = sum [gauss sigma y' x' | y' <- [-kH..kH], x' <- [-kW..kW]]
    {-# INLINE g #-}
    {-# INLINE tot #-}
{-# NOINLINE greyBlur #-}

-- |Computes the gradient magnitude and direction
gradient :: Array U DIM2 Int -> Array D DIM2 (Magnitude, Direction)
gradient img = R.zipWith magAndDir mapX mapY
  where
    magAndDir dx dy = (abs dx + abs dy, theta dx dy)
    theta (float->dy) (float->dx) = round(4*rad/pi) `mod` 4
        where rad | r < 0       = r+pi
                  | otherwise   = r
              r = atan2 dy dx

    mapX = mapStencil2 (BoundConst 0) kX img
    kX = [stencil2|  1  0 -1
                     2  0 -2
                     1  0 -1 |]
    mapY = mapStencil2 (BoundConst 0) kY img
    kY = [stencil2|  1  2  1
                     0  0  0
                    -1 -2 -1 |]

    {-# INLINE magAndDir #-}
    {-# INLINE theta #-}
    {-# INLINE kX #-}
    {-# INLINE kY #-}
{-# NOINLINE gradient #-}

-- |Computes the gradient magnitude and direction
nonMaximaSuppress :: Array U DIM2 (Magnitude, Direction)
                  -> Array D DIM2 (Magnitude, Direction)
nonMaximaSuppress img = R.traverse img id suppress
  where
    (Z :. h' :. w') = R.extent img

    suppress :: (DIM2 -> (Int,Int)) -> DIM2 -> (Int, Int)
    suppress fn (Z:.y:.x)
        | dir c == _NS      = keepIfPeak n s
        | dir c == _NWSE    = keepIfPeak nw se
        | dir c == _WE      = keepIfPeak w e
        | dir c == _SWNE    = keepIfPeak sw ne
      where [nw,n,ne,w,c,e,sw,s,se] = spots x y
            mag = maybe 0 (fst . fn)
            dir = maybe (error "This is wrong") (snd . fn)
            keepIfPeak p q
                | mag c > mag p && mag c > mag q    = (mag c, dir c)
                | otherwise                         = (0,0)
            {-# INLINE keepIfPeak #-}
    suppress _ _ = undefined

    spots x y = map toMaybeIndex
                      [ (-1,-1), ( 0,-1), ( 1,-1)
                      , (-1, 0), ( 0, 0), ( 1, 0)
                      , (-1, 1), ( 0, 1), ( 1, 1) ]
      where toMaybeIndex (x',y')
                | inBounds w' h' idx  = Just idx
                | otherwise         = Nothing
              where idx = R.ix2 (y-y') (x-x')

    {-# INLINE suppress #-}
    {-# INLINE spots #-}
{-# NOINLINE nonMaximaSuppress #-}

-- |Apply double thresholding on image. Only keep strong edges (>upper)
--  and weak edges (>lower) if they are connected to a strong edge.
doubleThreshold :: Int -> Int -> Array U DIM2 (Magnitude,Direction)
                -> Array D DIM2 Word8
doubleThreshold upper lower img = R.delay $ R.fromUnboxed imageSize finalVector
  where
    finalVector = runST $ do
        mVector <- MV.replicate (R.size imageSize) 0    -- Black image vector
        floodWeakEdges mVector (strongEdgePositions img)-- Fill in true edges
        V.freeze mVector                                -- return edges image

    -- DFS from all strong edges to find connected weak edges. Use a stack
    -- of pixels to consider
    floodWeakEdges _ [] = return ()
    floodWeakEdges vec ((x,y):xs) = do
        MV.write vec (pixelAt x y) 255  -- Paint pixel at top of stack white

        -- Get all weak edge neighbours
        top <- foldM (\ys pos@(x',y') -> do
            let (mag,_) = img ! R.ix2 y' x'     -- Gradient magnitude at pos
            pixel <- MV.read vec (pixelAt x' y')-- Current pixel color
            if weakEdge mag && notWhite pixel
                then return (pos:ys)
                else return ys
            ) [] adjacentCells

        -- Recurse with weak neighbours pushed onto stack
        floodWeakEdges vec (top++xs)
        return ()
      where weakEdge val = upper > val && val > lower
            notWhite = (/=255)
            adjacentCells = [(x',y')
                | x'<-[x-1..x+1], y'<-[y-1..y+1]    -- Surrounding positions
                , R.inShape imageSize (Z:.y':.x')    -- Not out of bounds
                , (x,y) /= (x',y')]                 -- Not the center

    -- List of positions of all strong edges
    strongEdgePositions image = [ i | (i,(gMag,_)) <- pixelList, gMag > upper]
      where pixelList = V.toList . V.imap setToIndex . R.toUnboxed $ image
            setToIndex i e = ((x,y),e)
              where (Z :. y :. x) = R.fromIndex imageSize i

    imageSize = R.extent img
    pixelAt x y = R.toIndex imageSize $ R.ix2 y x
    {-# INLINE imageSize #-}
    {-# INLINE pixelAt #-}
{-# NOINLINE doubleThreshold #-}


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
  where parts = foldr (\a (x:xs) -> if a == '/' then []:x:xs else (a:x):xs) [[]] filename
        (path,name) = (init parts, last parts)


--------------------------- Argument Parsing ----------------------------------
data Options = Opts {
    fname       :: String
  , blurSize    :: (Int, Int)
  , blurSigma   :: Float
  , thresholds  :: (Int, Int)
  , displayHelp :: Bool
  }

defaultOptions :: Options
defaultOptions = Opts {
    fname = error "Please specify an input image filename"
  , blurSize = (1,1)
  , blurSigma = 2
  , thresholds = (300, 200)
  , displayHelp = True
  }

getOptions :: [String] -> Options
getOptions args = parse args defaultOptions
  where
    parse [] opts = opts
    parse ("-h":xs) opts = parse xs $ opts { displayHelp = True }
    parse ("-b":x:y:xs) opts = parse xs $ opts { blurSize = (read x, read y) }
    parse ("-s":sig:xs) opts = parse xs $ opts { blurSigma = read sig }
    parse ("-t":u:l:xs) opts = parse xs $ opts { thresholds = (read u, read l) }
    parse (o@('-':_):_) opts = error $ "Unrecognized option " ++ o
    parse (x:xs) opts = parse xs $ opts { fname = x, displayHelp = False }


---------------------------- Useful Aliases -----------------------------------
_NS, _NWSE, _WE, _SWNE :: Int
(_NS, _NWSE, _WE, _SWNE) = (0,1,2,3)

type Magnitude  = Int
type Direction  = Int

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
