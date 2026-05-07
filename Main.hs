{-# LANGUAGE OverloadedStrings #-}

module Main where

import Codec.Picture
import Control.Monad (foldM, forM_)
import Control.Parallel.Strategies (rseq, using, parList)
import Data.Bits ((.&.), (.|.), xor, shiftR)
import Data.Either (lefts, rights)
import Data.List (break, foldl', intercalate, isPrefixOf)
import qualified Data.Vector.Storable as V
import Data.Word (Word64, Word8)
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import System.Environment (getArgs)
import System.Random (StdGen, newStdGen, randomR)
import Text.Parsec
import Text.Parsec.String (Parser)

-- =============================================================================
-- AST Data Types
-- =============================================================================

data PixelValue
    = Scalar Double
    | Color Double Double Double
    deriving (Show, Eq)

data Expr
    = EConst Double
    | EVec Double Double Double
    | EVar VarName
    | EAdd Expr Expr
    | ESub Expr Expr
    | EMult Expr Expr
    | EDiv Expr Expr
    | EMod Expr Expr
    | EAnd Expr Expr
    | EOr Expr Expr
    | EXor Expr Expr
    | EPolarCoords Expr Expr
    | EAbs Expr
    | ESin Expr
    | ECos Expr
    | EAtan Expr Expr
    | ELog Expr
    | ERound Expr
    | EExpt Expr Expr
    | EMin Expr Expr
    | EMax Expr Expr
    | EMandelbrot Expr Expr
    | EJulia Expr Expr Expr
    | ENewton Expr Expr
    | EIfs Expr Expr Expr Expr
    | EBwNoise Expr Expr
    | EColorNoise Expr Expr
    | EHsvToRgb Expr Expr Expr
    | EWarpedBwNoise Expr Expr Expr Expr
    | EWarpedColorNoise Expr Expr Expr Expr
    | EBlur Expr
    | EBandPass Expr
    | EGradMag Expr
    | EGradDir Expr
    | EBump Expr Expr Expr Expr
    deriving (Show, Eq)

data VarName = X | Y deriving (Show, Eq)

data Genotype = Genotype String Expr deriving (Show)
type Population = [Genotype]

data Dictionary = Dictionary [String] [String] [String]

-- =============================================================================
-- CLI Configuration
-- =============================================================================

data Command = Generate | Breed | Render deriving (Show, Eq)

data Config = Config
    { cmd      :: Command
    , depth    :: Int
    , mutation :: Int
    , genSize  :: Int
    , width    :: Int
    , height   :: Int
    , aa       :: Bool
    , dictFile :: String
    , srcFiles :: [String]
    } deriving (Show)

defaultConfig :: Config
defaultConfig = Config
    { cmd      = Generate
    , depth    = 3
    , mutation = 5
    , genSize  = 10
    , width    = 256
    , height   = 256
    , aa       = False
    , dictFile = "dictionary.txt"
    , srcFiles = []
    }

-- =============================================================================
-- S-Expression Parser
-- =============================================================================

data SExpr = SAtom String | SList [SExpr] deriving (Show, Eq)

parseSExpr :: Parser SExpr
parseSExpr = spaces *> parseSExpr'

parseSExpr' :: Parser SExpr
parseSExpr' = parseList <|> parseAtom

parseAtom :: Parser SExpr
parseAtom = do
    s <- many1 (noneOf " \t\n\r()")
    return (SAtom s)

parseList :: Parser SExpr
parseList = between (char '(') (char ')') $ do
    spaces
    exprs <- sepEndBy parseSExpr' spaces
    return (SList exprs)

-- =============================================================================
-- S-Expression to AST Conversion
-- =============================================================================

toExpr :: SExpr -> Either String Expr
toExpr (SAtom s) = parseAtom' s
toExpr (SList [SAtom fn]) = parseUnary fn
toExpr (SList [SAtom fn, a]) = parseBinary1 fn a
toExpr (SList [SAtom fn, a, b]) = parseBinary2 fn a b
toExpr (SList [SAtom fn, a, b, c]) = parseTernary fn a b c
toExpr (SList [SAtom fn, a, b, c, d]) = parseQuaternary fn a b c d
toExpr (SList xs) = Left $ "Invalid expression form: " ++ show xs

parseAtom' :: String -> Either String Expr
parseAtom' s
    | isNumber s   = Right (EConst (read s))
    | s == "X"     = Right (EVar X)
    | s == "Y"     = Right (EVar Y)
    | otherwise    = Left $ "Unknown atom: " ++ s

isNumber :: String -> Bool
isNumber s = case reads s :: [(Double, String)] of
    [(_, "")] -> True
    _         -> False

parseDouble :: SExpr -> Either String Double
parseDouble (SAtom s) = 
    case reads s :: [(Double, String)] of
        [(d, "")] -> Right d
        _ -> Left $ "Expected number, got: " ++ s
parseDouble s = Left $ "Expected number atom, got: " ++ show s

parseUnary :: String -> Either String Expr
parseUnary "abs"      = Left "abs requires 1 argument"
parseUnary "sin"      = Left "sin requires 1 argument"
parseUnary "cos"      = Left "cos requires 1 argument"
parseUnary "log"      = Left "log requires 1 argument"
parseUnary "round"    = Left "round requires 1 argument"
parseUnary "blur"     = Left "blur requires 1 argument"
parseUnary "band-pass" = Left "band-pass requires 1 argument"
parseUnary "grad-mag" = Left "grad-mag requires 1 argument"
parseUnary "grad-dir" = Left "grad-dir requires 1 argument"
parseUnary fn         = Left $ "Unknown function: " ++ fn

parseBinary1 :: String -> SExpr -> Either String Expr
parseBinary1 "abs"      a = EAbs   <$> toExpr a
parseBinary1 "sin"      a = ESin   <$> toExpr a
parseBinary1 "cos"      a = ECos   <$> toExpr a
parseBinary1 "log"      a = ELog   <$> toExpr a
parseBinary1 "round"    a = ERound <$> toExpr a
parseBinary1 "blur"     a = EBlur  <$> toExpr a
parseBinary1 "band-pass" a = EBandPass <$> toExpr a
parseBinary1 "grad-mag" a = EGradMag <$> toExpr a
parseBinary1 "grad-dir" a = EGradDir <$> toExpr a
parseBinary1 fn _         = Left $ "Unknown function: " ++ fn

parseBinary2 :: String -> SExpr -> SExpr -> Either String Expr
parseBinary2 "add" a b         = EAdd <$> toExpr a <*> toExpr b
parseBinary2 "sub" a b         = ESub <$> toExpr a <*> toExpr b
parseBinary2 "mult" a b        = EMult <$> toExpr a <*> toExpr b
parseBinary2 "div" a b         = EDiv <$> toExpr a <*> toExpr b
parseBinary2 "mod" a b         = EMod <$> toExpr a <*> toExpr b
parseBinary2 "and" a b         = EAnd <$> toExpr a <*> toExpr b
parseBinary2 "or" a b          = EOr <$> toExpr a <*> toExpr b
parseBinary2 "xor" a b         = EXor <$> toExpr a <*> toExpr b
parseBinary2 "polar-coords" a b = EPolarCoords <$> toExpr a <*> toExpr b
parseBinary2 "atan" a b        = EAtan <$> toExpr a <*> toExpr b
parseBinary2 "expt" a b        = EExpt <$> toExpr a <*> toExpr b
parseBinary2 "min" a b         = EMin <$> toExpr a <*> toExpr b
parseBinary2 "max" a b         = EMax <$> toExpr a <*> toExpr b
parseBinary2 "mandelbrot" a b  = EMandelbrot <$> toExpr a <*> toExpr b
parseBinary2 "newton" a b      = ENewton <$> toExpr a <*> toExpr b
parseBinary2 "bw-noise" a b    = EBwNoise <$> toExpr a <*> toExpr b
parseBinary2 "color-noise" a b = EColorNoise <$> toExpr a <*> toExpr b
parseBinary2 fn _ _            = Left $ "Unknown function: " ++ fn

parseTernary :: String -> SExpr -> SExpr -> SExpr -> Either String Expr
parseTernary "vec" a b c   = EVec <$> parseDouble a <*> parseDouble b <*> parseDouble c
parseTernary "julia" a b c = EJulia <$> toExpr a <*> toExpr b <*> toExpr c
parseTernary "hsv-to-rgb" a b c = EHsvToRgb <$> toExpr a <*> toExpr b <*> toExpr c
parseTernary fn _ _ _       = Left $ "Unknown function: " ++ fn

parseQuaternary :: String -> SExpr -> SExpr -> SExpr -> SExpr -> Either String Expr
parseQuaternary "warped-bw-noise" a b c d = EWarpedBwNoise <$> toExpr a <*> toExpr b <*> toExpr c <*> toExpr d
parseQuaternary "warped-color-noise" a b c d = EWarpedColorNoise <$> toExpr a <*> toExpr b <*> toExpr c <*> toExpr d
parseQuaternary "bump" a b c d = EBump <$> toExpr a <*> toExpr b <*> toExpr c <*> toExpr d
parseQuaternary "ifs" a b c d = EIfs <$> toExpr a <*> toExpr b <*> toExpr c <*> toExpr d
parseQuaternary fn _ _ _ _       = Left $ "Unknown function: " ++ fn

-- =============================================================================
-- Expression to S-Expression (for serialization)
-- =============================================================================

fromExpr :: Expr -> SExpr
fromExpr (EConst d)         = SAtom (show d)
fromExpr (EVec r g b)       = SList [SAtom "vec", SAtom (show r), SAtom (show g), SAtom (show b)]
fromExpr (EVar X)           = SAtom "X"
fromExpr (EVar Y)           = SAtom "Y"
fromExpr (EAdd a b)         = SList [SAtom "add", fromExpr a, fromExpr b]
fromExpr (ESub a b)         = SList [SAtom "sub", fromExpr a, fromExpr b]
fromExpr (EMult a b)        = SList [SAtom "mult", fromExpr a, fromExpr b]
fromExpr (EDiv a b)         = SList [SAtom "div", fromExpr a, fromExpr b]
fromExpr (EMod a b)         = SList [SAtom "mod", fromExpr a, fromExpr b]
fromExpr (EAnd a b)         = SList [SAtom "and", fromExpr a, fromExpr b]
fromExpr (EOr a b)          = SList [SAtom "or", fromExpr a, fromExpr b]
fromExpr (EXor a b)         = SList [SAtom "xor", fromExpr a, fromExpr b]
fromExpr (EPolarCoords a b) = SList [SAtom "polar-coords", fromExpr a, fromExpr b]
fromExpr (EAbs a)           = SList [SAtom "abs", fromExpr a]
fromExpr (ESin a)           = SList [SAtom "sin", fromExpr a]
fromExpr (ECos a)           = SList [SAtom "cos", fromExpr a]
fromExpr (EAtan a b)        = SList [SAtom "atan", fromExpr a, fromExpr b]
fromExpr (ELog a)           = SList [SAtom "log", fromExpr a]
fromExpr (ERound a)         = SList [SAtom "round", fromExpr a]
fromExpr (EExpt a b)        = SList [SAtom "expt", fromExpr a, fromExpr b]
fromExpr (EMin a b)         = SList [SAtom "min", fromExpr a, fromExpr b]
fromExpr (EMax a b)         = SList [SAtom "max", fromExpr a, fromExpr b]
fromExpr (EMandelbrot a b)  = SList [SAtom "mandelbrot", fromExpr a, fromExpr b]
fromExpr (EJulia a b c)     = SList [SAtom "julia", fromExpr a, fromExpr b, fromExpr c]
fromExpr (ENewton a b)      = SList [SAtom "newton", fromExpr a, fromExpr b]
fromExpr (EIfs a b c d)     = SList [SAtom "ifs", fromExpr a, fromExpr b, fromExpr c, fromExpr d]
fromExpr (EBwNoise a b)     = SList [SAtom "bw-noise", fromExpr a, fromExpr b]
fromExpr (EColorNoise a b)  = SList [SAtom "color-noise", fromExpr a, fromExpr b]
fromExpr (EHsvToRgb a b c)  = SList [SAtom "hsv-to-rgb", fromExpr a, fromExpr b, fromExpr c]
fromExpr (EWarpedBwNoise a b c d) = SList [SAtom "warped-bw-noise", fromExpr a, fromExpr b, fromExpr c, fromExpr d]
fromExpr (EWarpedColorNoise a b c d) = SList [SAtom "warped-color-noise", fromExpr a, fromExpr b, fromExpr c, fromExpr d]
fromExpr (EBlur a)          = SList [SAtom "blur", fromExpr a]
fromExpr (EBandPass a)      = SList [SAtom "band-pass", fromExpr a]
fromExpr (EGradMag a)       = SList [SAtom "grad-mag", fromExpr a]
fromExpr (EGradDir a)       = SList [SAtom "grad-dir", fromExpr a]
fromExpr (EBump a b c d)    = SList [SAtom "bump", fromExpr a, fromExpr b, fromExpr c, fromExpr d]

showSExpr :: SExpr -> String
showSExpr (SAtom s) = s
showSExpr (SList xs) = "(" ++ unwords (map showSExpr xs) ++ ")"

showExpr :: Expr -> String
showExpr = showSExpr . fromExpr

showGenotype :: Genotype -> String
showGenotype (Genotype name body) = "(define (" ++ name ++ ")\n  " ++ showExpr body ++ ")"

-- =============================================================================
-- File I/O & Dictionary Parsing
-- =============================================================================

parseGenotype :: Parser Genotype
parseGenotype = do
    _ <- char '('
    spaces
    _ <- string "define"
    spaces
    _ <- char '('
    spaces
    name <- many1 (noneOf " \t\n\r)")
    spaces
    _ <- char ')'
    spaces
    bodyExpr <- parseSExpr'
    spaces
    _ <- char ')'
    spaces
    case toExpr bodyExpr of
        Left err -> fail $ "Invalid body in " ++ name ++ ": " ++ err
        Right expr -> return (Genotype name expr)

parseGenotypes :: Parser [Genotype]
parseGenotypes = spaces *> sepEndBy parseGenotype spaces <* eof

writePopulation :: FilePath -> Population -> IO ()
writePopulation path pop = writeFile path (intercalate "\n\n" $ map showGenotype pop)

readPopulation :: FilePath -> IO (Either String Population)
readPopulation path = do
    contents <- readFile path
    return $ case parse parseGenotypes "" contents of
        Left err -> Left (show err)
        Right genotypes -> Right genotypes

readDictionary :: FilePath -> IO (Either String Dictionary)
readDictionary path = do
    contents <- readFile path
    return $ parseDict contents

parseDict :: String -> Either String Dictionary
parseDict contents = 
    let ls = lines contents
        (h1, r1) = break ("[ADJECTIVES2]" `isPrefixOf`) ls
        (h2, r2) = break ("[NOUNS]" `isPrefixOf`) r1
        getWords section = map (head . words) $ filter (not . null . words) $ tail section
        a1 = getWords h1
        a2 = getWords h2
        ns = getWords r2
    in if null a1 || null a2 || null ns
       then Left "Dictionary missing required sections: [ADJECTIVES1], [ADJECTIVES2], [NOUNS]"
       else Right (Dictionary a1 a2 ns)

-- =============================================================================
-- Pixel Value Operations
-- =============================================================================

binaryOp :: (Double -> Double -> Double) -> PixelValue -> PixelValue -> PixelValue
binaryOp f (Scalar a) (Scalar b) = Scalar (f a b)
binaryOp f (Color ar ag ab) (Color br bg bb) = Color (f ar br) (f ag bg) (f ab bb)
binaryOp f (Scalar a) (Color r g b) = Color (f a r) (f a g) (f a b)
binaryOp f (Color r g b) (Scalar a) = Color (f r a) (f g a) (f b a)

unaryOp :: (Double -> Double) -> PixelValue -> PixelValue
unaryOp f (Scalar a) = Scalar (f a)
unaryOp f (Color r g b) = Color (f r) (f g) (f b)

safeDiv :: Double -> Double -> Double
safeDiv _ 0 = 0
safeDiv a b = a / b

safeMod :: Double -> Double -> Double
safeMod _ 0 = 0
safeMod a b = a - b * fromInteger (floor (a / b) :: Integer)

doubleBitOp :: (Word64 -> Word64 -> Word64) -> Double -> Double -> Double
doubleBitOp f a b = 
  let w = f (castDoubleToWord64 a) (castDoubleToWord64 b)
      mantissa = w .&. 0x000FFFFFFFFFFFFF
      topBits = mantissa `shiftR` 36
  in fromIntegral topBits / 65535.0

extractScalar :: PixelValue -> Double
extractScalar (Scalar d) = d
extractScalar (Color r _ _) = r

-- =============================================================================
-- Noise Generation
-- =============================================================================

fade :: Double -> Double
fade t = t * t * t * (t * (t * 6 - 15) + 10)

lerp :: Double -> Double -> Double -> Double
lerp t a b = a + t * (b - a)

hashXY :: Int -> Int -> Int -> Int
hashXY x y seed = 
  let h = (seed * 374761393 + x * 668265263 + y * 1274126177) .&. 0x7FFFFFFF
      h1 = h `xor` ((h `shiftR` 13) .&. 0x7FFFFFFF)
      h2 = (h1 * 1274126177) .&. 0x7FFFFFFF
  in h2 `xor` ((h2 `shiftR` 16) .&. 0x7FFFFFFF)

grad2D :: Int -> Double -> Double -> Double
grad2D h x y = case h .&. 7 of
    0 ->  x + y; 1 -> -x + y; 2 ->  x - y; 3 -> -x - y
    4 ->  x;      5 -> -x;      6 ->  y;      7 -> -y
    _ -> 0

noise2D :: Double -> Double -> Int -> Double
noise2D x y seed = 
  let xi = floor x; yi = floor y
      xf = x - fromIntegral xi; yf = y - fromIntegral yi
      u = fade xf; v = fade yf
      aa = hashXY xi yi seed; ab = hashXY xi (yi+1) seed
      ba = hashXY (xi+1) yi seed; bb = hashXY (xi+1) (yi+1) seed
      x1 = lerp u (grad2D aa xf yf) (grad2D ba (xf-1) yf)
      x2 = lerp u (grad2D ab xf (yf-1)) (grad2D bb (xf-1) (yf-1))
  in (lerp v x1 x2 + 1) / 2

colorNoise2D :: Double -> Double -> Int -> (Double, Double, Double)
colorNoise2D x y seed = (noise2D x y seed, noise2D x y (seed + 1000), noise2D x y (seed + 2000))

-- =============================================================================
-- Fractal Generation
-- =============================================================================

maxFractalIter :: Int
maxFractalIter = 50

mandelbrotIter :: Double -> Double -> Double -> Double -> Int
mandelbrotIter zxr zyr cxr cyr = go zxr zyr 0
  where go zxr zyr i | i >= maxFractalIter = maxFractalIter
                     | zxr*zxr + zyr*zyr > 4 = i
                     | otherwise = let newZxr = zxr*zxr - zyr*zyr + cxr
                                       newZyr = 2*zxr*zyr + cyr
                                   in go newZxr newZyr (i + 1)

juliaIter :: Double -> Double -> Double -> Double -> Int
juliaIter zxr zyr cr ci = go zxr zyr 0
  where go zxr zyr i | i >= maxFractalIter = maxFractalIter
                     | zxr*zxr + zyr*zyr > 4 = i
                     | otherwise = let newZxr = zxr*zxr - zyr*zyr + cr
                                       newZyr = 2*zxr*zyr + ci
                                   in go newZxr newZyr (i + 1)

newtonIter :: Double -> Double -> (Double, Double, Double)
newtonIter initZx initZy = 
    let (finalZx, finalZy, iters) = go initZx initZy 0
        r1x = 1.0; r1y = 0.0
        r2x = -0.5; r2y = 0.86602540378
        r3x = -0.5; r3y = -0.86602540378
        d1 = distSq finalZx finalZy r1x r1y
        d2 = distSq finalZx finalZy r2x r2y
        d3 = distSq finalZx finalZy r3x r3y
        (cr, cg, cb) = if d1 < d2 && d1 < d3 then (1.0, 0.2, 0.2)
                       else if d2 < d3 then (0.2, 1.0, 0.2)
                       else (0.2, 0.2, 1.0)
        shade = 1.0 - (fromIntegral iters / fromIntegral maxFractalIter) * 0.8
    in (cr * shade, cg * shade, cb * shade)
  where go zxr zyr i | i >= maxFractalIter = (zxr, zyr, i)
                     | otherwise = let zr2 = zxr * zxr - zyr * zyr
                                       zi2 = 2 * zxr * zyr
                                       fr = zr2 * zxr - zi2 * zyr - 1
                                       fi = zr2 * zyr + zi2 * zxr
                                       dr = 3 * zr2; di = 3 * zi2
                                       denom = dr * dr + di * di
                                       newZxr = zxr - (fr * dr + fi * di) / denom
                                       newZyr = zyr - (fi * dr - fr * di) / denom
                                   in if distSq zxr zyr newZxr newZyr < 1e-6 
                                      then (newZxr, newZyr, i) 
                                      else go newZxr newZyr (i + 1)
        distSq x1 y1 x2 y2 = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)

ifsIter :: Double -> Double -> Double -> Double -> Double -> Double -> Int -> Double
ifsIter x y a b c d 0 = normalizeSinCos (sin (x * 10.0) + cos (y * 10.0))
ifsIter x y a b c d n =
    let ax = abs x
        ay = abs y
        (fx, fy) = if ax > ay then (ay, ax) else (ax, ay)
        nx = a * fx - b * fy + c
        ny = b * fx + a * fy + d
        cx = max (-10.0) (min 10.0 nx)
        cy = max (-10.0) (min 10.0 ny)
    in ifsIter cx cy a b c d (n - 1)

-- =============================================================================
-- Safe Math & Color Space Helpers
-- =============================================================================

normalizeSinCos :: Double -> Double
normalizeSinCos x = (x + 1.0) / 2.0

normalizeAtan :: Double -> Double
normalizeAtan x = (x + pi) / (2.0 * pi)

safeLog :: Double -> Double
safeLog x
    | x <= 0    = 0
    | x == 1    = 0
    | otherwise = log x

safeExpt :: Double -> Double -> Double
safeExpt base exp
    | base == 0 && exp > 0 = 0
    | base < 0             = safeExptPositive (abs base) exp
    | otherwise            = safeExptPositive base exp

safeExptPositive :: Double -> Double -> Double
safeExptPositive base exp
    | result >= 1e10 = 1e10
    | result <= -1e10 = -1e10
    | isNaN result || isInfinite result = 0
    | otherwise = result
  where
    result = base ** exp

hsvToRgb :: Double -> Double -> Double -> (Double, Double, Double)
hsvToRgb h s v
    | s == 0    = (v, v, v)
    | otherwise = let h' = h * 6.0
                      i = floor h' :: Int
                      f = h' - fromIntegral i
                      p = v * (1.0 - s)
                      q = v * (1.0 - s * f)
                      t = v * (1.0 - s * (1.0 - f))
                  in case i `mod` 6 of
                       0 -> (v, t, p)
                       1 -> (q, v, p)
                       2 -> (p, v, t)
                       3 -> (p, q, v)
                       4 -> (t, p, v)
                       5 -> (v, p, q)
                       _ -> (v, v, v)

-- =============================================================================
-- Image Processing & Analytical Math Helpers
-- =============================================================================

fdDelta :: Double
fdDelta = 0.005

evalSafe :: Expr -> Double -> Double -> PixelValue
evalSafe e x y = case evalExpr e x y of
    Just v -> v
    Nothing -> Scalar 0.0

analyticalBlur :: Expr -> Double -> Double -> PixelValue
analyticalBlur e x y = 
    let vals = [ evalSafe e (x + dx') (y + dy') | dx' <- [-fdDelta, 0, fdDelta], dy' <- [-fdDelta, 0, fdDelta] ]
        sumPV = foldl' (binaryOp (+)) (Scalar 0) vals
    in binaryOp safeDiv sumPV (Scalar 9.0)

-- =============================================================================
-- Expression Evaluator
-- =============================================================================

evalExpr :: Expr -> Double -> Double -> Maybe PixelValue
evalExpr (EVar X) x _     = Just (Scalar x)
evalExpr (EVar Y) _ y     = Just (Scalar y)
evalExpr (EConst d) _ _   = Just (Scalar d)
evalExpr (EVec r g b) _ _ = Just (Color r g b)
evalExpr (EAdd a b) x y = liftBinary (+) a b x y
evalExpr (ESub a b) x y = liftBinary (-) a b x y
evalExpr (EMult a b) x y = liftBinary (*) a b x y
evalExpr (EDiv a b) x y = liftBinary safeDiv a b x y
evalExpr (EMod a b) x y = liftBinary safeMod a b x y
evalExpr (EAbs a) x y = do va <- evalExpr a x y; Just (unaryOp abs va)
evalExpr (ESin a) x y = do va <- evalExpr a x y; Just (unaryOp (normalizeSinCos . sin) va)
evalExpr (ECos a) x y = do va <- evalExpr a x y; Just (unaryOp (normalizeSinCos . cos) va)
evalExpr (EAtan a b) x y = liftBinary (\ya xa -> normalizeAtan (atan2 ya xa)) a b x y
evalExpr (ELog a) x y = do va <- evalExpr a x y; Just (unaryOp safeLog va)
evalExpr (ERound a) x y = do va <- evalExpr a x y; Just (unaryOp (\v -> fromIntegral (round v :: Integer)) va)
evalExpr (EExpt a b) x y = liftBinary safeExpt a b x y
evalExpr (EMin a b) x y = liftBinary min a b x y
evalExpr (EMax a b) x y = liftBinary max a b x y
evalExpr (EAnd a b) x y = liftBinary (doubleBitOp (.&.)) a b x y
evalExpr (EOr a b) x y  = liftBinary (doubleBitOp (.|.)) a b x y
evalExpr (EXor a b) x y = liftBinary (doubleBitOp xor) a b x y
evalExpr (EPolarCoords xExpr yExpr) x y = do
    vx <- evalExpr xExpr x y; vy <- evalExpr yExpr x y
    let px = extractScalar vx; py = extractScalar vy
        r = sqrt (px * px + py * py); theta = atan2 py px
    Just (Color r ((theta + pi) / (2 * pi)) 0)
evalExpr (EBwNoise fExpr sExpr) x y = do
    vf <- evalExpr fExpr x y; vs <- evalExpr sExpr x y
    let freq = extractScalar vf; seed = round (extractScalar vs) :: Int
    Just (Scalar (noise2D (x * freq) (y * freq) seed))
evalExpr (EColorNoise fExpr sExpr) x y = do
    vf <- evalExpr fExpr x y; vs <- evalExpr sExpr x y
    let freq = extractScalar vf; seed = round (extractScalar vs) :: Int
    let (r, g, b) = colorNoise2D (x * freq) (y * freq) seed
    Just (Color r g b)
evalExpr (EMandelbrot cxExpr cyExpr) x y = do
    vcx <- evalExpr cxExpr x y; vcy <- evalExpr cyExpr x y
    let cx = extractScalar vcx; cy = extractScalar vcy
    Just (Scalar (1.0 - fromIntegral (mandelbrotIter 0 0 cx cy) / fromIntegral maxFractalIter))
evalExpr (EJulia zxExpr zyExpr cExpr) x y = do
    vzx <- evalExpr zxExpr x y; vzy <- evalExpr zyExpr x y; vc <- evalExpr cExpr x y
    let zx = extractScalar vzx; zy = extractScalar vzy
        (cr, ci) = case vc of Scalar s -> (s, 0.0); Color r g _ -> (r, g)
    Just (Scalar (1.0 - fromIntegral (juliaIter zx zy cr ci) / fromIntegral maxFractalIter))
evalExpr (ENewton zxExpr zyExpr) x y = do
    vzx <- evalExpr zxExpr x y; vzy <- evalExpr zyExpr x y
    let zx = extractScalar vzx; zy = extractScalar vzy
    let (r, g, b) = newtonIter zx zy
    Just (Color r g b)
evalExpr (EIfs aExpr bExpr cExpr dExpr) x y = do
    va <- evalExpr aExpr x y; vb <- evalExpr bExpr x y
    vc <- evalExpr cExpr x y; vd <- evalExpr dExpr x y
    let a = extractScalar va; b = extractScalar vb
        c = extractScalar vc; d = extractScalar vd
    Just (Scalar (ifsIter x y a b c d 8))

-- Color Spaces & Warped Noise
evalExpr (EHsvToRgb hExpr sExpr vExpr) x y = do
    vh <- evalExpr hExpr x y; vs <- evalExpr sExpr x y; vv <- evalExpr vExpr x y
    let h = extractScalar vh; s = extractScalar vs; v = extractScalar vv
        (r, g, b) = hsvToRgb h s v
    Just (Color r g b)

evalExpr (EWarpedBwNoise uExpr vExpr fExpr sExpr) x y = do
    vu <- evalExpr uExpr x y; vv <- evalExpr vExpr x y
    vf <- evalExpr fExpr x y; vs <- evalExpr sExpr x y
    let u = extractScalar vu; v = extractScalar vv
        freq = extractScalar vf; seed = round (extractScalar vs) :: Int
    Just (Scalar (noise2D (u * freq) (v * freq) seed))

evalExpr (EWarpedColorNoise uExpr vExpr fExpr sExpr) x y = do
    vu <- evalExpr uExpr x y; vv <- evalExpr vExpr x y
    vf <- evalExpr fExpr x y; vs <- evalExpr sExpr x y
    let u = extractScalar vu; v = extractScalar vv
        freq = extractScalar vf; seed = round (extractScalar vs) :: Int
        (r, g, b) = colorNoise2D (u * freq) (v * freq) seed
    Just (Color r g b)

-- Image Processing (Analytical)
evalExpr (EBlur a) x y = Just (analyticalBlur a x y)

evalExpr (EBandPass a) x y = 
    let largeDelta = 0.05 
        lowVals = [ evalSafe a (x + dx') (y + dy') | dx' <- [-largeDelta, 0, largeDelta], dy' <- [-largeDelta, 0, largeDelta] ]
        lowPV = foldl' (binaryOp (+)) (Scalar 0) lowVals
        lowFreq = binaryOp safeDiv lowPV (Scalar 9.0)
        
        highFreq = analyticalBlur a x y
        
        diff = binaryOp (-) highFreq lowFreq
    in Just (binaryOp (*) diff (Scalar 5.0)) 

evalExpr (EGradMag a) x y = 
    let va_p = evalSafe a (x + fdDelta) y
        va_m = evalSafe a (x - fdDelta) y
        va_u = evalSafe a x (y + fdDelta)
        va_d = evalSafe a x (y - fdDelta)
        gx = binaryOp (-) va_p va_m
        gy = binaryOp (-) va_u va_d
        gx2 = binaryOp (*) gx gx
        gy2 = binaryOp (*) gy gy
        sumSq = binaryOp (+) gx2 gy2
        mag = unaryOp sqrt sumSq
        normMag = unaryOp (/ (2.0 * fdDelta)) mag
    in Just (unaryOp (min 1.0) normMag)

evalExpr (EGradDir a) x y = 
    let gx = extractScalar (evalSafe a (x + fdDelta) y) - extractScalar (evalSafe a (x - fdDelta) y)
        gy = extractScalar (evalSafe a x (y + fdDelta)) - extractScalar (evalSafe a x (y - fdDelta))
        angle = atan2 gy gx
        norm = (angle + pi) / (2.0 * pi)
    in Just (Scalar norm)

evalExpr (EBump imgExpr lxExpr lyExpr lzExpr) x y = 
    let baseColor = evalSafe imgExpr x y
        bumpDelta = 0.05 
        gx = extractScalar (evalSafe imgExpr (x + bumpDelta) y) - extractScalar (evalSafe imgExpr (x - bumpDelta) y)
        gy = extractScalar (evalSafe imgExpr x (y + bumpDelta)) - extractScalar (evalSafe imgExpr x (y - bumpDelta))
        surfaceScale = 15.0 
        nx = -gx * surfaceScale; ny = -gy * surfaceScale; nz = 1.0
        nLen = sqrt (nx*nx + ny*ny + nz*nz)
        nnx = nx/nLen; nny = ny/nLen; nnz = nz/nLen
        vlx = extractScalar (evalSafe lxExpr x y)
        vly = extractScalar (evalSafe lyExpr x y)
        vlz = extractScalar (evalSafe lzExpr x y)
        lLen = sqrt (vlx*vlx + vly*vly + vlz*vlz)
    in if lLen < 0.0001 
       then Just baseColor 
       else let llx = vlx/lLen; lly = vly/lLen; llz = vlz/lLen
                dot = max 0.0 (nnx*llx + nny*lly + nnz*llz)
            in Just (binaryOp (*) baseColor (Scalar dot))

liftBinary :: (Double -> Double -> Double) -> Expr -> Expr -> Double -> Double -> Maybe PixelValue
liftBinary f a b x y = do
    va <- evalExpr a x y; vb <- evalExpr b x y
    Just (binaryOp f va vb)

-- =============================================================================
-- Pixel Value Conversion & Rendering
-- =============================================================================

toByte :: Double -> Pixel8
toByte d = let i = round (d * 255) :: Int in if i < 0 then 0 else if i > 255 then 255 else fromIntegral i

toPixelRGB8 :: PixelValue -> PixelRGB8
toPixelRGB8 (Scalar s) = PixelRGB8 (toByte s) (toByte s) (toByte s)
toPixelRGB8 (Color r g b) = PixelRGB8 (toByte r) (toByte g) (toByte b)

renderExpr :: Config -> Expr -> IO (Either String (Image PixelRGB8))
renderExpr cfg expr = return $ Right $ generateImageParallel pixelFn (width cfg) (height cfg)
  where
    w = width cfg
    h = height cfg
    normX px = (px / fromIntegral w) * 2 - 1
    normY py = (py / fromIntegral h) * 2 - 1

    pixelFn px py 
      | aa cfg    = aaPixel px py
      | otherwise = singlePixel px py

    singlePixel px py = 
      case evalExpr expr (normX (fromIntegral px)) (normY (fromIntegral py)) of 
        Just val -> toPixelRGB8 val
        Nothing  -> PixelRGB8 255 0 255

    aaPixel px py = 
      let offsets = [ (0.25, 0.25), (0.75, 0.25)
                , (0.25, 0.0), (0.75, 0.0) 
                , (0.25, 1.0), (0.75, 1.0) ]
          samples = [ evalNorm (normX (fromIntegral px + dx)) (normY (fromIntegral py + dy)) 
                    | (dx, dy) <- offsets ]
      in averagePixel samples

    evalNorm nx ny = 
      case evalExpr expr nx ny of 
        Just val -> val
        Nothing  -> Scalar 0 

    averagePixel vals = 
      let (r, g, b, c) = foldl' accumulate (0.0, 0.0, 0.0, 0) vals
          invC = 1.0 / fromIntegral c
      in PixelRGB8 (toByte (r * invC)) (toByte (g * invC)) (toByte (b * invC))

    accumulate (r, g, b, c) (Scalar s) = (r + s, g + s, b + s, c + 1)
    accumulate (r, g, b, c) (Color cr cg cb) = (r + cr, g + cg, b + cb, c + 1)

-- | Splits image generation by row, evaluating strictly in parallel across all CPU cores
generateImageParallel :: (Int -> Int -> PixelRGB8) -> Int -> Int -> Image PixelRGB8
generateImageParallel gen w h = 
    let rows = map genRow [0..h-1]
        genRow y = V.generate (w * 3) $ \i ->
            let (PixelRGB8 r g b) = gen (i `div` 3) y
            in case i `mod` 3 of
                 0 -> r
                 1 -> g
                 _ -> b
        parallelRows = rows `using` parList rseq
    in Image w h (V.concat parallelRows)

-- =============================================================================
-- Random Helpers
-- =============================================================================

mapStateList :: (s -> a -> (b, s)) -> s -> [a] -> ([b], s)
mapStateList _ s [] = ([], s)
mapStateList f s (x:xs) = 
    let (y, s') = f s x
        (ys, s'') = mapStateList f s' xs
    in (y:ys, s'')

randomName :: StdGen -> Dictionary -> (String, StdGen)
randomName g (Dictionary a1 a2 n) =
    let (i1, g1) = randomR (0, length a1 - 1) g
        (i2, g2) = randomR (0, length a2 - 1) g1
        (i3, g3) = randomR (0, length n - 1) g2
    in (a1 !! i1 ++ "-" ++ a2 !! i2 ++ "-" ++ n !! i3, g3)

-- =============================================================================
-- Random Expression Generator
-- =============================================================================

randomExpr :: StdGen -> Int -> (Expr, StdGen)
randomExpr g 0 = randomLeaf g
randomExpr g depth =
    let (isLeaf, g1) = randomR (0, 3 :: Int) g
    in if isLeaf == 0
       then randomLeaf g1
       else let (fnIdx, g2) = randomR (0, 30 :: Int) g1
            in if fnIdx < 9        -- 9 Unaries
               then randomUnary g2 depth
               else if fnIdx < 25   -- 16 Binaries
               then randomBinary g2 depth
               else if fnIdx < 27   -- 2 Ternaries
               then randomTernary g2 depth
               else                 -- 4 Quaternaries
                    randomQuaternary g2 depth

randomLeaf :: StdGen -> (Expr, StdGen)
randomLeaf g =
    case r of
        0 -> (EVar X, g1)
        1 -> (EVar Y, g1)
        2 -> (EConst c, g2)
        3 -> (EVec cr cg cb, g5)
        _ -> (EVar X, g1)
  where
    (r, g1) = randomR (0, 3 :: Int) g
    (c, g2) = randomR (-1.0, 1.0) g1
    (cr, g3) = randomR (0.0, 1.0) g2
    (cg, g4) = randomR (0.0, 1.0) g3
    (cb, g5) = randomR (0.0, 1.0) g4

randomUnary :: StdGen -> Int -> (Expr, StdGen)
randomUnary g depth =
    let (idx, g1) = randomR (0, 8 :: Int) g
        (a, g2) = randomExpr g1 (depth - 1)
    in case idx of
        0 -> (EAbs a, g2)
        1 -> (ESin a, g2)
        2 -> (ECos a, g2)
        3 -> (ELog a, g2)
        4 -> (ERound a, g2)
        5 -> (EBlur a, g2)
        6 -> (EBandPass a, g2)
        7 -> (EGradMag a, g2)
        8 -> (EGradDir a, g2)
        _ -> (EAbs a, g2)

randomBinary :: StdGen -> Int -> (Expr, StdGen)
randomBinary g depth =
    let fns = [ EAdd, ESub, EMult, EDiv, EMod
              , EAnd, EOr, EXor
              , EPolarCoords
              , EAtan, EExpt, EMin, EMax
              , EMandelbrot, ENewton, EBwNoise, EColorNoise ]
        (idx, g1) = randomR (0, length fns - 1) g
        fn = fns !! idx
        (a, g2) = randomExpr g1 (depth - 1)
        (b, g3) = randomExpr g2 (depth - 1)
    in (fn a b, g3)

randomTernary :: StdGen -> Int -> (Expr, StdGen)
randomTernary g depth =
    let (idx, g1) = randomR (0, 1 :: Int) g
        (a, g2) = randomExpr g1 (depth - 1)
        (b, g3) = randomExpr g2 (depth - 1)
        (c, g4) = randomExpr g3 (depth - 1)
    in case idx of
        0 -> (EJulia a b c, g4)
        1 -> (EHsvToRgb a b c, g4)
        _ -> (EJulia a b c, g4)

randomQuaternary :: StdGen -> Int -> (Expr, StdGen)
randomQuaternary g depth =
    let (idx, g1) = randomR (0, 3 :: Int) g
        (a, g2) = randomExpr g1 (depth - 1)
        (b, g3) = randomExpr g2 (depth - 1)
        (c, g4) = randomExpr g3 (depth - 1)
        (d, g5) = randomExpr g4 (depth - 1)
    in case idx of
        0 -> (EWarpedBwNoise a b c d, g5)
        1 -> (EWarpedColorNoise a b c d, g5)
        2 -> (EBump a b c d, g5)
        3 -> (EIfs a b c d, g5)
        _ -> (EIfs a b c d, g5)

generatePopulation :: Config -> IO Population
generatePopulation cfg = do
    dictE <- readDictionary (dictFile cfg)
    case dictE of
        Left err -> do
            putStrLn $ "Dictionary error: " ++ err
            return []
        Right dict -> do
            g <- newStdGen
            let (exprs, g1) = mapStateList (\gen _ -> randomExpr gen (depth cfg)) g [1..genSize cfg]
                (names, _) = mapStateList (\gen _ -> randomName gen dict) g1 [1..genSize cfg]
            return (zipWith Genotype names exprs)

-- =============================================================================
-- Mutation Operations
-- =============================================================================

sizeExpr :: Expr -> Int
sizeExpr (EConst _)           = 1
sizeExpr (EVec _ _ _)         = 1
sizeExpr (EVar _)             = 1
sizeExpr (EAbs a)             = 1 + sizeExpr a
sizeExpr (ESin a)             = 1 + sizeExpr a
sizeExpr (ECos a)             = 1 + sizeExpr a
sizeExpr (ELog a)             = 1 + sizeExpr a
sizeExpr (ERound a)           = 1 + sizeExpr a
sizeExpr (EBlur a)            = 1 + sizeExpr a
sizeExpr (EBandPass a)        = 1 + sizeExpr a
sizeExpr (EGradMag a)         = 1 + sizeExpr a
sizeExpr (EGradDir a)         = 1 + sizeExpr a
sizeExpr (EAdd a b)           = 1 + sizeExpr a + sizeExpr b
sizeExpr (ESub a b)           = 1 + sizeExpr a + sizeExpr b
sizeExpr (EMult a b)          = 1 + sizeExpr a + sizeExpr b
sizeExpr (EDiv a b)           = 1 + sizeExpr a + sizeExpr b
sizeExpr (EMod a b)           = 1 + sizeExpr a + sizeExpr b
sizeExpr (EAnd a b)           = 1 + sizeExpr a + sizeExpr b
sizeExpr (EOr a b)            = 1 + sizeExpr a + sizeExpr b
sizeExpr (EXor a b)           = 1 + sizeExpr a + sizeExpr b
sizeExpr (EPolarCoords a b)   = 1 + sizeExpr a + sizeExpr b
sizeExpr (EAtan a b)          = 1 + sizeExpr a + sizeExpr b
sizeExpr (EExpt a b)          = 1 + sizeExpr a + sizeExpr b
sizeExpr (EMin a b)           = 1 + sizeExpr a + sizeExpr b
sizeExpr (EMax a b)           = 1 + sizeExpr a + sizeExpr b
sizeExpr (EMandelbrot a b)    = 1 + sizeExpr a + sizeExpr b
sizeExpr (ENewton a b)        = 1 + sizeExpr a + sizeExpr b
sizeExpr (EIfs a b c d)       = 1 + sizeExpr a + sizeExpr b + sizeExpr c + sizeExpr d
sizeExpr (EBwNoise a b)       = 1 + sizeExpr a + sizeExpr b
sizeExpr (EColorNoise a b)    = 1 + sizeExpr a + sizeExpr b
sizeExpr (EJulia a b c)       = 1 + sizeExpr a + sizeExpr b + sizeExpr c
sizeExpr (EHsvToRgb a b c)    = 1 + sizeExpr a + sizeExpr b + sizeExpr c
sizeExpr (EWarpedBwNoise a b c d)    = 1 + sizeExpr a + sizeExpr b + sizeExpr c + sizeExpr d
sizeExpr (EWarpedColorNoise a b c d) = 1 + sizeExpr a + sizeExpr b + sizeExpr c + sizeExpr d
sizeExpr (EBump a b c d)      = 1 + sizeExpr a + sizeExpr b + sizeExpr c + sizeExpr d

getAllNodes :: Expr -> [Expr]
getAllNodes e@(EConst _)           = [e]
getAllNodes e@(EVec _ _ _)         = [e]
getAllNodes e@(EVar _)             = [e]
getAllNodes e@(EAbs a)             = e : getAllNodes a
getAllNodes e@(ESin a)             = e : getAllNodes a
getAllNodes e@(ECos a)             = e : getAllNodes a
getAllNodes e@(ELog a)             = e : getAllNodes a
getAllNodes e@(ERound a)           = e : getAllNodes a
getAllNodes e@(EBlur a)            = e : getAllNodes a
getAllNodes e@(EBandPass a)        = e : getAllNodes a
getAllNodes e@(EGradMag a)         = e : getAllNodes a
getAllNodes e@(EGradDir a)         = e : getAllNodes a
getAllNodes e@(EAdd a b)           = e : getAllNodes a ++ getAllNodes b
getAllNodes e@(ESub a b)           = e : getAllNodes a ++ getAllNodes b
getAllNodes e@(EMult a b)          = e : getAllNodes a ++ getAllNodes b
getAllNodes e@(EDiv a b)           = e : getAllNodes a ++ getAllNodes b
getAllNodes e@(EMod a b)           = e : getAllNodes a ++ getAllNodes b
getAllNodes e@(EAnd a b)           = e : getAllNodes a ++ getAllNodes b
getAllNodes e@(EOr a b)            = e : getAllNodes a ++ getAllNodes b
getAllNodes e@(EXor a b)           = e : getAllNodes a ++ getAllNodes b
getAllNodes e@(EPolarCoords a b)   = e : getAllNodes a ++ getAllNodes b
getAllNodes e@(EAtan a b)          = e : getAllNodes a ++ getAllNodes b
getAllNodes e@(EExpt a b)          = e : getAllNodes a ++ getAllNodes b
getAllNodes e@(EMin a b)           = e : getAllNodes a ++ getAllNodes b
getAllNodes e@(EMax a b)           = e : getAllNodes a ++ getAllNodes b
getAllNodes e@(EMandelbrot a b)    = e : getAllNodes a ++ getAllNodes b
getAllNodes e@(ENewton a b)        = e : getAllNodes a ++ getAllNodes b
getAllNodes e@(EIfs a b c d)       = e : getAllNodes a ++ getAllNodes b ++ getAllNodes c ++ getAllNodes d
getAllNodes e@(EBwNoise a b)       = e : getAllNodes a ++ getAllNodes b
getAllNodes e@(EColorNoise a b)    = e : getAllNodes a ++ getAllNodes b
getAllNodes e@(EJulia a b c)       = e : getAllNodes a ++ getAllNodes b ++ getAllNodes c
getAllNodes e@(EHsvToRgb a b c)    = e : getAllNodes a ++ getAllNodes b ++ getAllNodes c
getAllNodes e@(EWarpedBwNoise a b c d)    = e : getAllNodes a ++ getAllNodes b ++ getAllNodes c ++ getAllNodes d
getAllNodes e@(EWarpedColorNoise a b c d) = e : getAllNodes a ++ getAllNodes b ++ getAllNodes c ++ getAllNodes d
getAllNodes e@(EBump a b c d)      = e : getAllNodes a ++ getAllNodes b ++ getAllNodes c ++ getAllNodes d

clamp01 :: Double -> Double
clamp01 v = max 0.0 (min 1.0 v)

mutateExpr :: StdGen -> Int -> Expr -> (Expr, StdGen)
mutateExpr g baseRate expr =
    let sz = max (sizeExpr expr) 1
        prob = fromIntegral baseRate / 10.0 / fromIntegral sz
        (shouldMutate, g1) = randomR (0.0, 1.0 :: Double) g
    in if shouldMutate < prob
       then applyRandomMutation g1 baseRate expr
       else mutateChildren g1 baseRate expr

mutateChildren :: StdGen -> Int -> Expr -> (Expr, StdGen)
mutateChildren g _ e@(EConst _)           = (e, g)
mutateChildren g _ e@(EVec _ _ _)         = (e, g)
mutateChildren g _ e@(EVar _)             = (e, g)
mutateChildren g r (EAbs a)               = let (a', g1) = mutateExpr g r a in (EAbs a', g1)
mutateChildren g r (ESin a)               = let (a', g1) = mutateExpr g r a in (ESin a', g1)
mutateChildren g r (ECos a)               = let (a', g1) = mutateExpr g r a in (ECos a', g1)
mutateChildren g r (ELog a)               = let (a', g1) = mutateExpr g r a in (ELog a', g1)
mutateChildren g r (ERound a)             = let (a', g1) = mutateExpr g r a in (ERound a', g1)
mutateChildren g r (EBlur a)              = let (a', g1) = mutateExpr g r a in (EBlur a', g1)
mutateChildren g r (EBandPass a)          = let (a', g1) = mutateExpr g r a in (EBandPass a', g1)
mutateChildren g r (EGradMag a)           = let (a', g1) = mutateExpr g r a in (EGradMag a', g1)
mutateChildren g r (EGradDir a)           = let (a', g1) = mutateExpr g r a in (EGradDir a', g1)
mutateChildren g r (EAdd a b)             = let (a', g1) = mutateExpr g r a; (b', g2) = mutateExpr g1 r b in (EAdd a' b', g2)
mutateChildren g r (ESub a b)             = let (a', g1) = mutateExpr g r a; (b', g2) = mutateExpr g1 r b in (ESub a' b', g2)
mutateChildren g r (EMult a b)            = let (a', g1) = mutateExpr g r a; (b', g2) = mutateExpr g1 r b in (EMult a' b', g2)
mutateChildren g r (EDiv a b)             = let (a', g1) = mutateExpr g r a; (b', g2) = mutateExpr g1 r b in (EDiv a' b', g2)
mutateChildren g r (EMod a b)             = let (a', g1) = mutateExpr g r a; (b', g2) = mutateExpr g1 r b in (EMod a' b', g2)
mutateChildren g r (EAnd a b)             = let (a', g1) = mutateExpr g r a; (b', g2) = mutateExpr g1 r b in (EAnd a' b', g2)
mutateChildren g r (EOr a b)              = let (a', g1) = mutateExpr g r a; (b', g2) = mutateExpr g1 r b in (EOr a' b', g2)
mutateChildren g r (EXor a b)             = let (a', g1) = mutateExpr g r a; (b', g2) = mutateExpr g1 r b in (EXor a' b', g2)
mutateChildren g r (EPolarCoords a b)     = let (a', g1) = mutateExpr g r a; (b', g2) = mutateExpr g1 r b in (EPolarCoords a' b', g2)
mutateChildren g r (EAtan a b)            = let (a', g1) = mutateExpr g r a; (b', g2) = mutateExpr g1 r b in (EAtan a' b', g2)
mutateChildren g r (EExpt a b)            = let (a', g1) = mutateExpr g r a; (b', g2) = mutateExpr g1 r b in (EExpt a' b', g2)
mutateChildren g r (EMin a b)             = let (a', g1) = mutateExpr g r a; (b', g2) = mutateExpr g1 r b in (EMin a' b', g2)
mutateChildren g r (EMax a b)             = let (a', g1) = mutateExpr g r a; (b', g2) = mutateExpr g1 r b in (EMax a' b', g2)
mutateChildren g r (EMandelbrot a b)      = let (a', g1) = mutateExpr g r a; (b', g2) = mutateExpr g1 r b in (EMandelbrot a' b', g2)
mutateChildren g r (ENewton a b)          = let (a', g1) = mutateExpr g r a; (b', g2) = mutateExpr g1 r b in (ENewton a' b', g2)
mutateChildren g r (EIfs a b c d)         = let (a', g1) = mutateExpr g r a; (b', g2) = mutateExpr g1 r b; (c', g3) = mutateExpr g2 r c; (d', g4) = mutateExpr g3 r d in (EIfs a' b' c' d', g4)
mutateChildren g r (EBwNoise a b)         = let (a', g1) = mutateExpr g r a; (b', g2) = mutateExpr g1 r b in (EBwNoise a' b', g2)
mutateChildren g r (EColorNoise a b)      = let (a', g1) = mutateExpr g r a; (b', g2) = mutateExpr g1 r b in (EColorNoise a' b', g2)
mutateChildren g r (EJulia a b c)         = let (a', g1) = mutateExpr g r a; (b', g2) = mutateExpr g1 r b; (c', g3) = mutateExpr g2 r c in (EJulia a' b' c', g3)
mutateChildren g r (EHsvToRgb a b c)      = let (a', g1) = mutateExpr g r a; (b', g2) = mutateExpr g1 r b; (c', g3) = mutateExpr g2 r c in (EHsvToRgb a' b' c', g3)
mutateChildren g r (EWarpedBwNoise a b c d)    = let (a', g1) = mutateExpr g r a; (b', g2) = mutateExpr g1 r b; (c', g3) = mutateExpr g2 r c; (d', g4) = mutateExpr g3 r d in (EWarpedBwNoise a' b' c' d', g4)
mutateChildren g r (EWarpedColorNoise a b c d) = let (a', g1) = mutateExpr g r a; (b', g2) = mutateExpr g1 r b; (c', g3) = mutateExpr g2 r c; (d', g4) = mutateExpr g3 r d in (EWarpedColorNoise a' b' c' d', g4)
mutateChildren g r (EBump a b c d)        = let (a', g1) = mutateExpr g r a; (b', g2) = mutateExpr g1 r b; (c', g3) = mutateExpr g2 r c; (d', g4) = mutateExpr g3 r d in (EBump a' b' c' d', g4)

applyRandomMutation :: StdGen -> Int -> Expr -> (Expr, StdGen)
applyRandomMutation g rate expr =
    let (mType, g1) = randomR (0, 6 :: Int) g
    in case mType of
        0 -> let depth = max 2 (sizeExpr expr `div` 3) in randomExpr g1 depth
        1 -> case expr of
                EConst d -> let (delta, g2) = randomR (-0.2, 0.2) g1 in (EConst (d + delta), g2)
                _ -> (expr, g1)
        2 -> case expr of
                EVec r gb b -> 
                    let (dr, g2) = randomR (-0.2, 0.2) g1
                        (dg, g3) = randomR (-0.2, 0.2) g2
                        (db, g4) = randomR (-0.2, 0.2) g3
                    in (EVec (clamp01 (r+dr)) (clamp01 (gb+dg)) (clamp01 (b+db)), g4)
                _ -> (expr, g1)
        3 -> swapFunction g1 expr
        4 -> wrapInFunction g1 expr
        5 -> unwrapFunction g1 expr
        6 -> let nodes = getAllNodes expr
             in if length nodes > 1
                then let (idx, g2) = randomR (0, length nodes - 1) g1
                     in (nodes !! idx, g2)
                else (expr, g1)

applySwapped :: Either (Expr -> Expr) (Expr -> Expr -> Expr) -> [Expr] -> StdGen -> (Expr, StdGen)
applySwapped (Left f) [a, _] g = (f a, g)
applySwapped (Left f) [a] g = (f a, g)
applySwapped (Right f) [a] g = let (b, g1) = randomLeaf g in (f a b, g1)
applySwapped (Right f) [a, b] g = (f a b, g)
applySwapped _ _ g = (EConst 0, g)

swapFunction :: StdGen -> Expr -> (Expr, StdGen)
swapFunction g expr =
    let unaries = [EAbs, ESin, ECos, ELog, ERound, EBlur, EBandPass, EGradMag, EGradDir]
        binaries = [EAdd, ESub, EMult, EDiv, EMod, EAnd, EOr, EXor, EPolarCoords, EAtan, EExpt, EMin, EMax, EMandelbrot, ENewton, EBwNoise, EColorNoise]
        allFns = map Left unaries ++ map Right binaries
        (idx, g1) = randomR (0, length allFns - 1) g
        fn = allFns !! idx
    in case expr of
        EAbs a             -> applySwapped fn [a] g1
        ESin a             -> applySwapped fn [a] g1
        ECos a             -> applySwapped fn [a] g1
        ELog a             -> applySwapped fn [a] g1
        ERound a           -> applySwapped fn [a] g1
        EBlur a            -> applySwapped fn [a] g1
        EBandPass a        -> applySwapped fn [a] g1
        EGradMag a         -> applySwapped fn [a] g1
        EGradDir a         -> applySwapped fn [a] g1
        EAdd a b           -> applySwapped fn [a, b] g1
        ESub a b           -> applySwapped fn [a, b] g1
        EMult a b          -> applySwapped fn [a, b] g1
        EDiv a b           -> applySwapped fn [a, b] g1
        EMod a b           -> applySwapped fn [a, b] g1
        EAnd a b           -> applySwapped fn [a, b] g1
        EOr a b            -> applySwapped fn [a, b] g1
        EXor a b           -> applySwapped fn [a, b] g1
        EPolarCoords a b   -> applySwapped fn [a, b] g1
        EAtan a b          -> applySwapped fn [a, b] g1
        EExpt a b          -> applySwapped fn [a, b] g1
        EMin a b           -> applySwapped fn [a, b] g1
        EMax a b           -> applySwapped fn [a, b] g1
        EMandelbrot a b    -> applySwapped fn [a, b] g1
        ENewton a b        -> applySwapped fn [a, b] g1
        EBwNoise a b       -> applySwapped fn [a, b] g1
        EColorNoise a b    -> applySwapped fn [a, b] g1
        _                  -> (expr, g)

wrapInFunction :: StdGen -> Expr -> (Expr, StdGen)
wrapInFunction g expr =
    let (wType, g1) = randomR (0, 10 :: Int) g
    in case wType of
        0 -> (EAbs expr, g1)
        1 -> (ESin expr, g1)
        2 -> (ECos expr, g1)
        3 -> (ELog expr, g1)
        4 -> (ERound expr, g1)
        5 -> (EBlur expr, g1)
        6 -> (EBandPass expr, g1)
        7 -> (EGradMag expr, g1)
        8 -> (EGradDir expr, g1)
        9 -> let (leaf, g2) = randomLeaf g1 in (EAdd expr leaf, g2)
        10 -> let (leaf, g2) = randomLeaf g1 in (EMult expr leaf, g2)
        _ -> (EAbs expr, g1)

unwrapFunction :: StdGen -> Expr -> (Expr, StdGen)
unwrapFunction g (EAbs a)               = (a, g)
unwrapFunction g (ESin a)               = (a, g)
unwrapFunction g (ECos a)               = (a, g)
unwrapFunction g (ELog a)               = (a, g)
unwrapFunction g (ERound a)             = (a, g)
unwrapFunction g (EBlur a)              = (a, g)
unwrapFunction g (EBandPass a)          = (a, g)
unwrapFunction g (EGradMag a)           = (a, g)
unwrapFunction g (EGradDir a)           = (a, g)
unwrapFunction g (EJulia a b c)         = let (idx, g1) = randomR (0, 2 :: Int) g in ([a, b, c] !! idx, g1)
unwrapFunction g (EHsvToRgb a b c)      = let (idx, g1) = randomR (0, 2 :: Int) g in ([a, b, c] !! idx, g1)
unwrapFunction g (EIfs a b c d)         = let (idx, g1) = randomR (0, 3 :: Int) g in ([a, b, c, d] !! idx, g1)
unwrapFunction g (EWarpedBwNoise a b c d)    = let (idx, g1) = randomR (0, 3 :: Int) g in ([a, b, c, d] !! idx, g1)
unwrapFunction g (EWarpedColorNoise a b c d) = let (idx, g1) = randomR (0, 3 :: Int) g in ([a, b, c, d] !! idx, g1)
unwrapFunction g (EBump a b c d)        = let (idx, g1) = randomR (0, 3 :: Int) g in ([a, b, c, d] !! idx, g1)
unwrapFunction g expr =
    let (idx, g1) = randomR (0, 1 :: Int) g
    in case expr of
        EAdd a b           -> (if idx == 0 then a else b, g1)
        ESub a b           -> (if idx == 0 then a else b, g1)
        EMult a b          -> (if idx == 0 then a else b, g1)
        EDiv a b           -> (if idx == 0 then a else b, g1)
        EMod a b           -> (if idx == 0 then a else b, g1)
        EAnd a b           -> (if idx == 0 then a else b, g1)
        EOr a b            -> (if idx == 0 then a else b, g1)
        EXor a b           -> (if idx == 0 then a else b, g1)
        EPolarCoords a b   -> (if idx == 0 then a else b, g1)
        EAtan a b          -> (if idx == 0 then a else b, g1)
        EExpt a b          -> (if idx == 0 then a else b, g1)
        EMin a b           -> (if idx == 0 then a else b, g1)
        EMax a b           -> (if idx == 0 then a else b, g1)
        EMandelbrot a b    -> (if idx == 0 then a else b, g1)
        ENewton a b        -> (if idx == 0 then a else b, g1)
        EBwNoise a b       -> (if idx == 0 then a else b, g1)
        EColorNoise a b    -> (if idx == 0 then a else b, g1)
        _                  -> (expr, g)

-- =============================================================================
-- CLI Parser & Main Execution
-- =============================================================================

parseArgs :: [String] -> Either String Config
parseArgs args = go args defaultConfig
  where
    go [] cfg = Right cfg
    go ("--generate":rest) cfg = go rest cfg { cmd = Generate }
    go ("--render":rest) cfg = go rest cfg { cmd = Render }
    go ("--breed":rest) cfg = go rest cfg { cmd = Breed }
    go (arg:rest) cfg
      | "--generate" `isPrefixOf` arg = go rest cfg { cmd = Generate }
      | "--render" `isPrefixOf` arg = go rest cfg { cmd = Render }
      | "--breed" `isPrefixOf` arg = go rest cfg { cmd = Breed }
      | "-d" `isPrefixOf` arg = case reads (drop 2 arg) of
          [(d, "")] -> go rest cfg { depth = d }
          _ -> Left "Invalid depth"
      | "-m" `isPrefixOf` arg = case reads (drop 2 arg) of
          [(m, "")] -> go rest cfg { mutation = m }
          _ -> Left "Invalid mutation rate"
      | "-g" `isPrefixOf` arg = case reads (drop 2 arg) of
          [(g, "")] -> go rest cfg { genSize = g }
          _ -> Left "Invalid generation size"
      | "-s" `isPrefixOf` arg = case break (== 'x') (drop 2 arg) of
          (wStr, 'x':hStr) -> case (reads wStr, reads hStr) of
              ([(w, "")], [(h, "")]) -> go rest cfg { width = w, height = h }
              _ -> Left "Invalid dimensions"
          _ -> Left "Invalid dimensions format (use WxH)"
      | "-aa0" == arg = go rest cfg { aa = False }
      | "-aa1" == arg = go rest cfg { aa = True }
      | otherwise = go rest cfg { srcFiles = srcFiles cfg ++ [arg] }

runCommand :: Config -> IO ()
runCommand cfg = case cmd cfg of
    Generate -> do
        pop <- generatePopulation cfg
        if null pop then return ()
        else case srcFiles cfg of
            [outFile] -> writePopulation outFile pop
            _ -> putStrLn "Please specify exactly one output file for --generate"
    Render -> do
        forM_ (srcFiles cfg) $ \srcFile -> do
            result <- readPopulation srcFile
            case result of
                Left err -> putStrLn $ "Error reading " ++ srcFile ++ ": " ++ err
                Right pop -> forM_ pop $ \(Genotype name expr) -> do
                    imgResult <- renderExpr cfg expr
                    case imgResult of
                        Left err -> putStrLn $ "Error rendering " ++ name ++ ": " ++ err
                        Right img -> do
                            let outPath = name ++ ".png"
                            writePng outPath img
                            putStrLn $ "Saved " ++ outPath
    Breed -> do
        forM_ (srcFiles cfg) $ \srcFile -> do
            result <- readPopulation srcFile
            case result of
                Left err -> putStrLn $ "Error reading " ++ srcFile ++ ": " ++ err
                Right pop -> do
                    g <- newStdGen
                    let bredPop = breedPopulation g (mutation cfg) pop
                    case srcFiles cfg of
                        [outFile] -> writePopulation outFile bredPop
                        _ -> putStrLn "Please specify exactly one output file for --breed"

breedPopulation :: StdGen -> Int -> Population -> Population
breedPopulation g rate pop = map (mutateGenotype g rate) pop

mutateGenotype :: StdGen -> Int -> Genotype -> Genotype
mutateGenotype g rate (Genotype name expr) =
    let (newExpr, _) = mutateExpr g rate expr
    in Genotype name newExpr

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Left err -> do
            putStrLn $ "CLI Error: " ++ err
            putStrLn "Usage: monadrian <opt> <args> <src_files>"
            putStrLn "  --generate         Generate a new population"
            putStrLn "  --render           Render a population to PNGs"
            putStrLn "  --breed            Breed a population via crossover + mutation"
            putStrLn "  -d[1-n]            Nesting depth for generation"
            putStrLn "  -m[1-10]           Mutation rate during breeding"
            putStrLn "  -g[1-n]            Number of images to generate"
            putStrLn "  -s[<w>x<h>]        Image dimensions"
            putStrLn "  -aa[0|1]           Anti-aliasing off/on"
        Right cfg -> runCommand cfg
