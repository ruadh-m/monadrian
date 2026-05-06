{-# LANGUAGE OverloadedStrings #-}

module Main where

import Codec.Picture
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Word (Word64)
import Data.Bits ((.&.), (.|.), xor, shiftR)
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import System.Environment (getArgs)
import System.Random (StdGen, newStdGen, randomR, split)
import Control.Monad (foldM)

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
    | EMandelbrot Expr Expr
    | EJulia Expr Expr Expr
    | ENewton Expr Expr
    | EBwNoise Expr Expr
    | EColorNoise Expr Expr
    deriving (Show, Eq)

data VarName = X | Y deriving (Show, Eq)

type Population = [Expr]

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
    , srcFile  :: String
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
    , srcFile  = "population.gen"
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

parseSExprs :: Parser [SExpr]
parseSExprs = spaces *> sepEndBy parseSExpr' spaces <* eof

-- =============================================================================
-- S-Expression to AST Conversion
-- =============================================================================

toExpr :: SExpr -> Either String Expr
toExpr (SAtom s) = parseAtom' s
toExpr (SList [SAtom fn]) = parseUnary fn
toExpr (SList [SAtom fn, a]) = parseBinary1 fn a
toExpr (SList [SAtom fn, a, b]) = parseBinary2 fn a b
toExpr (SList [SAtom fn, a, b, c]) = parseTernary fn a b c
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
parseUnary "abs" = Left "abs requires 1 argument"
parseUnary fn    = Left $ "Unknown function: " ++ fn

parseBinary1 :: String -> SExpr -> Either String Expr
parseBinary1 "abs" a = EAbs <$> toExpr a
parseBinary1 fn _    = Left $ "Unknown function: " ++ fn

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
parseBinary2 "mandelbrot" a b  = EMandelbrot <$> toExpr a <*> toExpr b
parseBinary2 "newton" a b      = ENewton <$> toExpr a <*> toExpr b
parseBinary2 "bw-noise" a b    = EBwNoise <$> toExpr a <*> toExpr b
parseBinary2 "color-noise" a b = EColorNoise <$> toExpr a <*> toExpr b
parseBinary2 fn _ _            = Left $ "Unknown function: " ++ fn

parseTernary :: String -> SExpr -> SExpr -> SExpr -> Either String Expr
parseTernary "vec" a b c   = EVec <$> parseDouble a <*> parseDouble b <*> parseDouble c
parseTernary "julia" a b c = EJulia <$> toExpr a <*> toExpr b <*> toExpr c
parseTernary fn _ _ _       = Left $ "Unknown function: " ++ fn

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
fromExpr (EMandelbrot a b)  = SList [SAtom "mandelbrot", fromExpr a, fromExpr b]
fromExpr (EJulia a b c)     = SList [SAtom "julia", fromExpr a, fromExpr b, fromExpr c]
fromExpr (ENewton a b)      = SList [SAtom "newton", fromExpr a, fromExpr b]
fromExpr (EBwNoise a b)     = SList [SAtom "bw-noise", fromExpr a, fromExpr b]
fromExpr (EColorNoise a b)  = SList [SAtom "color-noise", fromExpr a, fromExpr b]

showSExpr :: SExpr -> String
showSExpr (SAtom s) = s
showSExpr (SList xs) = "(" ++ unwords (map showSExpr xs) ++ ")"

showExpr :: Expr -> String
showExpr = showSExpr . fromExpr

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

toNormalized :: Int -> Int -> Int -> Int -> (Double, Double)
toNormalized width height px py = ((fromIntegral px / fromIntegral width) * 2 - 1, (fromIntegral py / fromIntegral height) * 2 - 1)

renderExpr :: Config -> Expr -> Either String (Image PixelRGB8)
renderExpr cfg expr = Right $ generateImage pixelFn (width cfg) (height cfg)
  where pixelFn px py = let (nx, ny) = toNormalized (width cfg) (height cfg) px py
                        in case evalExpr expr nx ny of Just val -> toPixelRGB8 val; Nothing -> PixelRGB8 255 0 255

-- =============================================================================
-- Random Expression Generator (Iteration 7)
-- =============================================================================

randomExpr :: StdGen -> Int -> (Expr, StdGen)
randomExpr g 0 = randomLeaf g
randomExpr g depth =
    let (isLeaf, g1) = randomR (0, 3 :: Int) g
    in if isLeaf == 0
       then randomLeaf g1
       else let (fnIdx, g2) = randomR (0, 13 :: Int) g1
            in if fnIdx < 1 
               then randomUnary g2 depth
               else if fnIdx < 13
               then randomBinary g2 depth
               else randomJulia g2 depth

randomLeaf :: StdGen -> (Expr, StdGen)
randomLeaf g =
    case r of
        0 -> (EVar X, g1)
        1 -> (EVar Y, g1)
        2 -> (EConst c, g2)
        3 -> (EVec cr cg cb, g5)
        _ -> (EVar X, g1) -- Unreachable
  where
    (r, g1) = randomR (0, 3 :: Int) g
    (c, g2) = randomR (-1.0, 1.0) g1
    (cr, g3) = randomR (0.0, 1.0) g2
    (cg, g4) = randomR (0.0, 1.0) g3
    (cb, g5) = randomR (0.0, 1.0) g4

randomUnary :: StdGen -> Int -> (Expr, StdGen)
randomUnary g depth =
    let (a, g1) = randomExpr g (depth - 1)
    in (EAbs a, g1)

randomBinary :: StdGen -> Int -> (Expr, StdGen)
randomBinary g depth =
    let fns = [EAdd, ESub, EMult, EDiv, EMod, EAnd, EOr, EXor, EPolarCoords, EMandelbrot, ENewton, EBwNoise, EColorNoise]
        (idx, g1) = randomR (0, length fns - 1) g
        fn = fns !! idx
        (a, g2) = randomExpr g1 (depth - 1)
        (b, g3) = randomExpr g2 (depth - 1)
    in (fn a b, g3)

randomJulia :: StdGen -> Int -> (Expr, StdGen)
randomJulia g depth =
    let (a, g1) = randomExpr g (depth - 1)
        (b, g2) = randomExpr g1 (depth - 1)
        (c, g3) = randomLeaf g2 -- Keep constant C as a leaf to avoid insane nesting
    in (EJulia a b c, g3)

generatePopulation :: Config -> IO Population
generatePopulation cfg = do
    g <- newStdGen
    let (gens, _) = split g
        (exprs, _) = mapAccumL (\gen _ -> randomExpr gen (depth cfg)) gens [1..genSize cfg]
    return exprs
  where
    mapAccumL f s [] = ([], s)
    mapAccumL f s (x:xs) = let (y, s') = f s x; (ys, s'') = mapAccumL f s' xs in (y:ys, s'')

-- =============================================================================
-- File I/O
-- =============================================================================

writePopulation :: FilePath -> Population -> IO ()
writePopulation path pop = writeFile path (unlines $ map showExpr pop)

readPopulation :: FilePath -> IO (Either String Population)
readPopulation path = do
    contents <- readFile path
    return $ case parse parseSExprs "" contents of
        Left err -> Left (show err)
        Right sexprs -> mapM toExpr sexprs

-- =============================================================================
-- CLI Argument Parsing
-- =============================================================================

parseSize :: String -> Either String (Int, Int)
parseSize s = case break (== 'x') s of
    (w, 'x':h) -> case (reads w, reads h) of
        ([(w', "")], [(h', "")]) -> Right (w', h')
        _ -> Left $ "Invalid size format: " ++ s
    _ -> Left $ "Invalid size format (missing 'x'): " ++ s

parseArgs :: [String] -> Either String Config
parseArgs args = foldM parseOne defaultConfig args
  where
    parseOne cfg "--generate" = Right cfg { cmd = Generate }
    parseOne cfg "--breed"    = Right cfg { cmd = Breed }
    parseOne cfg "--render"   = Right cfg { cmd = Render }
    
    parseOne cfg ('-':'d':ds) = case reads ds of
        [(d, "")] -> Right cfg { depth = d }
        _ -> Left $ "Invalid depth: " ++ ds
        
    parseOne cfg ('-':'m':ds) = case reads ds of
        [(m, "")] -> Right cfg { mutation = m }
        _ -> Left $ "Invalid mutation rate: " ++ ds
        
    parseOne cfg ('-':'g':ds) = case reads ds of
        [(g, "")] -> Right cfg { genSize = g }
        _ -> Left $ "Invalid generation size: " ++ ds
        
    parseOne cfg "-aa0" = Right cfg { aa = False }
    parseOne cfg "-aa1" = Right cfg { aa = True }
    parseOne cfg ('-':'a':'a':ds)   = Left $ "Invalid anti-aliasing flag: " ++ ds
    
    parseOne cfg ('-':'s':ds) = case parseSize ds of
        Right (w, h) -> Right cfg { width = w, height = h }
        Left err -> Left err
        
    parseOne cfg f = Right cfg { srcFile = f } -- Assume it's the filename

-- =============================================================================
-- Command Execution
-- =============================================================================

runCommand :: Config -> IO ()
runCommand cfg = case cmd cfg of
    Generate -> runGenerate cfg
    Breed    -> putStrLn "Error: --breed is not implemented until Iteration 9."
    Render   -> runRender cfg

runGenerate :: Config -> IO ()
runGenerate cfg = do
    putStrLn $ "Generating " ++ show (genSize cfg) ++ " images at depth " ++ show (depth cfg) ++ "..."
    pop <- generatePopulation cfg
    writePopulation (srcFile cfg) pop
    putStrLn $ "Saved to " ++ srcFile cfg
    mapM_ (\(i, e) -> putStrLn $ "  [" ++ show i ++ "] " ++ showExpr e) (zip [0..] pop)

runRender :: Config -> IO ()
runRender cfg = do
    result <- readPopulation (srcFile cfg)
    case result of
        Left err -> putStrLn $ "Error reading file: " ++ err
        Right pop -> do
            putStrLn $ "Rendering " ++ show (length pop) ++ " images..."
            mapM_ (renderAndSave cfg) (zip [0..] pop)
            putStrLn "Done."
  where
    renderAndSave cfg (idx, expr) = do
        let filename = "img_" ++ padNum idx ++ ".png"
        case renderExpr cfg expr of
            Left err -> putStrLn $ "  Error rendering " ++ filename ++ ": " ++ err
            Right img -> do
                writePng filename img
                putStrLn $ "  Wrote " ++ filename
    padNum n = if n < 10 then "00" ++ show n else if n < 100 then "0" ++ show n else show n

-- =============================================================================
-- Main
-- =============================================================================

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Left err -> do
            putStrLn $ "CLI Error: " ++ err
            putStrLn "Usage: monadrian <opt> <args> <src_file>"
            putStrLn "  --generate         Generate a new population"
            putStrLn "  --render           Render a population to PNGs"
            putStrLn "  --breed            Breed a population (Coming soon)"
            putStrLn "  -d[1-n]            Nesting depth for generation"
            putStrLn "  -m[1-10]           Mutation rate for breeding"
            putStrLn "  -g[1-n]            Number of images to generate"
            putStrLn "  -s[<w>x<h>]        Image dimensions"
            putStrLn "  -aa[0|1]           Anti-aliasing off/on"
        Right cfg -> runCommand cfg
