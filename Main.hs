{-# LANGUAGE OverloadedStrings #-}

module Main where

import Codec.Picture
import Text.Parsec
import Text.Parsec.String (Parser)

-- AST DATA TYPES

-- | A pixel value can be scalar (grayscale) or a color vector (RGB)
data PixelValue
    = Scalar Double
    | Color Double Double Double  -- R, G, B in [0, 1]
    deriving (Show, Eq)

-- | The expression AST for our image DSL
data Expr
    -- Leaf nodes
    = EConst Double                        -- Numeric constant
    | EVec Double Double Double            -- RGB color constant
    | EVar VarName                         -- Variable (X, Y)

    -- Arithmetic (2 args)
    | EAdd Expr Expr
    | ESub Expr Expr
    | EMult Expr Expr
    | EDiv Expr Expr
    | EMod Expr Expr

    -- Bitwise boolean (2 args)
    | EAnd Expr Expr
    | EOr Expr Expr
    | EXor Expr Expr

    -- Coordinate transforms
    | EPolarCoords Expr Expr               -- (polar-coords r theta) -> (x, y)

    -- Unary
    | EAbs Expr

    -- Fractals
    | EMandelbrot Expr Expr                -- (mandelbrot cx cy)
    | EJulia Expr Expr Expr                -- (julia x y c)
    | ENewton Expr Expr                    -- (newton x y)

    -- Noise
    | EBwNoise Expr Expr                   -- (bw-noise frequency seed)
    | EColorNoise Expr Expr                -- (color-noise frequency seed)
    deriving (Show, Eq)

-- | Variable names
data VarName = X | Y deriving (Show, Eq)

-- | A population of expressions (for breeding/generation)
type Population = [Expr]

-- S-EXPRESSION PARSER

-- | Raw s-expression before conversion to AST
data SExpr
    = SAtom String
    | SList [SExpr]
    deriving (Show, Eq)

-- | Parse a complete s-expression
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

-- | Parse multiple s-expressions (for population files)
parseSExprs :: Parser [SExpr]
parseSExprs = spaces *> sepEndBy parseSExpr' spaces <* eof

-- S-EXPRESSION TO AST CONVERTER

-- | Convert an s-expression to an Expr
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
parseTernary "julia" a b c = EJulia <$> toExpr a <*> toExpr b <*> toExpr c
parseTernary fn _ _ _      = Left $ "Unknown function: " ++ fn

-- EXPRESSION TO S-EXPRESSION (FOR SERIALIZATION)

-- | Convert an Expr back to S-Expression format
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

-- | Pretty-print an S-Expression
showSExpr :: SExpr -> String
showSExpr (SAtom s) = s
showSExpr (SList xs) = "(" ++ unwords (map showSExpr xs) ++ ")"

-- | Pretty-print an Expr
showExpr :: Expr -> String
showExpr = showSExpr . fromExpr

-- EXPRESSION EVALUATOR

-- | Evaluate an expression at normalized coordinates (x, y) in [-1, 1]
-- Returns Nothing for unimplemented operations
evalExpr :: Expr -> Double -> Double -> Maybe PixelValue
evalExpr (EVar X) x _     = Just (Scalar x)
evalExpr (EVar Y) _ y     = Just (Scalar y)
evalExpr (EConst d) _ _   = Just (Scalar d)
evalExpr (EVec r g b) _ _ = Just (Color r g b)
evalExpr _ _ _             = Nothing

-- PIXEL VALUE CONVERSION

-- | Convert a normalized [0,1] value to an 8-bit color component
toByte :: Double -> Pixel8
toByte = clampByte . round . (* 255)
  where
    clampByte v
        | v < 0     = 0
        | v > 255   = 255
        | otherwise = fromIntegral v

-- | Convert a PixelValue to a JuicyPixels PixelRGB8
-- Scalar values become grayscale
toPixelRGB8 :: PixelValue -> PixelRGB8
toPixelRGB8 (Scalar s) = PixelRGB8 (toByte s) (toByte s) (toByte s)
toPixelRGB8 (Color r g b) = PixelRGB8 (toByte r) (toByte g) (toByte b)

-- RENDERING

-- | Render settings
data RenderSettings = RenderSettings
    { rsWidth  :: Int
    , rsHeight :: Int
    }
    deriving (Show)

defaultRenderSettings :: RenderSettings
defaultRenderSettings = RenderSettings 256 256

-- | Convert pixel coordinates to normalized [-1, 1] range
toNormalized :: Int -> Int -> Int -> Int -> (Double, Double)
toNormalized width height px py =
    ( (fromIntegral px / fromIntegral width) * 2 - 1
    , (fromIntegral py / fromIntegral height) * 2 - 1
    )

-- | Render an expression to an Image
renderExpr :: RenderSettings -> Expr -> Either String (Image PixelRGB8)
renderExpr settings expr =
    let w = rsWidth settings
        h = rsHeight settings
        img = generateImage pixelFn w h
        pixelFn px py =
            let (nx, ny) = toNormalized w h px py
            in case evalExpr expr nx ny of
                Just val -> toPixelRGB8 val
                Nothing  -> PixelRGB8 255 0 255  -- Magenta for unimplemented
    in Right img

-- | Write an image to a PNG file
writePngFile :: FilePath -> Image PixelRGB8 -> IO ()
writePngFile path img = writePng path img

-- MAIN (testing)

main :: IO ()
main = do
    putStrLn "=== Monadrian: Test Outputs  ==="
    putStrLn ""

    -- Test expressions we can render in this iteration
    let tests =
            [ ("X", "Horizontal gradient (-1 to 1)", EVar X)
            , ("Y", "Vertical gradient (-1 to 1)", EVar Y)
            , ("0.5", "Solid mid-gray", EConst 0.5)
            , ("0.0", "Solid black", EConst 0.0)
            , ("1.0", "Solid white", EConst 1.0)
            , ("vec", "Solid color (0.8, 0.2, 0.1)", EVec 0.8 0.2 0.1)
            ]

    let settings = defaultRenderSettings

    mapM_ (runRenderTest settings) tests

    -- Test that unimplemented expressions render as magenta
    putStrLn "Testing unimplemented expression (should be magenta)..."
    let unimplExpr = EAdd (EVar X) (EVar Y)
    case renderExpr settings unimplExpr of
        Left err -> putStrLn $ "  Render error: " ++ err
        Right img -> do
            writePngFile "test_unimplemented.png" img
            putStrLn $ "  Wrote test_unimplemented.png"

    putStrLn ""
    putStrLn "Done! Check the test_*.png files."

runRenderTest :: RenderSettings -> (String, String, Expr) -> IO ()
runRenderTest settings (name, desc, expr) = do
    putStrLn $ "Rendering: " ++ name ++ " -- " ++ desc
    case renderExpr settings expr of
        Left err -> putStrLn $ "  ERROR: " ++ err
        Right img -> do
            let filename = "test_" ++ name ++ ".png"
            writePngFile filename img
            putStrLn $ "  Wrote " ++ filename
