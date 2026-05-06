{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import Text.Parsec
import Text.Parsec.String (Parser)

-- =============================================================================
-- AST Data Types
-- =============================================================================

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

-- =============================================================================
-- S-Expression Parser
-- =============================================================================

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

-- =============================================================================
-- S-Expression to AST Conversion
-- =============================================================================

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

-- =============================================================================
-- Expression to S-Expression (for serialization)
-- =============================================================================

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

-- =============================================================================
-- Main (Testing)
-- =============================================================================

main :: IO ()
main = do
    putStrLn "=== Monadrian Iteration 1: Parser Test ==="
    putStrLn ""

    -- Test cases
    let testCases =
            [ "X"
            , "Y"
            , "0.5"
            , "(add X 0.5)"
            , "(sub Y X)"
            , "(mult X Y)"
            , "(div X 2.0)"
            , "(mod X Y)"
            , "(and X Y)"
            , "(or X Y)"
            , "(xor X Y)"
            , "(abs X)"
            , "(polar-coords X Y)"
            , "(mandelbrot X Y)"
            , "(julia X Y 0.355)"
            , "(newton X Y)"
            , "(bw-noise 0.1 42)"
            , "(color-noise 0.05 123)"
            , "(add (mult X 0.5) (abs Y))"
            , "(mod (add X 1.0) (abs Y))"
            ]

    let runTest input = do
            putStr $ "Parsing: " ++ input
            case parse parseSExpr "" input of
                Left err -> putStrLn $ " -> PARSE ERROR: " ++ show err
                Right sexpr -> do
                    putStrLn $ " -> SExpr: " ++ show sexpr
                    case toExpr sexpr of
                        Left err -> putStrLn $ "    -> CONV ERROR: " ++ err
                        Right expr -> do
                            putStrLn $ "    -> Expr:   " ++ show expr
                            putStrLn $ "    -> Back:   " ++ showExpr expr

    mapM_ runTest testCases

    -- Test round-trip
    putStrLn ""
    putStrLn "=== Round-trip test ==="
    let complexExpr = EAdd (EMult (EVar X) (EConst 0.5)) (EAbs (EVar Y))
    let serialized = showExpr complexExpr
    putStrLn $ "Original: " ++ show complexExpr
    putStrLn $ "Serialized: " ++ serialized
    case parse parseSExpr "" serialized of
        Left err -> putStrLn $ "Round-trip parse failed: " ++ show err
        Right sexpr -> case toExpr sexpr of
            Left err -> putStrLn $ "Round-trip convert failed: " ++ err
            Right expr
                | expr == complexExpr -> putStrLn "Round-trip SUCCESS: expressions match!"
                | otherwise -> putStrLn $ "Round-trip FAILED: got " ++ show expr

    putStrLn ""
    putStrLn "=== Error cases ==="
    let errorCases = ["Z", "(unknown X)", "(add)", "(add X Y Z)"]
    mapM_ runTest errorCases
