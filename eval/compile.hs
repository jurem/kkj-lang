module Compile
where

import Data.Char
import Data.Map.Strict

import Core


builtins :: Dict
builtins = Data.Map.Strict.map (\v -> Fun v) $ fromList [
        ("add", use sadd), ("sub", use ssub), ("mul", use smul),
        ("div", use sdiv), ("mod", use smod), ("divMod", use sdivMod),
        ("quot", use squot), ("rem", use srem), ("quotRem", use squotRem),
        ("isInt", use sisInt),

        ("true", use strue), ("false", use sfalse),
        ("and", use sand), ("or", use sor), ("not", use snot),
        ("choose", use schoose),
        ("isBool", use sisBool),

        ("toInt", use stoInt),
        ("toBool", use stoBool),

        ("cmp", use scmp),
        ("isZero", use sisZero), ("isNeg", use sisNeg), ("isPos", use sisPos),
        ("intchoose", use sintchoose),

        ("clear", use sclear), ("id", use sid),
        ("pop", use spop),
        ("dup", use sdup), ("dip", use sdip), ("dep", use sdep),

        ("swap", use sswap), ("rot", use srot), ("roll", use sroll),

        ("apply", sapply), ("over", sover),
        ("quote", use squote),
        ("compose", use scompose),

        ("eval", seval),
        ("isNull", sisNull),
        ("head", sfirst), ("tail", sback),
        ("first", sfirst), ("last", slast),
        ("front", sfront), ("back", sback),


        --  ("intfold", sintfold),


        -- ("cat", scat), ("yesno", syesno),

        ("print", sprint), ("println", sprintln),
        ("ln", sln), ("printstack", sprintstack),
        ("readInt", sreadInt), ("readBool", sreadBool), ("readStr", sreadStr),

        ("exit", sexit),

        -- ("def", sdef),

        -- -- some shortcuts
        (".", sprintstack)
    ]




data Node = TokInt Int | TokStr String | TokName String |
            TokFun String AST | TokErr String | TokQuote AST deriving Show
type AST = [Node]

parse :: String -> AST
parse src = ast where
    (ast, _) = parse' src

parse' :: String -> (AST, String)
parse' src =
    let
        skipSpace = dropWhile isSpace
        skipToEOL = dropWhile ((/=) '\n')
        spanStr delim = break ((==) delim)
        spanInt = span isDigit
        spanName = span isAlphaNum
        spanNonSpace = break isSpace
    in
        case skipSpace src of
            "" -> ([], "")
            ';' : ';' : src' ->
                parse' $ skipToEOL src'
            -- quotation begins with [ or {
            h : src' | h `elem` "[{" -> (TokQuote quote : ast, src''') where
                (quote, src'') = parse' src'
                (ast, src''') = parse' src''
            -- quotation ends with ] or }, function end
            h : src' | h `elem` "]};" -> ([], src')
            -- strings
            '"' : src' -> (TokStr str : ast, restsrc) where
                (str, src'') = spanStr '"' src'
                (ast, restsrc) = parse' (tail src'')
            -- function def.
            'f' : 'u' : 'n' : src' -> (TokFun name body : ast, restsrc) where
                (name, src'') = spanName (skipSpace src')
                (body, src''') = parse' src''
                (ast, restsrc) = parse' src'''
            -- integers
            s@(h : _) | isDigit h -> (TokInt int : ast, restsrc) where
                (lexeme, src') = spanInt s
                (int, _) : _ = reads lexeme :: [(Int, String)]
                (ast, restsrc) = parse' src'
            -- names
            s@(h : _) | isAlpha h -> (TokName name : ast, restsrc) where
                (name, src') = spanName s
                (ast, restsrc) = parse' src'
            -- symbols
            s -> (TokName lexeme : ast, restsrc) where
                (lexeme, src') = spanNonSpace s
                (ast, restsrc) = parse' src'


compile :: AST -> (State -> IO State)
compile [] = use sid
compile (TokInt int : rest) =
    \s -> return s >>= use (pint int) >>= compile rest
compile (TokStr str : rest) =
    \s -> return s >>= use (pstr str) >>= compile rest
compile (TokQuote quote : rest) =
    \s -> return s >>= use (pfun $ compile quote) >>= compile rest
compile (TokName name : rest) =
    \s -> return s >>= pcall name >>= compile rest
compile (TokFun name body : rest) =
    \(s, d) -> return (s, pdef name (Fun (compile body)) d) >>= compile rest    
--compile (TokErr lexeme : rest) =
--   \s -> return s >>= use pstr "invalid token" >>= sprint >>= use sexit


runone :: String -> State -> IO State
runone src state = do
    return state >>= (compile $ parse src)

runall :: [String] -> State -> IO State
runall [] state = return state
runall (h : t) state = 
    return state >>= runone h >>= runall t

