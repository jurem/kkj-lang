module Core 
where

import System.Exit
import Data.Map.Strict


------------------- the stack

data Value =
    Int Int |
    Bool Bool |
    Str String |
    Fun (State -> IO State)

type Stack = [Value]

type Dict = Map String Value

type State = (Stack, Dict)


instance Show Value where
    show (Int n) = show n
    show (Bool b) = show b
    show (Str s) = show s
    show (Fun f) = "->"


useState :: (Stack -> Stack) -> State -> State
useState f (s, d) =
    (f s, d)

useIO :: (State -> State) -> State -> IO State
useIO f s =
    return $ f s

use = useIO . useState


------------------- helpers

cmpbool False True = -1
cmpbool True False = 1
cmpbool _ _ = 0


push :: Value -> Stack -> Stack
push x s =
    x : s

pint x =
    push (Int x)

pbool b =
    push (Bool b)

pstr x s =
    push (Str x) s

-- push single function
pfun f =
    push (Fun f)

-- push function/program, quotation
pprg p =
    pfun (translate p) where
        translate p s = Prelude.foldl (>>=) (return s) p

-- returns a function which push te values from the first stack to the second
pushall :: Stack -> Stack -> Stack
pushall [] s =
    s
pushall (h : t) s =
    pushall t . push h $ s


------------------- arithmetic

sneg (Int x : s) =
    Int (-x) : s

sadd (Int y : Int x : s) =
    Int (x + y) : s

ssub (Int y : Int x : s) =
    Int (x - y) : s

smul (Int y : Int x : s) =
    Int (x * y) : s

sdiv (Int y : Int x : s) =
    Int (x `div` y) : s

smod (Int y : Int x : s) =
    Int (x `mod` y) : s

sdivMod (Int y : Int x : s) =
    Int m : Int d : s where (d, m) = x `divMod` y

squot (Int y : Int x : s) =
    Int (x `quot` y) : s

srem (Int y : Int x : s) =
    Int (x `rem` y) : s

squotRem (Int y : Int x : s) =
    Int r : Int q : s where (q, r) = x `quotRem` y

sisInt (Int x : s) =
   strue s
sisInt (_ : s) =
   sfalse s


------------------- logical

strue =
    pbool True

sfalse =
    pbool False

sand (Bool y : Bool x : s) =
    pbool (x && y) s

sor (Bool y : Bool x : s) =
    pbool (x || y) s

snot (Bool x : s) =
    pbool (not x) s

sisBool (Bool x : s) =
    strue s
sisBool (_ : s) =
    sfalse s


------------------- type conversions

stoInt s@(Int _ : _) =
    s
stoInt s@(Bool _ : _) =
    (schoose . pint 0 . pint 1) s
-- not defined for String and Fun

stoBool s@(Bool _ : _) =
    s
stoBool s@(Int _ : _) =
    (snot . sisZero) s
stoBool s@(Str _ : _) =
    (snot . sisEmpty) s
-- not defined for Fun


------------------- comparisons

--result: neg,0,pos
scmp s@(Int y : Int x : r) =
    ssub s    

scmp (Bool y : Bool x : r) =
    pint (cmpbool y x) r

sisZero (Int x : s) =
    pbool (x == 0) s

sisNeg (Int x : s) =
    pbool (x < 0) s

sisPos (Int x : s) =
    pbool (x > 0) s

-- b x y choose
schoose (y : x : Bool b : s) =
    (if b then x else y) : s

-- n x y z intchoose
sintchoose (z : y : x : Int n : s) = do
    if n < 0 then x : s
    else if n > 0 then z : s
    else y : s


------------------- stack manipulators

sclear s =
    []

sid s =
    s

spop (_ : s) =
    s

sdup s@(x : _) =
    x : s

sdip s@(_ : x : _) =
    x : s

sdep s@(_ : _ : x : _) =
    x : s

sswap (y : x : s) =
    x : y : s

srot (z : y : x : s) =
    x : z : y : s

sroll (w : z : y : x : s) =
    x : w : z : y : s


------------------- functions, combinators

sapply (Fun f : s, d) =
    f (s, d)

-- dip operator: save x, exec f, restore x
sover (Fun f : x : s, d) = do
    f (s, d) >>= use (push x)

squote (x : s) =
    pfun (use (push x)) s

scompose ((Fun g) : (Fun f) : s) =
    pfun (\x -> return x >>= f >>= g) s

sisNull (Fun f : s , d) = do
    (t, _) <- f ([], d)     -- we ignore the dict since new definitions are encapsulated inside a quotation
    let isnull = Prelude.null t
    use (pbool isnull) $ (s, d)

sfirst (Fun f : s, d) = do
    (t, _) <- f ([], d)     -- see comment at sisNull
    return (last t : s, d)

slast (Fun f : s, d) = do
    (t, _) <- f ([], d)
    return (head t : s, d)

sfront (Fun f : s, d) = do
    (t, _) <- f ([], d)
    let g = use (pushall $ reverse $ tail t)
    return (Fun g : s, d)

sback (Fun f : s, d) = do
    (t, _) <- f ([], d)
    let g = use (pushall $ tail $ reverse t)
    return (Fun g : s, d)

seval (Fun f : s, d) = do
    (t, _) <- f ([], d)     -- see comment at sisNull
    return (Fun (use (pushall $ reverse t)) : s, d)


------------------- strings

sisEmpty (Str s : t) =
    pbool (s == []) t


scat (Str y : Str x : s) =
    Str (x ++ y) : s


------------------- printing

printvalue :: Value -> IO ()
printvalue (Str s) =
    putStr s
printvalue v =
    putStr . show $ v

printstack [] =
    return []
printstack (s@(h:t)) = do
    printstack t
    printvalue h
    putChar ' '
    return s

sprint (h : t, d) = do
    printvalue h
    putChar ' '
    return (t, d)

sprintln s = do
     r <- sprint s
     putChar '\n'
     return r

sln s = do
     putChar '\n'
     return s

sprintstack (s, d) = do
    putStr "> "
    printstack s
    putStr "\n"
    return (s, d)

sreadInt (s, d) = do
    l <- getLine
    let num = read l :: Int
    return (pint num s, d)

sreadBool (s, d) = do
     l <- getLine
     let b = read l :: Bool 
     return (pbool b s, d)

sreadStr (s, d) = do
    l <- getLine
    let str = read l :: String
    return (pstr str s, d)


------------------- system

sexit s =
    exitSuccess


------------------- definitions


pcall :: String -> State -> IO State
pcall name (s, d) =
    case d !? name of
        Just (Fun fun) -> fun (s, d)
        Just (val) -> (use (push val)) (s, d)
--        Nothing -> \st -> return st >>= pstr "invalid name" >>= sprint >>= sexit


pdef :: String -> Value -> Dict -> Dict
pdef name val dict =
    insert name val dict


sdef (Str n : v : s, d) = return (s, pdef n v d)
