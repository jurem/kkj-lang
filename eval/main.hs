import Core
import Compile

import System.Environment


argsToSrc :: [String] -> [IO String]
argsToSrc srcs = map argToSrc srcs where
    argToSrc "-" = getContents
    argToSrc h = readFile h


main = do
    args <- getArgs
    srcs <- sequence $ argsToSrc args
    runall srcs ([], builtins)
