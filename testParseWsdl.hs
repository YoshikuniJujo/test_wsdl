{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import System.Environment

import ParseWsdl

main :: IO ()
main = print . parseWsdl =<< readFile . head =<< getArgs
