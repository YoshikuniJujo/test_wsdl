{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import System.Environment

import ParseXml

main :: IO ()
main = do
	fp : _ <- getArgs
	print . parse . snd . lexTags =<< readFile fp
