{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import System.Environment

import LexTags

main :: IO ()
main = do
	fp : _ <- getArgs
	print . lexTags =<< readFile fp
