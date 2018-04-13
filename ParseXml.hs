{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ParseXml (
	XmlTree(..), parse, lexTags,
	isTag, getAttr) where

import Control.Arrow

import LexTags

data XmlTree
	= Elem {
		tagName :: String,
		attributes :: [(String, String)],
		children :: [XmlTree]
		}
	| ETxt String
	deriving Show

parse :: [Tag] -> (XmlTree, [Tag])
parse (OpenTag t as : ts) = case parseList ts of
	(es, CloseTag t' : ts')
		| t == t' -> (Elem t as es, ts')
	_ -> error "parse: not closed element"
parse (SelfClosingTag t as : ts) = (Elem t as [], ts)
parse (Text txt : ts) = (ETxt txt, ts)
parse _ = error "parse: bad tag"

parseList :: [Tag] -> ([XmlTree], [Tag])
parseList ts@(CloseTag _ : _) = ([], ts)
parseList ts = let (e, ts') = parse ts in (e :) `first` parseList ts'

isTag :: XmlTree -> String -> Bool
Elem t1 _ _ `isTag` t0 | t1 == t0 = True
_ `isTag` _ = False

getAttr :: XmlTree -> String -> Maybe String
getAttr (Elem _ attrs _) = (`lookup` attrs)
getAttr _ = const Nothing
