{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module LexTags (XmlDecl(..), Tag(..), lexTags) where

import Control.Arrow
import Data.Char

lexTags :: String -> (XmlDecl, [Tag])
lexTags s = (xd, processTags $ tags r) where (xd, r) = xmlDecl s

data XmlDecl = XmlDecl { version :: String, encoding :: String } deriving Show

data Tag
	= OpenTag String [(String, String)]
	| SelfClosingTag String [(String, String)]
	| CloseTag String
	| Text String deriving Show

xmlDecl :: String -> (XmlDecl, String)
xmlDecl ('<' : '?' : s) =
	case (lookup "version" attrs, lookup "encoding" attrs) of
		(Just v, Just e) -> (XmlDecl { version = v, encoding = e }, r')
		_ -> error "xmlDecl: no version or no encoding"
	where
	("xml", r) = splitAt 3 s
	(ve, '?' : '>' : r') = span (/= '?') r
	attrs = attributes ve
xmlDecl _ = error "xmlDecl: not XML Declaration"

tags :: String -> [Tag]
tags ('<' : '/' : s) = let (t, r) = closeTag s in t : tags r
tags ('<' : s) = let (t, r) = openTag s in t : tags r
tags "" = []
tags s = let (t, r) = span (/= '<') s in Text t : tags r

openTag :: String -> (Tag, String)
openTag s = case r of
	'>' : r'' -> (OpenTag t $ attributes r', r'')
--	'>' : r'' -> (OpenTag t [(r', "")], r'')
	'/' : '>' : r'' -> (SelfClosingTag t $ attributes r', r'')
	_ -> error $ "openTag: tag not closing\n" ++ r
	where
	(s', r) = mySpan ((&&) <$> (/= '>') <*> (/= '/')) s
	(t, r') = span (not . isSpace) s'

mySpan :: (Char -> Bool) -> String -> (String, String)
mySpan p ('"' : s) = (('"' :) . (str ++) . ('"' :)) `first` mySpan p r
	where (str, '"' : r) = span (/= '"') s
mySpan p (c : s) | p c = (c :) `first` mySpan p s
mySpan _ s = ("", s)

attributes :: String -> [(String, String)]
attributes s | all isSpace s = []
attributes s = (trimming s', str) : attributes r'
	where
	(s', '=' : r) = span (/= '=') s
	(str, r') = string r 

string :: String -> (String, String)
string ('"' : s) = case span (/= '"') s of
	(str, '"' : r) -> (str, r)
	_ -> error "string: string literal not closed"
string (c : s) | isSpace c = string s
string _ = error "string: not string"

closeTag :: String -> (Tag, String)
closeTag s = (CloseTag s', r)
	where (s', '>' : r) = span (/= '>') s

processTags :: [Tag] -> [Tag]
processTags = removeEmpty . map trim

removeEmpty :: [Tag] -> [Tag]
removeEmpty = filter $ \case Text s | all isSpace s -> False; _ -> True

trim :: Tag -> Tag
trim (OpenTag t as) = OpenTag (trimming t) $ trimAttributes as
trim (SelfClosingTag t as) = SelfClosingTag (trimming t) $ trimAttributes as
trim (CloseTag t) = CloseTag $ trimming t
trim (Text t) = Text $ trimming t

trimAttributes :: [(String, String)] -> [(String, String)]
trimAttributes = map (first trimming)

trimming :: String -> String
trimming = reverse . dropWhile isSpace . reverse . dropWhile isSpace
