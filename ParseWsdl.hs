{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ParseWsdl where

import ParseXml

parseWsdl :: String -> Wsdl
parseWsdl = wsdl . fst . parse . snd . lexTags

data Wsdl = Wsdl
	deriving Show

wsdl :: XmlTree -> Wsdl
wsdl xt = Wsdl
