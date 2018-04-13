{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ParseWsdl where

import Control.Arrow
import Debug.Trace

import ParseXml

parseWsdl :: String -> Either String Wsdl
parseWsdl = wsdl . fst . parse . snd . lexTags

data Wsdl = Definition {
	types :: Maybe Types,
	messages :: [Message],
	portType :: PortType,
	binding :: Binding,
	service :: Service
	} deriving Show

data Types = Types deriving Show

data Message = Message { messageName :: String, parts :: [Part] } deriving Show
data Part = Part { partName :: String, typ :: Type } deriving Show
data Type = String | Int deriving Show

data PortType = PortType deriving Show

data Binding = Binding deriving Show

data Service = Service deriving Show

wsdl :: XmlTree -> Either String Wsdl
wsdl (Elem "wsdl:definitions" _ xts) = do
	let	ms_ = map (
			(`getAttr` "name") &&&
				map ((`getAttr` "name") &&& (`getAttr` "type"))
					. children )
			$ filter (`isTag` "wsdl:message") xts
	ms <- mapM mkMessage ms_
	return $ trace (show ()) Definition {
		types = Nothing,
		messages = ms,
		portType = PortType,
		binding = Binding,
		service = Service }
wsdl _ = Left "I can't process such WSDL"

mkMessage ::
	(Maybe String, [(Maybe String, Maybe String)]) -> Either String Message
mkMessage (mn, eps) = Message
	<$> maybe (Left "mkMessage: no message name") Right mn
	<*> mapM mkPart eps

mkPart :: (Maybe String, Maybe String) -> Either String Part
mkPart (mn, mt) =
	maybe (Left "mkPart: bad") Right $ Part <$> mn <*> (mkType =<< mt)

mkType :: String -> Maybe Type
mkType "xsd:string" = Just String
mkType "xsd:int" = Just Int
mkType _ = Nothing
