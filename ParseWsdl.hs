{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ParseWsdl where

import Control.Arrow
import Data.List
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

data Message = Message { messageName :: MessageName, parts :: [Part] } deriving Show
data Part = Part { partName :: PartName, typ :: Type } deriving Show
data MessageName = MessageName String deriving Show
data PartName = PartName String deriving Show
data Type = String | Int deriving Show

data PortType = PortType { portTypeName :: String, operations :: [Operation] }
	deriving Show

data Operation = Operation {
	operationName :: String,
	parameterOrder :: [PartName],
	input :: MessageName,
	output :: MessageName
	} deriving Show

data Binding = Binding deriving Show

data Service = Service deriving Show

wsdl :: XmlTree -> Either String Wsdl
wsdl (Elem "wsdl:definitions" _ xts) = do
	ms <- mapM mkMessage ms_
	pt <- maybe (Left "") Right $ mkPortType pt_
	return $ trace (show ()) Definition {
		types = Nothing,
		messages = ms,
		portType = pt,
		binding = Binding,
		service = Service }
	where
	ms_ = map (
		(`getAttr` "name") &&&
			map ((`getAttr` "name") &&& (`getAttr` "type"))
				. children )
		$ filter (`isTag` "wsdl:message") xts
	pt_ = ((`getAttr` "name") &&& map getOperation . children)
		. head $ filter (`isTag` "wsdl:portType") xts
wsdl _ = Left "I can't process such WSDL"

mkMessage ::
	(Maybe String, [(Maybe String, Maybe String)]) -> Either String Message
mkMessage (mn, eps) = Message
	<$> maybe (Left "mkMessage: no message name") (Right . MessageName) mn
	<*> mapM mkPart eps

mkPart :: (Maybe String, Maybe String) -> Either String Part
mkPart (mn, mt) = maybe (Left "mkPart: bad") Right
	$ Part . PartName <$> mn <*> (mkType =<< mt)

mkType :: String -> Maybe Type
mkType "xsd:string" = Just String
mkType "xsd:int" = Just Int
mkType _ = Nothing

mkPortType ::
	(Maybe String, [
		((Maybe String, Maybe [String]),
			(Maybe MessageName, Maybe MessageName)) ]) ->
	Maybe PortType
mkPortType (pn, os) = PortType <$> pn <*> mapM mkOperation os

mkOperation ::
	((Maybe String, Maybe [String]),
		(Maybe MessageName, Maybe MessageName)) ->
	Maybe Operation
mkOperation ((mn, po), (mi, mo)) = Operation
	<$> mn
	<*> (map PartName <$> po)
	<*> mi
	<*> mo

getOperation :: XmlTree ->
	((Maybe String, Maybe [String]), (Maybe MessageName, Maybe MessageName))
getOperation =
	((`getAttr` "name") &&& (words <$>) . (`getAttr` "parameterOrder")) &&&
	(getInput &&& getOutput) . children

getInput :: [XmlTree] -> Maybe MessageName
getInput = (MessageName <$>)
	. ((`getAttr` "message") =<<) . find (`isTag` "wsdl:input")

getOutput :: [XmlTree] -> Maybe MessageName
getOutput = (MessageName <$>)
	. ((`getAttr` "message") =<<) . find (`isTag` "wsdl:output")
