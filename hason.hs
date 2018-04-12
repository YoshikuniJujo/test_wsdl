{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module People where

data Person = Person { name :: String, age :: Int } deriving Show

whole :: [Person]
whole = [
	Person { name = "Taro", age = 35 },
	Person { name = "Hanako", age = 32 }
	]
