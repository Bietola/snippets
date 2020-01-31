module SimpleJSON 
  (
    JValue(..)
  , getString
  , getInt
  , getDouble
  , getBool
  , getObject
  , getArray
  , isNull
  ) where

data JValue = JString String
            | JBool Bool
            | JNumber Double
            | JArray [JValue]
            | JObject [(String, JValue)]
            | JNull
            deriving (Show)
          
---------------
-- Accessors --
---------------

getString :: JValue -> Maybe String
getString (JString str) = Just str
getString _ = Nothing

getDouble :: JValue -> Maybe Double
getDouble (JNumber num) = Just num
getDouble _ = Nothing

getInt :: JValue -> Maybe Int
getInt (JNumber num) = Just $ round num
getInt _ = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool bool) = Just bool
getBool _ = Nothing

getArray :: JValue -> Maybe [JValue]
getArray (JArray arr) = Just arr
getArray _ = Nothing

getObject :: JValue -> Maybe [(String, JValue)]
getObject (JObject obj) = Just obj
getObject _ = Nothing

isNull :: JValue -> Bool
isNull JNull = True
isNull _ = False
