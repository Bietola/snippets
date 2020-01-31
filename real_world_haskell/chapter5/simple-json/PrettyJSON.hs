module PrettyJSON where

import Numeric
import SimpleJSON
import Data.Char
import Data.Bits

data Doc = ToBeDefined
  deriving (Show)

-- For directly converting json values
renderJValue :: JValue -> Doc
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull         = text "null"
renderJValue (JNumber num)    = double num
renderJValue (JString str) = string str

----------------------
-- Helper functions --
----------------------

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

text :: String -> Doc
text = undefined

double :: Double -> Doc
double = undefined

char :: Char -> Doc
char = undefined

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
              Just r -> text r
              Nothing | mustEscape c -> hexEscape c
                      | otherwise    -> char c
                      where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
                where ch a b = (a, ['\\', b])

hcat :: [Doc] -> Doc
hcat = undefined

enclose :: Char -> Char -> Doc -> Doc
enclose left right doc = char left <> doc <> char right

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise   = astral (d - 0x10000)
  where d = ord c

smallHex :: Int -> Doc
smallHex x = text "\\u"
         <> text (replicate (4 - length h) '0')
         <> text h
   where h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <> smallHex (b + 0xdc00)
  where a = (n `shiftR` 10) .&. 0x3ff
        b = n .&. 0x3ff

(<>) :: Doc -> Doc -> Doc
(<>) = undefined

series :: Doc -> Doc -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close . fsep . punctuate (char ',') . map item
