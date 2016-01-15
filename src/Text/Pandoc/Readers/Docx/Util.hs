module Text.Pandoc.Readers.Docx.Util (
                                        NameSpaces
                                      , elemName
                                      , isElem
                                      , elemToNameSpaces
                                      , embedInPara
                                      ) where

import Text.XML.Light
import Data.Maybe (mapMaybe)

type NameSpaces = [(String, String)]

elemToNameSpaces :: Element -> NameSpaces
elemToNameSpaces = mapMaybe attrToNSPair . elAttribs

attrToNSPair :: Attr -> Maybe (String, String)
attrToNSPair (Attr (QName s _ (Just "xmlns")) val) = Just (s, val)
attrToNSPair _ = Nothing

elemName :: NameSpaces -> String -> String -> QName
elemName ns prefix name = QName name (lookup prefix ns) (Just prefix)

isElem :: NameSpaces -> String -> String -> Element -> Bool
isElem ns prefix name element =
  qName (elName element) == name &&
  qURI (elName element) == lookup prefix ns

-- This function wraps the provided elements in a paragraph unit. This is not a 
-- very pure function, since it alters XML.Light's AST, but it seems to be the 
-- cleanest way to implement paragraph math that is embedded within textual 
-- paragraph content
embedInPara :: NameSpaces -> [Attr] -> [Element] -> Element
embedInPara ns attribs elems = 
    Element { elName = elemName ns "w" "p",
              elAttribs = attribs,
              elContent = map (\e -> Elem e) elems,
              elLine = Nothing -- Should this really be nothing?
            }
