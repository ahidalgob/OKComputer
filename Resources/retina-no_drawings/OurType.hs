module OurType where

import AST

data OurType = Number | Boolean | Void deriving Eq
instance Show OurType where
    show Number = "number"
    show Boolean = "boolean"
    show Void = "void"



typeNConvert :: TypeN -> OurType
typeNConvert BooleanN = Boolean
typeNConvert NumberN = Number