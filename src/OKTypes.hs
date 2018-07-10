module OKTypes where
type Id = String

data OKType = OKPointer {pointer_Type::OKType}
            | OKVoid
            | OKFunc {func_ParamTypes::[OKType], func_RetType::OKType}
            | OKBoolean
            | OKInt
            | OKFloat
            | OKChar
            | OKString
            | OKNameType {type_Name::Id, type_Type::OKType}
            | OKArray {array_Size::Int, array_Type::OKType}
            | OKTuple [OKType]
            | OKList {elems_type::OKType}
            | OKRecord Int
            | OKErrorT deriving (Eq)

instance Show OKType where
  show (OKPointer t) = "pointer("++ show t ++")"
  show OKVoid = "void"
  show (OKFunc params ret) = "func(" ++ show params ++ "->" ++ show ret ++ ")"
  show OKBoolean = "boolean"
  show OKInt = "int"
  show OKFloat = "float"
  show OKChar = "char"
  show OKString = "string"
  show (OKNameType id t) = "name("++ id ++ "," ++ show t ++")"
  show (OKArray sz t) = "array("++ show t ++")"
  show (OKTuple ts) = "tuple(" ++ show ts ++ ")"
  show (OKList t) = "list("++ show t ++")"
  show (OKRecord scp) = "record(scope="++ show scp ++")"



isNumericalType :: OKType -> Bool
isNumericalType OKFloat = True
isNumericalType OKInt = True
isNumericalType _ = False

isListType (OKList _) = True
isListType _ = False

isErrorType OKErrorT = True
isErrorType _ = False

isNameType (OKNameType _ _) = True
isNameType _ = False

solveNameTypes (OKNameType _ oktype) = solveNameTypes oktype
solveNameTypes t = t
