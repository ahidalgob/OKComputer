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
            | OKErrorT deriving (Show, Eq)

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
