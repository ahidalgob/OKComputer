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
            | OKNameType Id OKType
            | OKArray {array_Size::Int, array_Type::OKType}
            | OKTuple [OKType]
            | OKList OKType

            | OKErrorT deriving (Show, Eq)

isNumericalType :: OKType -> Bool
isNumericalType OKFloat = True
isNumericalType OKInt = True
isNumericalType _ = False

