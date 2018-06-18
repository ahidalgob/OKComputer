module OKTypes where
type Id = String

data OKType = OKPointer {pointerType::OKType}
            | OKVoid
            | OKFunc {funcParamTypes::[OKType], funcRetType::OKType}
            | OKBoolean
            | OKInt
            | OKFloat
            | OKChar
            | OKString
            | OKNameType Id
            | OKArray {arraySize::Int, arrayType::OKType}
            | OKTuple [OKType]
            | OKList OKType

            | OKErrorT deriving (Show, Eq)

isNumericalType :: OKType -> Bool
isNumericalType OKFloat = True
isNumericalType OKInt = True
isNumericalType _ = False

