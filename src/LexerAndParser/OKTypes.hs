module OKTypes where


type Id = String

-- OKType {{{1
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
  show (OKArray _ t) = "array("++ show t ++")"
  show (OKTuple ts) = "tuple(" ++ show ts ++ ")"
  show (OKList OKVoid) = "list()"
  show OKErrorT = "error_type"
  show (OKList t) = "list("++ show t ++")"
  show (OKRecord scp) = "record(scope="++ show scp ++")"


solveNameTypes (OKNameType _ oktype) = solveNameTypes oktype
solveNameTypes t = t


-- OKType helper functions {{{1

mergeVoidType :: OKType -> OKType -> OKType
mergeVoidType (OKPointer t1) (OKPointer t2) =
        let merged = mergeVoidType t1 t2
        in if merged == OKErrorT then OKErrorT
                                 else OKPointer merged

mergeVoidType (OKArray sz t1) (OKArray _ t2) =
    let merged = mergeVoidType t1 t2
        in if merged == OKErrorT then OKErrorT
                                 else OKArray sz merged

mergeVoidType (OKTuple ts1) (OKTuple ts2) =
    if length ts1 /= length ts2
       then OKErrorT
       else let merged = zipWith mergeVoidType ts1 ts2
            in if OKErrorT `elem` merged
                  then OKErrorT
                  else OKTuple merged

mergeVoidType (OKList t1) (OKList t2) =
    let merged = mergeVoidType t1 t2
        in if merged == OKErrorT then OKErrorT
                                 else OKList merged


mergeVoidType OKVoid t = t
mergeVoidType t OKVoid = t

mergeVoidType t1 t2 = if t1 == t2 then t1 else OKErrorT


listComp :: OKType -> OKType -> Bool
listComp (OKPointer t1) (OKPointer t2) = listComp t1 t2
listComp (OKArray _ t1) (OKArray _ t2) = listComp t1 t2
listComp (OKTuple ts1) (OKTuple ts2) =
    length ts1 == length ts2 && all (uncurry listComp) (zip ts1 ts2)
listComp (OKList t1) (OKList t2) = listComp t1 t2
listComp OKVoid _ = True
listComp _ OKVoid = True
listComp t1 t2 = t1 == t2






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


