-- Retina - Proyecto de Traductores
-- Parser
-- Augusto Hidalgo 13-10665
-- Genesis Kufatty 13-10708

{
module Parser where
import Lexer
import AST
}

%name parse
%tokentype { Token }

%token
    while           { WhileTK $$ }
    for             { ForTK $$ }
    from            { FromTK _ } 
    to              { ToTK _ }
    by              { ByTK _ } 
    begin           { BeginTK _ }
    func            { FuncTK $$ } 
    return          { ReturnTK $$ } 
    repeat          { RepeatTK $$ }
    program         { ProgramTK _ }
    with            { WithTK $$ } 
    do              { DoTK _ } 
    end             { EndTK _ }
    times           { TimesTK _ }
    not             { NotTK $$ } 
    and             { AndTK $$ } 
    or              { OrTK $$ } 
    read            { ReadTK $$ }
    write           { WriteTK $$ }
    writeln         { WritelnTK $$ }
    if              { IfTK $$ } 
    then            { ThenTK _ } 
    else            { ElseTK _ }
    number          { NumberTK _ }
    boolean         { BooleanTK _ }
    true            { TrueTK $$ }
    false           { FalseTK $$ } 
    div             { DivTK $$ } 
    mod             { ModTK $$ } 
    ','             { CommaTK _ } 
    n               { NumLiteralTK _ _ }
    string          { StringTK _ $$ }
    id              { IdTK _ _ } 
    fid             { FuncIdTK _ _ } 
    '+'             { PlusTK $$ } 
    '=='            { EqualTK $$ } 
    '*'             { ProductTK $$ } 
    '-'             { MinusTK $$ } 
    '%'             { RestTK $$ } 
    '/'             { DivExacTK $$ } 
    '/='            { DifTK $$ } 
    '>='            { GreaterEqualTK $$ } 
    '<='            { LessEqualTK $$ }
    '>'             { GreaterTK $$ }
    '<'             { LessTK $$ }
    '='             { AssignTK $$ } 
    '('             { ParenOpenTK $$ }
    ')'             { ParenCloseTK _ }
    ';'             { SemicolonTK _ }
    '->'            { TypeTK _ }


%left or
%left and
%nonassoc '>' '<' '==' '/=' '>=' '<=' 
%left '+' '-'
%left '*' '/' '%' mod div
%nonassoc not

%%
-- Program
P : LDF program LI end ';'  { PN $1 $3 }

-- Lista de definicion de funciones
LDF : DF LDF    { LDFN $ $1 : listLDFN $2 }
    | {-empty-} { LDFN [] }

-- Definicion de funcion
DF  : func fid '(' LP ')' begin LI end ';'         { DFN (tokenString $2) $4 $7 $1 }
    | func fid '(' LP ')' '->' T begin LI end ';'  { RDFN (tokenString $2) $4 $7 $9 $1 }

-- Lista de parametros
LP : {-empty-}      { LPN [] }
   | LPNV           { LPN $ listLPNVN $1 }
   
-- Lista de Parametros no vacia (construccion auxiliar para manejar las comas)
LPNV : T id             { LPNVN [($1, (tokenString $2))] }
     | T id ',' LPNV    { LPNVN $ ($1, (tokenString $2)) : listLPNVN $4 }

-- Tipo
T : boolean { BooleanN }
  | number  { NumberN }
  
-- Expresion (Construcciones que evaluan boolean o number
--             (o llamados a funciones que pueden evaluar void))
E   : id { IdN (tokenString $1) (tokenPos $1) }
    | true { TrueN $1 }
    | false { FalseN $1 }
    | '(' E ')' { ParN $2 $1 }
    | E '<' E { ComparN $1 "<" $3 $2 }
    | E '>' E { ComparN $1 ">" $3 $2 }
    | E '<=' E { ComparN $1 "<=" $3 $2 }
    | E '>=' E { ComparN $1 ">=" $3 $2 }
    | E '==' E { ComparN $1 "==" $3 $2 }
    | E '/=' E { ComparN $1 "/=" $3 $2 }
    | not E { NotN $2 $1 }
    | E and E { LogicN $1 "and" $3 $2 }
    | E or E { LogicN $1 "or" $3 $2 }
    | fid '(' LE ')' { FuncN (tokenString $1) $3 (tokenPos $1) }
    | '-' E         { MinusN $2 $1 }
    | E '+' E       { AritN $1 "+" $3 $2 }
    | E '-' E       { AritN $1 "-" $3 $2 }
    | E '*' E       { AritN $1 "*" $3 $2 }
    | E '/' E       { AritN $1 "/" $3 $2 }
    | E '%' E       { AritN $1 "%" $3 $2 }
    | E mod E       { AritN $1 "mod" $3 $2 }
    | E div E       { AritN $1 "div" $3 $2 }
    | n { NumberLiteralN (tokenString $1) (tokenPos $1) }

-- Lista de expresiones (argumentos de una funcion)
LE  : LENV      { LEN $ listLENVN $1 }
    | {-empty-} { LEN [] }

-- Lista de expresiones no vacia (para manejar las comas)
LENV    : E             { LENVN [$1] }
        | E ',' LENV    { LENVN $ $1 : listLENVN $3}

-- Lista de declaracion de variables ([(Tipo , lista de variables)])
LD  : T DST ';' LD  { LDN $ ($1, listLVN $2) : listLDN $4}
    | ';' LD        { $2 }
    | {-empty-}     { LDN [] }

-- Lista de variables (posiblemente con un valor asignados)
DST : id '=' E          {LVarN [VarValN (tokenString $1) $3]}
    | id                {LVarN [VarN (tokenString $1)]}
    | id '=' E ',' DST  {LVarN $ VarValN (tokenString $1) $3 : listLVN $5}
    | id ',' DST        {LVarN $ VarN (tokenString $1) : listLVN $3}

-- Lista de instrucciones
LI  : {-empty-}         { LIN [] }
    | I LI              { LIN $ $1 : listLIN $2}
    | ';' LI            { $2 }

-- Instruccion
I   : with LD do LI end ';'                 { WithDoN $2 $4 $1 }
    | repeat E times LI end ';'             { RepeatN $2 $4 $1 }
    | id '=' E ';'                          { AssignN (tokenString $1) $3 (tokenPos $1)}
    | for id from E to E do LI end ';'      { ForN (tokenString $2) $4 $6 $8 $1}
    | for id from E to E by E do LI end ';' { ForByN (tokenString $2) $4 $6 $8 $10 $1 }
    | if E then LI end ';'                  { IfThenN $2 $4 $1 }
    | if E then LI else LI end ';'          { IfThenElseN $2 $4 $6 $1 }
    | while E do LI end ';'                 { WhileN $2 $4 $1}
    | write string LPW                      { WriteN (PWSN $2 : listLPWN $3) $1 }
    | write E LPW                           { WriteN (PWEN $2 : listLPWN $3) $1 }
    | writeln string LPW                    { WritelnN (PWSN $2 : listLPWN $3) $1 }
    | writeln E LPW                         { WritelnN (PWEN $2 : listLPWN $3) $1 }
    | read id ';'                           { ReadN (tokenString $2) $1 }
    | return E ';'                          { ReturnN $2 $1 }
    | E ';'                                 { ExprN $1 }
    
-- Lista de Imprimibles (expresiones o strings)
LPW : ';'                               { LPWN [] }
    | ',' string LPW                    { LPWN $ PWSN $2 : listLPWN $3 }
    | ',' E LPW                         { LPWN $ PWEN $2 : listLPWN $3 }

{

happyError :: [Token] -> a
happyError tks = error ("Error en el parser en " ++ lcn ++ "\n" ++ "happy is sad :(")
    where
        lcn =   case tks of
                    [] -> "el final del archivo"
                    tk:_ -> "linea " ++ show l ++ ", columna " ++ show c ++ "\n" ++ show tk
                        where
                            (l, c) = tokenPos tk
}
