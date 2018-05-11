-- OKComputer
-- Parser

{
	module Parser where
	import Lexer
}

%name parse
%tokentype { Token }

%token
  youbegin                                { YouBeginTkn _ }        -- Block Start
  whereiend                               { WhereIEndTkn _ }       -- Block End
  if                                      { IfTkn $$ }              -- Selection
  ifyouhavetoask                          { IfYouHaveToAskTkn $$ }  -- Selection
  otherside                               { OthersideTkn _ }       -- Selection
  cantstop                                { CantStopTkn $$ }        -- While Iteration
  breakthru                               { BreakthruTkn _ }       -- Break
  onemoretime                             { OneMoreTimeTkn $$ }     -- For Iteration
  ';'                                     { SemiColonTkn _ }       -- For Iteration
  readmymind                              { ReadMyMindTkn $$ }      -- Data entry/read
  go                                      { GoTkn $$ }              -- Data exit/write
  gomental								  { GoMental $$ }
  goslowly                                { GoSlowlyTkn $$ }        -- Data exit/writeln
  dafunk                                  { DaFunkTkn $$}          -- Method with return/Function
  ':'    	                              { ColonTkn}           -- Method with return/Function
  getback                                 { GetBackTkn}         -- Return
  intothevoid                             { IntoTheVoidTkn}     -- Void
  newlife                                 { NewLifeTkn}         -- Calloc
--  saveme                                  { SaveMeTkn}          -- Malloc
--  keepyourselfalive                       { KeepAliveTkn}       -- Realloc
  amnesiac                                { AmnesiacTkn}        -- Free
  exitmusic                               { ExitMusicTkn}       -- Exit
  aroundtheworld                          { AroundTheWorldTkn}  -- Import
--  holeinmysoul                            { HoleInMySoulTkn}    -- Templates

  -- Type Tokens
  int                                     { IntTkn}
  pointer 								  { PointerTkn } 		-- Apuntador, agregar a LEXER
  float                                   { FloatTkn}
  char                                    { CharTkn}
  boolean                                 { BooleanTkn}
  ok                                      { OkTkn}              -- True
  notok                                   { NotOkTkn}           -- False
  '['                                      { ArrayStartTkn}
  ']'                                      { ArrayEndTkn}
  band                                    { BandTkn}            -- Registers/structs
  union                                   { UnionTkn}
--  '&'                                      { PointerTkn}         -- Pointers
  duets                                   { DuetsTkn}           -- Tuple
  left                                    { LeftTkn}
  right                                   { RightTkn}

  -- Operations Tokens
  mod                                     { ModTkn}
  div                                     { DivTkn}
  not                                     { NotTkn}
  and                                     { AndTkn}
  or                                      { OrTkn}
  ','                                      { CommaTkn}
  '('                                      { ParenOpenTkn}
  ')'                                      { ParenCloseTkn}
  '+'                                     { PlusTkn}
  '=='                                    { EqualTkn}
  '*'                                     { ProductTkn}
  '-'                                      { MinusTkn}
  '%'                                      { RestTkn}
  '/'                                      { DivExacTkn}
  '!='                                    { DifTkn}
  '>='                                    { GreaterEqualTkn}
  '<='                                    { LessEqualTkn}
  '>'                                      { GreaterTkn}
  '<'                                      { LessTkn}
 -- '->'                                    { TypeTkn}
  '='                                      { AssignTkn}

  -- Otros
  id 									  { IdTkn _ _}
  n 									  { NumLiteralTkn _ _}
  newline 								  { }
  c 									  { } -- char
  string								  { }


%left or
%left and
%nonassoc '>' '<' '==' '!=' '>=' '<=' 
%left '+' '-'
%left '*' '/' '%' mod div
%nonassoc not

%%
-- Start
START : IMPORTS newline LDECLARACIONES newline LFUNCIONES { }
	  | LDECLARACIONES newline LFUNCIONES		  { }
	  | LFUNCIONES						  { }

IMPORTS : IMPORT IMPORTS	{ }
		| IMPORT			{ }

IMPORT : aroundtheworld	IDS	newline	{ }

LDECLARACIONES : DECLARACION LDECLARACIONES { }
			   | DECLARACION					{ }

DECLARACION : TIPO DECLARACIONTIPO newline { }
				| TUPLA { }
				| ARREGLO { }
				| ESTRUCTURA { }
        		| newlife id { }      -- No estoy claro todavia como haremos esto

-- DECLARACION : IDS 	{ }
--			| IDS '=' EXPRESION 	{ }
--			| IDS '=' EXPRESION ',' DECLARACION 	{ }

DECLARACIONTIPO : id '=' EXPRESION { }
            | id '=' EXPRESION ',' DECLARACIONTIPO { }
            | id                 { }
            | id ',' DECLARACIONTIPO { }

LFUNCIONES : FUNCIONINIC newline LFUNCIONES { }
		   | FUNCIONINIC { }

-- Probablemente vaya newline antes del youbegin y whereiend

FUNCIONINIC : dafunk id '(' LPARAMETROSFUNC ')' ':' PARAMETROFUNCION newline youbegin newline DENTROFUNCION newline whereiend  { }
			| dafunk id '(' LPARAMETROSFUNC ')' ':' intothevoid newline youbegin newline DENTROFUNCION newline whereiend  { }
			| dafunk id '(' ')' ':' PARAMETROFUNCION newline youbegin newline DENTROFUNCION newline whereiend  { }
			| dafunk id '(' ')' ':' intothevoid newline youbegin newline DENTROFUNCION newline whereiend { }

PARAMETROFUNCION : TIPO id { }
			  	 | TIPO id '[' ']'	{ }
			  	 | duets id '(' ')'		{ }
			  	 | id id 		{ } 			-- Structs

DENTROFUNCION : DECLARACION DENTROFUNCION { }
		 	  | INSTRUCCION DENTROFUNCION { }
		 	  | DECLARACION 			   { }
		 	  | INSTRUCCION 			   { }

LPARAMETROSFUNC : PARAMETROFUNCION ',' LPARAMETROSFUNC { }
				| PARAMETROFUNCION 	{ }

-- Probablemente vaya newline antes del youbegin y whereiend PUESTOS
INSTRUCCION : go '(' IMPRIMIR ')' newline	{ }
			| goslowly '(' IMPRIMIR ')' newline	{ }
			| gomental '(' IMPRIMIR ')'	newline	{ }
      		| amnesiac '(' id ')' newline { }
     		| readmymind '(' id ')' newline { }
     		| if EXPRESION newline youbegin newline DENTROFUNCION whereiend newline { }
      		| if EXPRESION newline youbegin newline DENTROFUNCION whereiend newline IFELSE { }      -- No se si necesitaria newline
      		| cantstop EXPRESION newline youbegin newline DENTROFUNCION newline whereiend newline { }
      		| onemoretime TIPO id '=' EXPRESION ';' EXPRESION ';'EXPRESION newline youbegin newline DENTROFUNCION whereiend newline { }
      		| id '=' EXPRESION newline { }
      		| getback EXPRESION newline { }
      		| breakthru newline	{ }
      		| exitmusic newline	{ }



IFELSE : ifyouhavetoask EXPRESION newline youbegin newline DENTROFUNCION whereiend newline IFELSE { }
       | otherside newline youbegin newline DENTROFUNCION newline whereiend newline{ }

-- ONEMORETIMEDEC : TIPO id '=' E

IMPRIMIR : string ',' IMPRIMIR 	{ }
		 | id ',' 	  IMPRIMIR  { }
		 | string 				{ }
		 | id 					{ }

IDS : id ',' IDS { }
	| id 	{ }

TIPO : int { }
	 | pointer 	{ }
	 | float { }
	 | boolean { }
	 | char { }
	 | string { }

ARREGLO : TIPO id '[' n ']'		{ }
		| TIPO id '[' id ']'	{ }

ESTRUCTURA : band id id '(' LDECLARACIONES ')'		{ }
		   | union id id '(' LDECLARACIONES ')'	{ }
		   | id id '(' LPARAMETROSESTRUC ')'	{ }

TUPLA : duets id '(' TIPO ',' TIPO ',' n ')'		{ }
	  | duets id '(' TIPO ',' TIPO ',' id ')' 	{ }

EXPRESION : id 	{ }
		  | n 	{ }
		  | string	{ }
		  | c 	{ }
		  | ok 	{ }
		  | notok 	{ }
		  | '(' EXPRESION ')' { }
		  | EXPRESION '<' EXPRESION { }
		  | EXPRESION '>' EXPRESION { }
		  | EXPRESION '<=' EXPRESION { }
		  | EXPRESION '>=' EXPRESION { }
		  | EXPRESION '==' EXPRESION { }
		  | EXPRESION '!=' EXPRESION { }
		  | not EXPRESION { }
		  | EXPRESION and EXPRESION { }
		  | EXPRESION or EXPRESION { }
		  | '-' EXPRESION	{ }
		  | EXPRESION '+' EXPRESION { }
		  | EXPRESION '-' EXPRESION { }
		  | EXPRESION '*' EXPRESION { }
		  | EXPRESION '/' EXPRESION { }
		  | EXPRESION '%' EXPRESION { }
		  | EXPRESION mod EXPRESION { }
		  | EXPRESION div EXPRESION { }
		  | EXPRESIONTUPLA 			{ }
		  | EXPRESIONARREGLO 		{ }
		  | EXPRESIONESTRUCTURA 	{ }

EXPRESIONTUPLA : left id '(' n ')'	{ } -- x = left tupla1(2)
			   | right id '(' n ')'	{ } -- x = right tupla1(1)
			   | left id '(' id ')'	{ } 
			   | right id '(' id ')'	{ } 

EXPRESIONARREGLO : id '[' n ']'		{ }
				 | id '[' id ']'	{ }

EXPRESIONESTRUCTURA : id '(' id ')' 	{ }

LPARAMETROSESTRUC : id '=' EXPRESION ',' LPARAMETROSESTRUC 	{ }
				  | id '=' EXPRESION 		{ }

