-- OKComputer
-- Parser

{
	module Parser where
	import Lexer
    import ParseMonad
    import Tokens
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
START : IMPORTS newline LDECLARATIONS newline LFUNCTIONS { }
	  | LDECLARATIONS newline LFUNCTIONS		  { }
	  | LFUNCTIONS						  { }

IMPORTS : IMPORT IMPORTS	{ }
		| IMPORT			{ }

IMPORT : aroundtheworld	IDS	newline	{ }

LDECLARATIONS : DECLARATION LDECLARATIONS { }
			   | DECLARATION					{ }

DECLARATION : TYPE DECLARATIONTYPE newline { }
				| TUPLE { }
				| ARRAY { }
				| STRUCT { }
        		| newlife id { }      -- No estoy claro todavia como haremos esto

-- DECLARATION : IDS 	{ }
--			| IDS '=' EXPRESSION 	{ }
--			| IDS '=' EXPRESSION ',' DECLARATION 	{ }

DECLARATIONTYPE : id '=' EXPRESSION { }
            | id '=' EXPRESSION ',' DECLARATIONTYPE { }
            | id                 { }
            | id ',' DECLARATIONTYPE { }

LFUNCTIONS : FUNCIONINIC newline LFUNCTIONS { }
		   | FUNCIONINIC { }

-- Probablemente vaya newline antes del youbegin y whereiend

FUNCIONINIC : dafunk id '(' LPARAMETERSFUNC ')' ':' PARAMETROFUNCION newline youbegin newline INSIDEFUNCTION newline whereiend  { }
			| dafunk id '(' LPARAMETERSFUNC ')' ':' intothevoid newline youbegin newline INSIDEFUNCTION newline whereiend  { }
			| dafunk id '(' ')' ':' PARAMETROFUNCION newline youbegin newline INSIDEFUNCTION newline whereiend  { }
			| dafunk id '(' ')' ':' intothevoid newline youbegin newline INSIDEFUNCTION newline whereiend { }

PARAMETROFUNCION : TYPE id { }
			  	 | TYPE id '[' ']'	{ }
			  	 | duets id '(' ')'		{ }
			  	 | id id 		{ } 			-- Structs

INSIDEFUNCTION : DECLARATION INSIDEFUNCTION { }
		 	  | INSTRUCTION INSIDEFUNCTION { }
		 	  | DECLARATION 			   { }
		 	  | INSTRUCTION 			   { }

LPARAMETERSFUNC : PARAMETROFUNCION ',' LPARAMETERSFUNC { }
				| PARAMETROFUNCION 	{ }

-- Probablemente vaya newline antes del youbegin y whereiend PUESTOS
INSTRUCTION : go '(' PRINT ')' newline	{ }
			| goslowly '(' PRINT ')' newline	{ }
			| gomental '(' PRINT ')'	newline	{ }
      		| amnesiac '(' id ')' newline { }
     		| readmymind '(' id ')' newline { }
     		| if EXPRESSION newline youbegin newline INSIDEFUNCTION whereiend newline { }
      		| if EXPRESSION newline youbegin newline INSIDEFUNCTION whereiend newline IFELSE { }      -- No se si necesitaria newline
      		| cantstop EXPRESSION newline youbegin newline INSIDEFUNCTION newline whereiend newline { }
      		| onemoretime TYPE id '=' EXPRESSION ';' EXPRESSION ';'EXPRESSION newline youbegin newline INSIDEFUNCTION whereiend newline { }
      		| id '=' EXPRESSION newline { }
      		| getback EXPRESSION newline { }
      		| breakthru newline	{ }
      		| exitmusic newline	{ }



IFELSE : ifyouhavetoask EXPRESSION newline youbegin newline INSIDEFUNCTION whereiend newline IFELSE { }
       | otherside newline youbegin newline INSIDEFUNCTION newline whereiend newline{ }

-- ONEMORETIMEDEC : TYPE id '=' E

PRINT : string ',' PRINT 	{ }
		 | id ',' 	  PRINT  { }
		 | string 				{ }
		 | id 					{ }

IDS : id ',' IDS { }
	| id 	{ }

TYPE : int { }
	 | pointer 	{ }
	 | float { }
	 | boolean { }
	 | char { }
	 | string { }

ARRAY : TYPE id '[' n ']'		{ }
		| TYPE id '[' id ']'	{ }

STRUCT : band id id '(' LDECLARATIONS ')'		{ }
		   | union id id '(' LDECLARATIONS ')'	{ }
		   | id id '(' LPARAMETERSSTRUCT ')'	{ }

TUPLE : duets id '(' TYPE ',' TYPE ',' n ')'		{ }
	  | duets id '(' TYPE ',' TYPE ',' id ')' 	{ }

EXPRESSION : id 	{ }
		  | n 	{ }
		  | string	{ }
		  | c 	{ }
		  | ok 	{ }
		  | notok 	{ }
		  | '(' EXPRESSION ')' { }
		  | EXPRESSION '<' EXPRESSION { }
		  | EXPRESSION '>' EXPRESSION { }
		  | EXPRESSION '<=' EXPRESSION { }
		  | EXPRESSION '>=' EXPRESSION { }
		  | EXPRESSION '==' EXPRESSION { }
		  | EXPRESSION '!=' EXPRESSION { }
		  | not EXPRESSION { }
		  | EXPRESSION and EXPRESSION { }
		  | EXPRESSION or EXPRESSION { }
		  | '-' EXPRESSION	{ }
		  | EXPRESSION '+' EXPRESSION { }
		  | EXPRESSION '-' EXPRESSION { }
		  | EXPRESSION '*' EXPRESSION { }
		  | EXPRESSION '/' EXPRESSION { }
		  | EXPRESSION '%' EXPRESSION { }
		  | EXPRESSION mod EXPRESSION { }
		  | EXPRESSION div EXPRESSION { }
		  | EXPRESSIONTUPLE 			{ }
		  | EXPRESSIONARRAY 		{ }
		  | EXPRESSIONSTRUCT 	{ }

EXPRESSIONTUPLE : left id '(' n ')'	{ } -- x = left tupla1(2)
			   | right id '(' n ')'	{ } -- x = right tupla1(1)
			   | left id '(' id ')'	{ }
			   | right id '(' id ')'	{ }

EXPRESSIONARRAY : id '[' n ']'		{ }
				 | id '[' id ']'	{ }

EXPRESSIONSTRUCT : id '(' id ')' 	{ }

LPARAMETERSSTRUCT : id '=' EXPRESSION ',' LPARAMETERSSTRUCT 	{ }
				  | id '=' EXPRESSION 		{ }

