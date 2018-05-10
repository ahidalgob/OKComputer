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
  saveme                                  { SaveMeTkn}          -- Malloc
  keepyourselfalive                       { KeepAliveTkn}       -- Realloc
  amnesiac                                { AmnesiacTkn}        -- Free
  exitmusic                               { ExitMusicTkn}       -- Exit
  aroundtheworld                          { AroundTheWorldTkn}  -- Import
  holeinmysoul                            { HoleInMySoulTkn}    -- Templates

  -- Type Tokens
  int                                     { IntTkn}
  float                                   { FloatTkn}
  char                                    { CharTkn}
  boolean                                 { BooleanTkn}
  ok                                      { OkTkn}              -- True
  notok                                   { NotOkTkn}           -- False
  '['                                      { ArrayStartTkn}
  ']'                                      { ArrayEndTkn}
  band                                    { BandTkn}            -- Registers/structs
  union                                   { UnionTkn}
  '&'                                      { PointerTkn}         -- Pointers
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
  '->'                                    { TypeTkn}
  '='                                      { AssignTkn}

  -- Otros
  id 									  { IdTkn _ _}
  n 									  { NumLiteralTkn _ _}
  newline 								  { }
  string								  { }


%left or
%left and
%nonassoc '>' '<' '==' '!=' '>=' '<=' 
%left '+' '-'
%left '*' '/' '%' mod div
%nonassoc not

%%
-- Start
START : IMPORTS LDECLARACIONES LFUNCIONES { }
	  | LDECLARACIONES LFUNCIONES		  { }
	  | LFUNCIONES						  { }

IMPORTS : IMPORT newline IMPORTS	{ }
		| IMPORT			{ }

IMPORT : aroundtheworld	IDS		{ }

LDECLARACIONES : DECLARACION newline LDECLARACIONES { }
			   | DECLARACION					{ }

DECLARACION : TIPO IDS { }
			| TIPO IDS '=' EXPRESION { }
			| TIPO IDS '=' EXPRESION ',' DECLARACION { }
			| TUPLA { }
			| ARREGLO { }
			| ESTRUCTURA { }

LFUNCIONES : FUNCION newline LFUNCIONES { }
		   | FUNCION { }

FUNCION : youbegin whereiend { }

IDS : id ',' IDS { }
	| id 	{ }

TIPO : int { }
	 | float { }
	 | boolean { }
	 | char { }
	 | string { }

ARREGLO : TIPO id '[' n ']'		{ }
		| TIPO id '[' id ']'	{ }

ESTRUCTURA : band id id '(' LDECLARACIONES ')'		{ }
		   | union id id '(' LDECLARACIONES ')'	{ }
		   | id id '(' LPARAMETROS ')'	{ }

TUPLA : duets id '(' TIPO ',' TIPO ',' n ')'		{ }
	  | duets id '(' TIPO ',' TIPO ',' id ')' 	{ }

EXPRESION : n 	{ }

LPARAMETROS : right 	{ }