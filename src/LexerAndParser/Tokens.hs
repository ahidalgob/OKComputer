module Tokens where

data Token =
  TypedefTkn          { tkn_pos :: (Int, Int) }  |
  YouBeginTkn         { tkn_pos :: (Int, Int) }  |
  WhereIEndTkn        { tkn_pos :: (Int, Int) }  |
  IfTkn               { tkn_pos :: (Int, Int) }  |
  IfYouHaveToAskTkn   { tkn_pos :: (Int, Int) }  |
  OthersideTkn        { tkn_pos :: (Int, Int) }  |
  CantStopTkn         { tkn_pos :: (Int, Int) }  |
  BreakthruTkn        { tkn_pos :: (Int, Int) }  |
  OneMoreTimeTkn      { tkn_pos :: (Int, Int) }  |
  OpenBraceTkn        { tkn_pos :: (Int, Int) }  |
  CloseBraceTkn       { tkn_pos :: (Int, Int) }  |
  DotTkn              { tkn_pos :: (Int, Int) }  |
  SemiColonTkn        { tkn_pos :: (Int, Int) }  |
  ReadMyMindTkn       { tkn_pos :: (Int, Int) }  |
  GoTkn               { tkn_pos :: (Int, Int) }  |
  GoMentalTkn         { tkn_pos :: (Int, Int) }  |
  GoSlowlyTkn         { tkn_pos :: (Int, Int) }  |
  DaFunkTkn           { tkn_pos :: (Int, Int) }  |
  ColonTkn            { tkn_pos :: (Int, Int) }  |
  GetBackTkn          { tkn_pos :: (Int, Int) }  |
  IntoTheVoidTkn      { tkn_pos :: (Int, Int) }  |
  NewLifeTkn          { tkn_pos :: (Int, Int) }  |
  SaveMeTkn           { tkn_pos :: (Int, Int) }  |
  KeepAliveTkn        { tkn_pos :: (Int, Int) }  |
  AmnesiacTkn         { tkn_pos :: (Int, Int) }  |
  ExitMusicTkn        { tkn_pos :: (Int, Int) }  |
  AroundTheWorldTkn   { tkn_pos :: (Int, Int) }  |
  HoleInMySoulTkn     { tkn_pos :: (Int, Int) }  |
  IntTkn              { tkn_pos :: (Int, Int) }  |
  FloatTkn            { tkn_pos :: (Int, Int) }  |
  CharTkn             { tkn_pos :: (Int, Int) }  |
  BooleanTkn          { tkn_pos :: (Int, Int) }  |
  StringTkn          { tkn_pos :: (Int, Int) }  |
  OkTkn               { tkn_pos :: (Int, Int) }  |
  NotOkTkn            { tkn_pos :: (Int, Int) }  |
  ArrayStartTkn       { tkn_pos :: (Int, Int) }  |
  ArrayEndTkn         { tkn_pos :: (Int, Int) }  |
  RecordTkn           { tkn_pos :: (Int, Int) }  |
  PointerTkn          { tkn_pos :: (Int, Int) }  |
  ModTkn              { tkn_pos :: (Int, Int) }  |
  DivTkn              { tkn_pos :: (Int, Int) }  |
  NotTkn              { tkn_pos :: (Int, Int) }  |
  AndTkn              { tkn_pos :: (Int, Int) }  |
  OrTkn               { tkn_pos :: (Int, Int) }  |
  CommaTkn            { tkn_pos :: (Int, Int) }  |
  ParenOpenTkn        { tkn_pos :: (Int, Int) }  |
  ParenCloseTkn       { tkn_pos :: (Int, Int) }  |
  PlusTkn             { tkn_pos :: (Int, Int) }  |
  EqualTkn            { tkn_pos :: (Int, Int) }  |
  ProductTkn          { tkn_pos :: (Int, Int) }  |
  MinusTkn            { tkn_pos :: (Int, Int) }  |
  RestTkn             { tkn_pos :: (Int, Int) }  |
  DivExacTkn          { tkn_pos :: (Int, Int) }  |
  DifTkn              { tkn_pos :: (Int, Int) }  |
  GreaterEqualTkn     { tkn_pos :: (Int, Int) }  |
  LessEqualTkn        { tkn_pos :: (Int, Int) }  |
  GreaterTkn          { tkn_pos :: (Int, Int) }  |
  LessTkn             { tkn_pos :: (Int, Int) }  |
  TypeTkn             { tkn_pos :: (Int, Int) }  |
  AssignTkn           { tkn_pos :: (Int, Int) }  |
  LiteralCharTkn      { tkn_pos :: (Int, Int), tkn_char :: Char } |
  IntLiteralTkn       { tkn_pos :: (Int, Int), tkn_string :: String } |
  FloatLiteralTkn       { tkn_pos :: (Int, Int), tkn_string :: String } |
  IdTkn               { tkn_pos :: (Int, Int), tkn_string :: String } |
  LiteralStringTkn           { tkn_pos :: (Int, Int), tkn_string :: String } |

  NewLineTkn          { tkn_pos :: (Int, Int) } |
  EOFTkn

  deriving Show
