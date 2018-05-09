module Tokens where

type Pos = (Int, Int)

data Token =

  MainThemeTkn        { tknPos :: (Int, Int) }  |
  YouBeginTkn         { tknPos :: (Int, Int) }  |
  WhereIEndTkn        { tknPos :: (Int, Int) }  |
  IfTkn               { tknPos :: (Int, Int) }  |
  IfYouHaveToAskTkn   { tknPos :: (Int, Int) }  |
  OthersideTkn        { tknPos :: (Int, Int) }  |
  CantStopTkn         { tknPos :: (Int, Int) }  |
  BreakthruTkn        { tknPos :: (Int, Int) }  |
  OneMoreTimeTkn      { tknPos :: (Int, Int) }  |
  SemiColonTkn        { tknPos :: (Int, Int) }  |
  ReadMyMindTkn       { tknPos :: (Int, Int) }  |
  GoTkn               { tknPos :: (Int, Int) }  |
  GoSlowlyTkn         { tknPos :: (Int, Int) }  |
  NewOrderTkn         { tknPos :: (Int, Int) }  |
  DaFunkTkn           { tknPos :: (Int, Int) }  |
  GetBackTkn          { tknPos :: (Int, Int) }  |
  IntoTheVoidTkn      { tknPos :: (Int, Int) }  |
  NewLifeTkn          { tknPos :: (Int, Int) }  |
  SaveMeTkn           { tknPos :: (Int, Int) }  |
  KeepAliveTkn        { tknPos :: (Int, Int) }  |
  AmnesiacTkn         { tknPos :: (Int, Int) }  |
  ExitMusicTkn        { tknPos :: (Int, Int) }  |
  AroundTheWorldTkn   { tknPos :: (Int, Int) }  |
  HoleInMySoulTkn     { tknPos :: (Int, Int) }  |
  IntTkn              { tknPos :: (Int, Int) }  |
  FloatTkn            { tknPos :: (Int, Int) }  |
  CharTkn             { tknPos :: (Int, Int) }  |
  BooleanTkn          { tknPos :: (Int, Int) }  |
  OkTkn               { tknPos :: (Int, Int) }  |
  NotOkTkn            { tknPos :: (Int, Int) }  |
  ArrayStartTkn       { tknPos :: (Int, Int) }  |
  ArrayEndTkn         { tknPos :: (Int, Int) }  |
  BandTkn             { tknPos :: (Int, Int) }  |
  UnionTkn            { tknPos :: (Int, Int) }  |
  PointerTkn          { tknPos :: (Int, Int) }  |
  DuetsTkn            { tknPos :: (Int, Int) }  |
  LeftTkn             { tknPos :: (Int, Int) }  |
  RightTkn            { tknPos :: (Int, Int) }  |
  ModTkn              { tknPos :: (Int, Int) }  |
  DivTkn              { tknPos :: (Int, Int) }  |
  NotTkn              { tknPos :: (Int, Int) }  |
  AndTkn              { tknPos :: (Int, Int) }  |
  OrTkn               { tknPos :: (Int, Int) }  |
  CommaTkn            { tknPos :: (Int, Int) }  |
  ParenOpenTkn        { tknPos :: (Int, Int) }  |
  ParenCloseTkn       { tknPos :: (Int, Int) }  |
  SemicolonTkn        { tknPos :: (Int, Int) }  |
  PlusTkn             { tknPos :: (Int, Int) }  |
  EqualTkn            { tknPos :: (Int, Int) }  |
  ProductTkn          { tknPos :: (Int, Int) }  |
  MinusTkn            { tknPos :: (Int, Int) }  |
  RestTkn             { tknPos :: (Int, Int) }  |
  DivExacTkn          { tknPos :: (Int, Int) }  |
  DifTkn              { tknPos :: (Int, Int) }  |
  GreaterEqualTkn     { tknPos :: (Int, Int) }  |
  LessEqualTkn        { tknPos :: (Int, Int) }  |
  GreaterTkn          { tknPos :: (Int, Int) }  |
  LessTkn             { tknPos :: (Int, Int) }  |
  TypeTkn             { tknPos :: (Int, Int) }  |
  AssignTkn           { tknPos :: (Int, Int) }  |
  NumLiteralTkn       { tknPos :: (Int, Int), tknString :: String } |
  IdTkn               { tknPos :: (Int, Int), tknString :: String } |
  StringTkn           { tknPos :: (Int, Int), tknString :: String }

  deriving Show
