{

module Parser (parse, JsonExpr) where

import qualified Lexer as L

}

%name parse
%tokentype { L.Token L.Pos }
%error { parseError }

%token
    lbrace    { L.LBrace       }
    rbrace    { L.RBrace       }
    lbracket  { L.LBracket     }
    rbracket  { L.RBracket     }
    colon     { L.Colon        }
    comma     { L.Comma        }
    strlit    { L.StringLit $$ }
    numlit    { L.NumLit $$    }

%%

Value  : Num                                 { $1 }
       | String                              { $1 }
       | List                                { $1 }
       | Object                              { $1 }
Num    : numlit                              { JsonNum $1 }
String : strlit                              { JsonStr $1 }
List   : lbracket rbracket                   { JsonList [] }
       | lbracket ListContents rbracket      { JsonList $2 }
ListContents : Value                         { [$1]    }
             | ListContents comma Value      { $3 : $1 }

KeyValue : strlit colon Value                { ($1, $3) }
KeyValuePairs : KeyValue                     { [$1] }
              | KeyValuePairs comma KeyValue { $3 : $1 }
Object : lbrace rbrace                       { JsonObject [] }
       | lbrace KeyValuePairs rbrace         { JsonObject $2 }

{

parse :: [L.Token L.Pos] -> JsonExpr L.Pos

parseError = error "Parse error"

data JsonExpr a = JsonNum Integer a
                | JsonStr String a
                | JsonList [JsonExpr] a
                | JsonObject [(String, JsonExpr)] a
  deriving Show

}
