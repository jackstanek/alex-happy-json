{

module Parser (parse, JsonExpr) where

import qualified Lexer

}

%name parse
%tokentype { Lexer.Token }
%error { parseError }

%token
    lbrace    { Lexer.LBrace       }
    rbrace    { Lexer.RBrace       }
    lbracket  { Lexer.LBracket     }
    rbracket  { Lexer.RBracket     }
    colon     { Lexer.Colon        }
    comma     { Lexer.Comma        }
    strlit    { Lexer.StringLit $$ }
    numlit    { Lexer.NumLit $$    }

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

parse :: [Lexer.Token] -> JsonExpr

parseError = error "Parse error"

data JsonExpr = JsonNum Integer
              | JsonStr String
              | JsonList [JsonExpr]
              | JsonObject [(String, JsonExpr)]
  deriving Show

-- TODO: A nicer Show instance for JsonExpr?
-- instance Show JsonExpr where
--   show (JsonNum n) = show n
--   show (JsonStr s) = s
--   show (JsonList es)  = "[" ++ unwords (((++ ",") . show) <$> es) "]"
--   show (JsonObject (kv:kvs)) = undefined

}
