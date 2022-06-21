{

module Parser (parse, JsonExpr) where

import Control.Monad.Except
import Data.List (intersperse)

import qualified Lexer as L

}

%name parse
%tokentype { L.Token L.Pos }
%error { parseError }
%monad { Either String } { (>>=) } { return }

%token
    lbrace    { L.LBrace $$          }
    rbrace    { L.RBrace $$          }
    lbracket  { L.LBracket $$        }
    rbracket  { L.RBracket $$        }
    colon     { L.Colon $$           }
    comma     { L.Comma $$           }
    strlit    { $$@(L.StringLit _ _) }
    numlit    { $$@(L.NumLit _ _)    }

%%

Value  : Num                                 { $1 }
       | String                              { $1 }
       | List                                { $1 }
       | Object                              { $1 }
Num    : numlit                              { mkNum $1 }
String : strlit                              { mkStr $1 }
List   : lbracket rbracket                   { JsonList $1 [] }
       | lbracket ListContents rbracket      { JsonList $1 (reverse $2) }
ListContents : Value                         { [$1]    }
             | ListContents comma Value      { $3 : $1 }

KeyValue : strlit colon Value                { (extractStr $1, $3) }
KeyValuePairs : KeyValue                     { [$1] }
              | KeyValuePairs comma KeyValue { $3 : $1 }
Object : lbrace rbrace                       { JsonObject $1 [] }
       | lbrace KeyValuePairs rbrace         { JsonObject $1 (reverse $2) }

{

parseError tokens = Left "parse error"

data JsonExpr a = JsonNum a Integer
                | JsonStr a String
                | JsonList a [JsonExpr a]
                | JsonObject a [(String, JsonExpr a)]
  deriving Show

-- instance Show (JsonExpr a) where
--   show (JsonNum _ i) = show i
--   show (JsonStr _ s) = show s
--   show (JsonList _ l) = "[" ++ (concat $ intersperse ", " $ show <$> l) ++ "]"
--   show (JsonObject _ p) = "{" ++ (concat $ intersperse ", " $ colonPair <$> p) ++ "}"
--     where colonPair (s, e) = show s ++ ": " ++ show e

mkNum (L.NumLit p v) = JsonNum p v
mkStr (L.StringLit p v) = JsonStr p v
extractStr (L.StringLit _ s) = s
}
