{
{-# OPTIONS_GHC -w #-}
module Deftab.Lexer (alexScanTokens, Token(..),
                     FilePos, line, col)

where

import Data.Char ( isSpace )
import Deftab.ParserTypes
import qualified Data.ByteString.Char8 as C
}

%wrapper "posn"

$digit = [0-9]                  -- digits
$alpha = [a-zA-Z]               -- alphabetic characters
$alphaNum = [$alpha$digit]

@int = \-?$digit+

hyloTokens :-
  $white+                   ;       -- strip whitspaces
  \%.*                      ;       -- strip comments
  \{ (~\} | \n)* \}         ;       -- strip multiple line comments

  intuitionistic            { discardValue TokenIntuitionistic}
  classical                 { discardValue TokenClassical}
  facts:                    { discardValue TokenFacts}
  defaults:                 { discardValue TokenDefaults}
  consequence:              { discardValue TokenConsequence}
  fof\([^\,]+\,             { discardValue TokenFof }
  axiom                     { discardValue TokenAxiom }
  conjecture                { discardValue TokenConjecture }

  \<\>                      { discardValue TokenDia }
  dia                       { discardValue TokenDia }

  \[\]                      { discardValue TokenBox }
  box                       { discardValue TokenBox }

  \:                        { discardValue TokenAt  }
  \@                        { discardValue TokenAt2 }

  down                      { discardValue TokenDown }

  A                         { discardValue TokenA }
  E                         { discardValue TokenE }

  true                      { discardValue TokenTrue  }
  T                         { discardValue TokenTrue  }
  false                     { discardValue TokenFalse }
  F                         { discardValue TokenFalse }

  [v\|]                     { discardValue TokenOr   }
  [&\^]                     { discardValue TokenAnd  }
  [\!\-\~]                  { discardValue TokenNeg  }
  "->"                      { discardValue TokenImp  }
  "=>"                      { discardValue TokenImp  }
  "<->"                     { discardValue TokenDimp }
  "<=>"                     { discardValue TokenDimp }

  "("                       { discardValue TokenOB }
  ")"                       { discardValue TokenCB }

  \.                        { discardValue TokenDot}

  "-->"                     { discardValue TokenRuleSep }

  \,                        { discardValue TokenComma }
  \;                        { discardValue TokenSC }

  [Pp]@int                  { storeValue (TokenProp . prop  . C.pack . tail) }
  [Nn]@int                  { storeValue (TokenNom  . nom   . C.pack . tail) }
  [a-z_]+@int?              { storeValue (TokenProp . prop . C.pack ) }

{
data Token = TokenIntuitionistic  | TokenClassical
           | TokenBegin           | TokenEnd
           | TokenTrue            | TokenFalse
           | TokenProp Atom       | TokenNom Atom
           | TokenNeg             | TokenAnd            | TokenOr
           | TokenAt              | TokenAt2
           | TokenDown
           | TokenBox             | TokenDia
           | TokenImp             | TokenDimp
           | TokenA               | TokenE
           | TokenOB              | TokenCB
           | TokenRuleSep
           | TokenSC              | TokenComma
           | TokenFacts           | TokenDefaults       | TokenConsequence
           | TokenFof             | TokenDot
           | TokenAxiom           | TokenConjecture
  deriving (Eq, Show)

data FilePos = FP{line :: Int, col :: Int} deriving (Eq, Show)

makeFilePos (AlexPn _ l c) = FP{line = l, col = c}

-- for building tokens which hold a value
storeValue     mkToken alexPos v = (mkToken v, makeFilePos alexPos)

-- for building tokens which discard the parsed string
discardValue   token alexPos _ = (token, makeFilePos alexPos)

prop   = P
nom    = N
}
