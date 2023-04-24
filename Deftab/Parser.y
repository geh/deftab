{
{-# OPTIONS_GHC -w #-}

module Deftab.Parser ( parse )

where

import Deftab.Lexer ( Token(..), FilePos, line, col )
import Deftab.ParserTypes
import qualified Data.ByteString.Char8 as C
}

%name parse
%tokentype { (Token, FilePos) }

%token
             intuitionistic  { (TokenIntuitionistic , _) }
             classical       { (TokenClassical, _) }
             at              { (TokenAt       , _) }
             at2             { (TokenAt2      , _) }
             down            { (TokenDown     , _) }
             prop            { (TokenProp $$  , _) }
             nom             { (TokenNom $$   , _) }
             true            { (TokenTrue     , _) }
             false           { (TokenFalse    , _) }
             neg             { (TokenNeg      , _) }
             and             { (TokenAnd      , _) }
             or              { (TokenOr       , _) }
             dimp            { (TokenDimp     , _) }
             imp             { (TokenImp      , _) }
             box             { (TokenBox      , _) }
             univ            { (TokenA        , _) }
             dia             { (TokenDia      , _) }
             exist           { (TokenE        , _) }
             '('             { (TokenOB       , _) }
             ')'             { (TokenCB       , _) }
             rulesep         { (TokenRuleSep  , _) }
             ';'             { (TokenSC       , _) }
             facts           { (TokenFacts    , _) }
             defaults        { (TokenDefaults , _) }
             fof             { (TokenFof      , _) }
             '.'             { (TokenDot      , _) }
             ','             { (TokenComma    , _) }
             axiom           { (TokenAxiom    , _) }
             conjecture      { (TokenConjecture, _) }
             consequence     { (TokenConsequence , _) }

%right imp
%right dimp
%left or
%left and
%left box dia univ exist neg
%right at

%%

Input :: { (Language,[Form],[Rule'],[Form]) }
Input :
  Language facts Formulas defaults Rules consequence Formulas  { ($1,$3,$5,$7) }
| Language defaults Rules consequence Formulas  { ($1,[],$3,$5) }
| Language facts Formulas consequence Formulas  { ($1,$3,[],$5) }
| Language consequence Formulas  { ($1,[],[],$3) }
| TPTP { (Intuitionistic, fst $1, [], snd $1) }

TPTP :: {[Form], [Form] }
TPTP :
   Axioms Conjecture    { ($1, [$2]) }
 | Conjecture           { ([], [$1]) }

Axioms :: { [Form] }
Axioms :
   Axiom           { [$1]}
 | Axioms Axiom    { $2:$1 }

Axiom :: { Form }
Axiom :
  fof axiom ',' Formula ')' '.' { $4 }

Conjecture :: { Form }
Conjecture :
  fof conjecture ',' Formula ')' '.' { $4 }

Language :: { Language }
Language :
  intuitionistic { Intuitionistic }
| classical      { Classical }

Rules :: { [Rule'] }
Rules :
  Rule                          { [$1] }
| Rule ';'                      { [$1] }
| Rule ';' Rules                { $1:$3 }

Rule :: { Rule' }
Rules :
  Formula rulesep Formula       { Rule' $1 $3}

Formulas :: { [Form] }
Formulas :
  Formula                          { [$1] }
| Formula ';'                      { [$1] }
| Formula ';' Formulas             { $1:$3 }


Formula :: { Form }
Formula :
  true                             { Lit' Taut    }
| false                            { Neg' (Lit' Taut) }
| nom                              { Lit' $1  }
| prop                             { Lit' $1  }
| dia    Formula                   { Dia' $2      }
| exist  Formula                   { E' $2        }
| box    Formula                   { Box' $2    }
| univ   Formula                   { A' $2       }
| Formula dimp Formula             { Dimp' $1 $3 }
| Formula imp Formula              { Imp'  $1 $3  }
| neg Formula                      { Neg' $2      }
| Formula and Formula              { Con' $1 $3   }
| Formula or Formula               { Dis' $1 $3   }
| nom at Formula                   { At' (unnom $1) $3    }
| at2 nom Formula %prec at         { At' (unnom $2) $3    }
| down '(' nom Formula ')'         { Down' (unnom $3) $4  }
| '(' Formula ')'                  { $2           }

{
happyError :: [(Token, FilePos)] -> a
happyError ((_, fp):_) = error ("Parse error near line " ++
                                   (show $ line fp) ++
                                   ", col. " ++
                                   (show $ col fp))
}
