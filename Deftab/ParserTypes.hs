module Deftab.ParserTypes
   ( Form(..), Atom(..), Nom, Prop, unnom
   , Rule'(..), Language(..) ) where

import Data.Char (isDigit)
import qualified Data.ByteString.Char8 as C

data Language = Classical | Intuitionistic deriving (Eq,Show)

data Rule'= Rule' Form Form Form
  deriving (Eq,Ord,Show)

-- formula, as parsed
data Form
     = Lit'    Atom
     | Con'    Form Form
     | Dis'    Form Form
     | At'     Nom Form
     | Down'   Nom Form
     | Box'    Form
     | Dia'    Form
     | A'      Form
     | E'      Form
     | Neg'    Form
     | Imp'    Form Form
     | Dimp'   Form Form
  deriving (Eq, Ord, Show)


type Nom     = C.ByteString
type Prop    = C.ByteString
data Atom    = Taut | N Nom | P Prop deriving(Eq, Ord)

unnom :: Atom -> C.ByteString
unnom (N s) = s
unnom _ = undefined

instance Show Atom where
 show (Taut) = "T"
 show (N n)  = "N" ++ C.unpack n
 show (P p) | isDigit (C.head p) = "P" ++ C.unpack p
            | otherwise  = C.unpack p
