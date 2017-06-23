{-# LANGUAGE OverloadedStrings #-}
-- | An example module.
module Example (main) where

import           Data.Monoid                 ((<>))
import           Text.PrettyPrint            hiding ((<>))
import Data.Time

import           Control.Monad.Identity
import           Control.Monad.Writer.Strict

backed :: Doc -> Doc
backed d = "\\" <> d

unary :: Doc -> Doc -> Doc
unary c arg = backed (c <> braces arg)

newcommand0 :: Doc -> Doc -> Doc
newcommand0 name expansion =
  backed "newcommand" <> braces (backed name) <> braces expansion

newcommand1 :: Doc -> (Doc -> Doc) -> Doc
newcommand1 name expansion =
  backed "newcommand" <> braces (backed name) <> brackets "1" <> braces (expansion "#1")

declareMathOperator :: Doc -> Doc -> Doc
declareMathOperator name expansion =
  backed "DeclareMathOperator" <> braces (backed name) <> braces expansion

scriptMacro :: Doc -> Doc -> Doc -> Doc -> Doc
scriptMacro script pre sym letter = newcommand0 (pre <> sym) (unary script letter)

scriptMacros
  :: [(Doc, Doc)] -- ^ The symbols to process, with intended output
  -> (Doc -> Bool) -- ^ Exception?
  -> (Doc -> Doc -> Doc) -- ^ The rule to be used for non-exceptions
  -> (Doc -> Doc -> Doc) -- ^ The rule to be used for exceptions
  -> [Doc]
scriptMacros syms clashes mk mkExp = fmap expand syms
  where
    expand (cmd, expansion) =
      if clashes cmd
        then mkExp cmd expansion
        else mk cmd expansion

uppers :: [Doc]
uppers = map char ['A' .. 'Z']

lowers :: [Doc]
lowers = map char ['a' .. 'z']

uppersLowers :: [(Doc, Doc)]
uppersLowers = zip uppers lowers

lowersUppers :: [(Doc, Doc)]
lowersUppers = zip lowers uppers

scriptMacros'
  :: [(Doc, Doc)] -- ^ The symbols to process, with intended output
  -> (Doc -> Doc -> Doc) -- ^ The rule to be used for non-exceptions
  -> [Doc]
scriptMacros' syms mk = fmap (uncurry mk) syms

symbolSet
  :: [(Doc, Doc)]
  -> [Doc]
  -> Doc
  -> Doc
  -> Doc
symbolSet set clashes scriptName scriptIdent =
  vcat $ scriptMacros set (`elem` clashes) usualRule clashRule
  where
    usualRule = scriptMacro scriptName scriptIdent
    clashRule s = usualRule (s <> s)

symbolSet'
  :: [(Doc, Doc)]
  -> Doc
  -> Doc
  -> Doc
symbolSet' set scriptName scriptIdent = vcat $ scriptMacros' set usualRule
  where
    usualRule = scriptMacro scriptName scriptIdent

data Case = Upper | Lower

annotate Upper name = ("%%%" <+> "uppercase for" <+> name $$ "" $$)
annotate Lower name = ("%%%" <+> "lowercase for" <+> name $$ "" $$)

upperSet :: [Doc] -> Doc -> Doc -> Doc
upperSet exceptions name ident =
  annotate Upper name $ symbolSet lowersUppers exceptions name ident

upperSet' :: Doc -> Doc -> Doc
upperSet' name ident = annotate Upper name $ symbolSet' lowersUppers name ident

lowerSet' :: Doc -> Doc -> Doc
lowerSet' name ident = annotate Lower name $ symbolSet' uppersLowers name ident

joinSets :: Doc -> Doc -> Doc
joinSets a b = a $$ "" $$ b

----------------------------------------------------------
-- Actual symbol sets
----------------------------------------------------------

mkSetGen fullname tag l u = joinSets (u fullname tag) (l fullname tag)
mkSet fullname tag exc = mkSetGen fullname tag lowerSet' (upperSet exc)
mkSet' fullname tag  = mkSetGen fullname tag lowerSet' upperSet'

singles =
  [ frakturSet
  , mathscrSet
  , blackboardBoldSet
  , boldSet
  , sansSerifSet
  , mathcalSet
  ]
  where
    sansSerifSet      = mkSet  "mathsf"   "r" ["m", "q"]
    frakturSet        = mkSet  "mathfrak" "f" ["i"]
    mathcalSet        = mkSet  "mathcal"  "h" ["t"]
    mathscrSet        = mkSet' "mathscr"  "k"

    blackboardBoldSet = upperSet ["f"] "mathbb" "B"
    boldSet           = upperSet ["f"] "mathbf" "b"


mathrmify :: Doc -> Doc
mathrmify = unary "mathrm"

hat :: Doc -> Doc
hat = unary "hat"

gamma' :: Doc
gamma' = backed "Gamma"

gamma :: Doc
gamma = backed "gamma"

caret :: Doc
caret = "^"

data Newcmd0 = Newcmd0 Doc Doc Doc

subscript :: Doc -> Doc -> Doc
subscript l r = braces l <> "_" <> braces r

mathcal :: Doc -> Doc
mathcal = unary "mathcal"

bold :: Doc -> Doc
bold = unary "mathbf"

sans :: Doc -> Doc
sans = unary "mathsf"

roman :: Doc -> Doc
roman = unary "mathrm"

(.-) :: Doc -> Doc -> Doc
(.-) = subscript

($=) :: Doc -> Doc -> (Doc, Doc)
($=) = (,)

infixr 4 $=

(&=) :: Doc -> (Doc, Doc) -> Writer [Doc] ()
(&=) comment (name, expansion) =
  tell ["%%%" <+> comment $$ newcommand0 name expansion $$ ""]

infixr 3 &=

(!=) :: Doc -> (Doc, Doc) -> Writer [Doc] ()
(!=) comment (name, expansion) =
  tell ["%%%" <+> comment $$ declareMathOperator name expansion $$ ""]

infixr 3 !=

withHeader :: Doc -> Writer [Doc] a -> Doc
withHeader title pairs =
  vcat $
  "%%%" : "%%%" <+> title : "%%%" : "" : (runIdentity . execWriterT $ pairs)

algebraicNumberTheory :: Doc
algebraicNumberTheory =
  withHeader "Algebraic number theory" $ do
    "Absolute Galois group of Q"           &= "agqab"
      $= hcat [gamma', caret, mathrmify "ab"]
    "Class group of a number field"        &= "clk"
      $= unary "Cl" "K"
    "Ring of p-integers in a number field" &= "op"
      $= mathcal "O" .- backed "fP"
    "Profinite completion of Z"            &= "zhat"
      $= hat (bold "Z")

comment :: Doc -> Writer [Doc] ()
comment c = tell ["%%%" <+> c $$ ""]

general :: Doc
general =
  withHeader "General commands" $ do
    comment "Categories"

    "rings"             &&= "ring" $$= "Ring"
    "groups"            &&= "grp"  $$= "Grp"
    "abelian groups"    &&= "ab"   $$= "Ab"
    "sets"              &&= "set"  $$= "Set"
    "simplicial sets"   &&= "sset" $$= "sSet"
    "cosimplicial sets" &&= "cset" $$= "cSet"

    comment "Algebraic operations"

    "Cokernel"      != "coker" $= "coker"
    "Kernel"        != "kker"  $= "ker"
    "Automorphisms" != "aut"   $= "Aut"
    "Endomorphisms" != "eend"  $= "End"
    "Spectrum of a ring" != "spec"  $= "Spec"

  where
    infixr 3 &&=
    (&&=) c x = "The category of" <+> c &= x

    infixr 3 $$=
    ($$=) x y = x  $= sans y


topics :: [Doc]
topics = [general, algebraicNumberTheory]

defs :: Doc
defs =
  vcat $
  punctuate
    "\n" $ concat [singles, topics]

-- | An example function.
main :: IO ()
main = do
  let
    path = "/home/mrkgnao/code/tex/dfat/generated-preamble.tex"
    write = writeFile path
    append = appendFile path

  getCurrentTime >>= (write . (\t -> "%%% Generated at " <> t <> "\n\n") . show)
  append (render defs)
