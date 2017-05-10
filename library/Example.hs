{-# LANGUAGE OverloadedStrings #-}
-- | An example module.
module Example (main) where

import           Text.PrettyPrint

backed :: Doc -> Doc
backed d = "\\" <> d

unary :: Doc -> Doc -> Doc
unary c arg = backed (c <> braces arg)

newcommand1 :: Doc -> Doc -> Doc
newcommand1 name expansion =
  backed "newcommand" <> braces (backed name) <> braces expansion

scriptMacro :: Doc -> Doc -> Doc -> Doc -> Doc
scriptMacro script pre sym letter = newcommand1 (pre <> sym) (unary script letter)

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

fraktur :: Doc
fraktur = joinSets frakturUpper frakturLower
  where
    frakturUpper = upperSet ["i"] "mathfrak" "f"
    frakturLower = lowerSet' "mathfrak" "f"

mathscr :: Doc
mathscr = joinSets mathscrUpper mathscrLower
  where
    mathscrUpper = upperSet' "mathscr" "k"
    mathscrLower = lowerSet' "mathscr" "k"

blackboardBold :: Doc
blackboardBold = bbUpper
  where
    bbUpper = upperSet ["f"] "mathbb" "b"

sansSerif :: Doc
sansSerif = joinSets sfUpper sfLower
  where
    sfUpper = upperSet ["m", "q"] "sf" "r"
    sfLower = lowerSet' "sf" "r"

mathcal :: Doc
mathcal = joinSets mathcalUpper mathcalLower
  where
    mathcalUpper = upperSet ["t"] "mathcal" "h"
    mathcalLower = lowerSet' "mathcal" "h"

-- | An example function.
main :: IO ()
main =
  print $ vcat $ punctuate "\n" [fraktur, mathscr, blackboardBold, sansSerif, mathcal]
