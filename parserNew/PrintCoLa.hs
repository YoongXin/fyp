-- File generated by the BNF Converter (bnfc 2.9.5).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for PrintCoLa.

module PrintCoLa where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified AbsCoLa

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t, null spc, null rest) of
      (True , _   , True ) -> []             -- remove trailing space
      (False, _   , True ) -> t              -- remove trailing space
      (False, True, False) -> t ++ ' ' : s   -- add space if none
      _                    -> t ++ s
    where
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print AbsCoLa.Ident where
  prt _ (AbsCoLa.Ident i) = doc $ showString i
instance Print AbsCoLa.Contract where
  prt i = \case
    AbsCoLa.ConEmpty -> prPrec i 0 (concatD [])
    AbsCoLa.ConComp component -> prPrec i 0 (concatD [prt 0 component])
    AbsCoLa.ConAnd component contract -> prPrec i 0 (concatD [prt 0 component, doc (showString "C-AND"), prt 0 contract])

instance Print AbsCoLa.Component where
  prt i = \case
    AbsCoLa.ComDef definition -> prPrec i 0 (concatD [prt 0 definition])
    AbsCoLa.ComConDef conditionaldefinition -> prPrec i 0 (concatD [prt 0 conditionaldefinition])
    AbsCoLa.ComState statement -> prPrec i 0 (concatD [prt 0 statement])
    AbsCoLa.ComConState conditionalstatement -> prPrec i 0 (concatD [prt 0 conditionalstatement])

instance Print AbsCoLa.Definition where
  prt i = \case
    AbsCoLa.DefSim simpledefinition -> prPrec i 0 (concatD [prt 0 simpledefinition])
    AbsCoLa.DefAnd simpledefinition definition -> prPrec i 0 (concatD [prt 0 simpledefinition, doc (showString "AND"), prt 0 definition])

instance Print AbsCoLa.SimpleDefinition where
  prt i = \case
    AbsCoLa.SimDefIs id_ subject1 subject2 -> prPrec i 0 (concatD [prt 0 id_, prt 0 subject1, doc (showString "IS"), prt 0 subject2])
    AbsCoLa.SimDefEq id_ subject numericalexpression -> prPrec i 0 (concatD [prt 0 id_, prt 0 subject, doc (showString "EQUALS"), prt 0 numericalexpression])

instance Print AbsCoLa.NumericalExpression where
  prt i = \case
    AbsCoLa.NumExpNum num -> prPrec i 0 (concatD [prt 0 num])
    AbsCoLa.NumExpObj numericalobject -> prPrec i 0 (concatD [prt 0 numericalobject])
    AbsCoLa.NumExpOp numericalexpression1 operator numericalexpression2 -> prPrec i 0 (concatD [prt 0 numericalexpression1, prt 0 operator, prt 0 numericalexpression2])

instance Print AbsCoLa.Operator where
  prt i = \case
    AbsCoLa.OpPlus -> prPrec i 0 (concatD [doc (showString "PLUS")])
    AbsCoLa.OpMin -> prPrec i 0 (concatD [doc (showString "MINUS")])
    AbsCoLa.OpMult -> prPrec i 0 (concatD [doc (showString "TIMES")])
    AbsCoLa.OpDiv -> prPrec i 0 (concatD [doc (showString "DIVIDE")])

instance Print AbsCoLa.ConditionalDefinition where
  prt i = \case
    AbsCoLa.ConDefIf definition condition -> prPrec i 0 (concatD [prt 0 definition, doc (showString "IF"), prt 0 condition])
    AbsCoLa.ConDefIfThen condition definition -> prPrec i 0 (concatD [doc (showString "IF"), prt 0 condition, doc (showString "THEN"), prt 0 definition])

instance Print AbsCoLa.Statement where
  prt i = \case
    AbsCoLa.StateSim simplestatement -> prPrec i 0 (concatD [prt 0 simplestatement])
    AbsCoLa.StateOr simplestatement statement -> prPrec i 0 (concatD [prt 0 simplestatement, doc (showString "OR"), prt 0 statement])
    AbsCoLa.StateAnd simplestatement statement -> prPrec i 0 (concatD [prt 0 simplestatement, doc (showString "AND"), prt 0 statement])

instance Print AbsCoLa.ConditionalStatement where
  prt i = \case
    AbsCoLa.ConStateIf statement condition -> prPrec i 0 (concatD [prt 0 statement, doc (showString "IF"), prt 0 condition])
    AbsCoLa.ConStateIfThen condition statement -> prPrec i 0 (concatD [doc (showString "IF"), prt 0 condition, doc (showString "THEN"), prt 0 statement])

instance Print AbsCoLa.SimpleStatement where
  prt i = \case
    AbsCoLa.SimStateOne id_ holds subject modalverb verb object receiver date -> prPrec i 0 (concatD [prt 0 id_, prt 0 holds, prt 0 subject, prt 0 modalverb, prt 0 verb, prt 0 object, prt 0 receiver, prt 0 date])
    AbsCoLa.SimStateTwo id_ holds subject date modalverb verb object receiver -> prPrec i 0 (concatD [prt 0 id_, prt 0 holds, prt 0 subject, prt 0 date, prt 0 modalverb, prt 0 verb, prt 0 object, prt 0 receiver])
    AbsCoLa.SimStateThree id_ holds date subject modalverb verb object receiver -> prPrec i 0 (concatD [prt 0 id_, prt 0 holds, prt 0 date, prt 0 subject, prt 0 modalverb, prt 0 verb, prt 0 object, prt 0 receiver])
    AbsCoLa.SimStateOneNH id_ subject modalverb verb object receiver date -> prPrec i 0 (concatD [prt 0 id_, prt 0 subject, prt 0 modalverb, prt 0 verb, prt 0 object, prt 0 receiver, prt 0 date])
    AbsCoLa.SimStateTwoNH id_ subject date modalverb verb object receiver -> prPrec i 0 (concatD [prt 0 id_, prt 0 subject, prt 0 date, prt 0 modalverb, prt 0 verb, prt 0 object, prt 0 receiver])
    AbsCoLa.SimStateThreeNH id_ date subject modalverb verb object receiver -> prPrec i 0 (concatD [prt 0 id_, prt 0 date, prt 0 subject, prt 0 modalverb, prt 0 verb, prt 0 object, prt 0 receiver])

instance Print AbsCoLa.Condition where
  prt i = \case
    AbsCoLa.CondiSim simplecondition -> prPrec i 0 (concatD [prt 0 simplecondition])
    AbsCoLa.CondiOr simplecondition condition -> prPrec i 0 (concatD [prt 0 simplecondition, doc (showString "OR"), prt 0 condition])
    AbsCoLa.CondiAnd simplecondition condition -> prPrec i 0 (concatD [prt 0 simplecondition, doc (showString "AND"), prt 0 condition])

instance Print AbsCoLa.SimpleCondition where
  prt i = \case
    AbsCoLa.SimConOne id_ holds subject verbstatus object receiver date -> prPrec i 0 (concatD [prt 0 id_, prt 0 holds, prt 0 subject, prt 0 verbstatus, prt 0 object, prt 0 receiver, prt 0 date])
    AbsCoLa.SimConTwo id_ holds subject date verbstatus object receiver -> prPrec i 0 (concatD [prt 0 id_, prt 0 holds, prt 0 subject, prt 0 date, prt 0 verbstatus, prt 0 object, prt 0 receiver])
    AbsCoLa.SimConThree id_ holds date subject verbstatus object receiver -> prPrec i 0 (concatD [prt 0 id_, prt 0 holds, prt 0 date, prt 0 subject, prt 0 verbstatus, prt 0 object, prt 0 receiver])
    AbsCoLa.SimConFour id_ holds subject modalverb verb object receiver date -> prPrec i 0 (concatD [prt 0 id_, prt 0 holds, prt 0 subject, prt 0 modalverb, prt 0 verb, prt 0 object, prt 0 receiver, prt 0 date])
    AbsCoLa.SimConFive id_ holds booleanexpression -> prPrec i 0 (concatD [prt 0 id_, prt 0 holds, prt 0 booleanexpression])
    AbsCoLa.SimConOneNH id_ subject verbstatus object receiver date -> prPrec i 0 (concatD [prt 0 id_, prt 0 subject, prt 0 verbstatus, prt 0 object, prt 0 receiver, prt 0 date])
    AbsCoLa.SimConTwoNH id_ subject date verbstatus object receiver -> prPrec i 0 (concatD [prt 0 id_, prt 0 subject, prt 0 date, prt 0 verbstatus, prt 0 object, prt 0 receiver])
    AbsCoLa.SimConThreeNH id_ date subject verbstatus object receiver -> prPrec i 0 (concatD [prt 0 id_, prt 0 date, prt 0 subject, prt 0 verbstatus, prt 0 object, prt 0 receiver])
    AbsCoLa.SimConFourNH id_ subject modalverb verb object receiver date -> prPrec i 0 (concatD [prt 0 id_, prt 0 subject, prt 0 modalverb, prt 0 verb, prt 0 object, prt 0 receiver, prt 0 date])
    AbsCoLa.SimConFiveNH id_ booleanexpression -> prPrec i 0 (concatD [prt 0 id_, prt 0 booleanexpression])

instance Print AbsCoLa.BooleanExpression where
  prt i = \case
    AbsCoLa.BoolEx subject1 verbstatus comparison subject2 -> prPrec i 0 (concatD [prt 0 subject1, prt 0 verbstatus, prt 0 comparison, prt 0 subject2])

instance Print AbsCoLa.ID where
  prt i = \case
    AbsCoLa.IDSim num -> prPrec i 0 (concatD [doc (showString "["), prt 0 num, doc (showString "]")])
    AbsCoLa.IDRep num1 num2 -> prPrec i 0 (concatD [doc (showString "["), prt 0 num1, doc (showString "("), prt 0 num2, doc (showString ")"), doc (showString "]")])

instance Print AbsCoLa.Holds where
  prt i = \case
    AbsCoLa.HoldYes -> prPrec i 0 (concatD [doc (showString "it"), doc (showString "is"), doc (showString "the"), doc (showString "case"), doc (showString "that")])
    AbsCoLa.HoldNo -> prPrec i 0 (concatD [doc (showString "it"), doc (showString "is"), doc (showString "not"), doc (showString "the"), doc (showString "case"), doc (showString "that")])

instance Print AbsCoLa.Subject where
  prt i = \case
    AbsCoLa.SubQuoted str -> prPrec i 0 (concatD [printString str])
    AbsCoLa.SubUnQuoted id_ -> prPrec i 0 (concatD [prt 0 id_])

instance Print AbsCoLa.Verb where
  prt i = \case
    AbsCoLa.VDel -> prPrec i 0 (concatD [doc (showString "deliver")])
    AbsCoLa.VPay -> prPrec i 0 (concatD [doc (showString "pay")])
    AbsCoLa.VCharge -> prPrec i 0 (concatD [doc (showString "charge")])
    AbsCoLa.VRefund -> prPrec i 0 (concatD [doc (showString "refund")])

instance Print AbsCoLa.VerbStatus where
  prt i = \case
    AbsCoLa.VSDel -> prPrec i 0 (concatD [doc (showString "delivered")])
    AbsCoLa.VSPay -> prPrec i 0 (concatD [doc (showString "paid")])
    AbsCoLa.VSCharge -> prPrec i 0 (concatD [doc (showString "charged")])
    AbsCoLa.VSRefund -> prPrec i 0 (concatD [doc (showString "refunded")])

instance Print AbsCoLa.Comparison where
  prt i = \case
    AbsCoLa.CompareLess -> prPrec i 0 (concatD [doc (showString "less"), doc (showString "than")])
    AbsCoLa.CompareEq equal -> prPrec i 0 (concatD [prt 0 equal])
    AbsCoLa.CompareMore more -> prPrec i 0 (concatD [prt 0 more])

instance Print AbsCoLa.Equal where
  prt i = \case
    AbsCoLa.EqOne -> prPrec i 0 (concatD [doc (showString "equals")])
    AbsCoLa.EqTwo -> prPrec i 0 (concatD [doc (showString "equal"), doc (showString "to")])

instance Print AbsCoLa.More where
  prt i = \case
    AbsCoLa.MoreOne -> prPrec i 0 (concatD [doc (showString "more"), doc (showString "than")])
    AbsCoLa.MoreTwo -> prPrec i 0 (concatD [doc (showString "greater"), doc (showString "than")])

instance Print AbsCoLa.ModalVerb where
  prt i = \case
    AbsCoLa.ModalObli obligation -> prPrec i 0 (concatD [prt 0 obligation])
    AbsCoLa.ModalPermi -> prPrec i 0 (concatD [doc (showString "may")])
    AbsCoLa.ModalForbi -> prPrec i 0 (concatD [doc (showString "is"), doc (showString "forbidden"), doc (showString "to")])

instance Print AbsCoLa.Obligation where
  prt i = \case
    AbsCoLa.ObliOne -> prPrec i 0 (concatD [doc (showString "shall")])
    AbsCoLa.ObliTwo -> prPrec i 0 (concatD [doc (showString "must")])

instance Print AbsCoLa.Date where
  prt i = \case
    AbsCoLa.DateSpe num1 month num2 -> prPrec i 0 (concatD [doc (showString "on"), doc (showString "the"), prt 0 num1, prt 0 month, prt 0 num2])
    AbsCoLa.DateAny -> prPrec i 0 (concatD [doc (showString "on"), doc (showString "ANYDATE")])
    AbsCoLa.DateSome subject -> prPrec i 0 (concatD [doc (showString "on"), doc (showString "SOMEDATE"), prt 0 subject])
    AbsCoLa.DateThe subject -> prPrec i 0 (concatD [doc (showString "on"), doc (showString "THEDATE"), prt 0 subject])
    AbsCoLa.DateQuanSpecific temporalquantifier num1 month num2 -> prPrec i 0 (concatD [prt 0 temporalquantifier, prt 0 num1, prt 0 month, prt 0 num2])
    AbsCoLa.DateQuanThe temporalquantifier1 temporaloffset temporalquantifier2 subject -> prPrec i 0 (concatD [prt 0 temporalquantifier1, prt 0 temporaloffset, prt 0 temporalquantifier2, doc (showString "THEDATE"), prt 0 subject])
    AbsCoLa.DateQuanSome temporalquantifier1 temporaloffset temporalquantifier2 subject -> prPrec i 0 (concatD [prt 0 temporalquantifier1, prt 0 temporaloffset, prt 0 temporalquantifier2, doc (showString "SOMEDATE"), prt 0 subject])

instance Print AbsCoLa.TemporalQuantifier where
  prt i = \case
    AbsCoLa.TempAfter -> prPrec i 0 (concatD [doc (showString "after")])
    AbsCoLa.TempBefore -> prPrec i 0 (concatD [doc (showString "before")])

instance Print AbsCoLa.TemporalOffset where
  prt i = \case
    AbsCoLa.TempOffDay num -> prPrec i 0 (concatD [prt 0 num, doc (showString "day(s)")])
    AbsCoLa.TempOffYear num -> prPrec i 0 (concatD [prt 0 num, doc (showString "year(s)")])
    AbsCoLa.TempOffWeek num -> prPrec i 0 (concatD [prt 0 num, doc (showString "week(s)")])

instance Print AbsCoLa.Month where
  prt i = \case
    AbsCoLa.MJan -> prPrec i 0 (concatD [doc (showString "January")])
    AbsCoLa.MFeb -> prPrec i 0 (concatD [doc (showString "February")])
    AbsCoLa.MMar -> prPrec i 0 (concatD [doc (showString "March")])
    AbsCoLa.MApr -> prPrec i 0 (concatD [doc (showString "April")])
    AbsCoLa.MMay -> prPrec i 0 (concatD [doc (showString "May")])
    AbsCoLa.MJun -> prPrec i 0 (concatD [doc (showString "June")])
    AbsCoLa.MJul -> prPrec i 0 (concatD [doc (showString "July")])
    AbsCoLa.MAug -> prPrec i 0 (concatD [doc (showString "August")])
    AbsCoLa.MSep -> prPrec i 0 (concatD [doc (showString "September")])
    AbsCoLa.MOct -> prPrec i 0 (concatD [doc (showString "October")])
    AbsCoLa.MNov -> prPrec i 0 (concatD [doc (showString "November")])
    AbsCoLa.MDec -> prPrec i 0 (concatD [doc (showString "December")])

instance Print AbsCoLa.Object where
  prt i = \case
    AbsCoLa.ObjNu numericalobject -> prPrec i 0 (concatD [prt 0 numericalobject])
    AbsCoLa.ObjNonNu nonnumericalobject -> prPrec i 0 (concatD [prt 0 nonnumericalobject])

instance Print AbsCoLa.NumericalObject where
  prt i = \case
    AbsCoLa.NumPound pounds num -> prPrec i 0 (concatD [prt 0 pounds, prt 0 num])
    AbsCoLa.NumDol dollars num -> prPrec i 0 (concatD [prt 0 dollars, prt 0 num])
    AbsCoLa.NumEur euros num -> prPrec i 0 (concatD [prt 0 euros, prt 0 num])
    AbsCoLa.NumAmount subject -> prPrec i 0 (concatD [doc (showString "AMOUNT"), prt 0 subject])

instance Print AbsCoLa.Pounds where
  prt i = \case
    AbsCoLa.PoundOne -> prPrec i 0 (concatD [doc (showString "GBP")])
    AbsCoLa.PoundTwo -> prPrec i 0 (concatD [doc (showString "POUNDS")])
    AbsCoLa.PoundThree -> prPrec i 0 (concatD [doc (showString "quid")])

instance Print AbsCoLa.Dollars where
  prt i = \case
    AbsCoLa.DollarOne -> prPrec i 0 (concatD [doc (showString "USD")])
    AbsCoLa.DollarTwo -> prPrec i 0 (concatD [doc (showString "DOLLARS")])
    AbsCoLa.DollarThree -> prPrec i 0 (concatD [doc (showString "buck")])

instance Print AbsCoLa.Euros where
  prt i = \case
    AbsCoLa.EuroOne -> prPrec i 0 (concatD [doc (showString "EUR")])
    AbsCoLa.EuroTwo -> prPrec i 0 (concatD [doc (showString "EUROS")])

instance Print AbsCoLa.NonNumericalObject where
  prt i = \case
    AbsCoLa.NonNumCurr subject -> prPrec i 0 (concatD [doc (showString "SOMECURRENCY"), prt 0 subject])
    AbsCoLa.NonNumRep subject -> prPrec i 0 (concatD [doc (showString "REPORT"), prt 0 subject])
    AbsCoLa.NonNumNamed subject -> prPrec i 0 (concatD [doc (showString "NAMEDOBJECT"), prt 0 subject])
    AbsCoLa.NonNumOther subject -> prPrec i 0 (concatD [doc (showString "OTHEROBJECT"), prt 0 subject])

instance Print AbsCoLa.Num where
  prt i = \case
    AbsCoLa.NumInt n -> prPrec i 0 (concatD [prt 0 n])

instance Print AbsCoLa.Receiver where
  prt i = \case
    AbsCoLa.Rec subject -> prPrec i 0 (concatD [doc (showString "to"), prt 0 subject])
