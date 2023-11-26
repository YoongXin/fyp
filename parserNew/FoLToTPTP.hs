module FOLToTPTP where

import AstToFOL
import Data.List 

data TPTPFormula
    = TPTPForAll [TPTPTerm] TPTPFormula
    | TPTPExists [TPTPTerm] TPTPFormula
    | TPTPImplication TPTPFormula TPTPFormula
    | TPTPConjunction TPTPFormula TPTPFormula
    | TPTPDisjunction TPTPFormula TPTPFormula
    | TPTPNot TPTPFormula 
    | TPTPPredicate String [TPTPTerm]
    | TPTPEqual TPTPTerm TPTPTerm
  deriving (Eq, Read)

data TPTPTerm 
    = Variable String
    | Function String [TPTPTerm]
  deriving (Eq, Read)

folToTPTP :: FOLFormula -> TPTPFormula
folToTPTP (Pred name terms) = TPTPPredicate name (map termToTptp terms)
folToTPTP (Equal term1 term2) = TPTPEqual (termToTptp term1) (termToTptp term2)
folToTPTP (Not formula) = TPTPNot (folToTPTP formula)
folToTPTP (And formula1 formula2) = TPTPConjunction (folToTPTP formula1) (folToTPTP formula2)
folToTPTP (Or formula1 formula2) = TPTPDisjunction (folToTPTP formula1) (folToTPTP formula2)
folToTPTP (Implies formula1 formula2) = TPTPImplication (folToTPTP formula1) (folToTPTP formula2)
folToTPTP (ForAll vars formula) = TPTPForAll (map termToTptp vars) (folToTPTP formula)
folToTPTP (Exists vars formula) = TPTPExists (map termToTptp vars) (folToTPTP formula)
folToTPTP (Brackets formula) = folToTPTP formula

arguments :: [TPTPTerm] -> String
arguments [] = ""
arguments ts = "(" ++ intercalate "," (map show ts) ++ ")"

termToTptp :: Term -> TPTPTerm
termToTptp (Var str) = Variable str
termToTptp (Fun str [term]) = Function str [termToTptp term]

instance Show TPTPFormula where
    show rep = case rep of
        TPTPForAll terms formula -> "! [" ++ arguments terms ++ "] : (" ++ show formula ++ ")"
        TPTPExists terms formula -> "? [" ++ arguments terms ++ "] : (" ++ show formula ++ ")"
        TPTPImplication formula1 formula2 -> "(" ++ show formula1 ++ ") => (" ++ show formula2 ++ ")"
        TPTPConjunction formula1 formula2 -> "(" ++ show formula1 ++ ") & (" ++ show formula2 ++ ")"
        TPTPDisjunction formula1 formula2 -> "(" ++ show formula1 ++ ") | (" ++ show formula2 ++ ")"
        TPTPNot formula -> "~ (" ++ show formula ++ ")"
        TPTPPredicate pred terms -> pred ++ arguments terms
        TPTPEqual term1 term2 -> show term1 ++ " = " ++ show term2

instance Show TPTPTerm where
    show (Function f ts) = f ++ arguments ts
    show (Variable s)    = s


