module FOLToTPTP where

import AbsCoLa
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

arguments' :: [TPTPTerm] -> String
arguments' [] = ""
arguments' ts = intercalate "," (map show ts) 

termToTptp :: Term -> TPTPTerm
termToTptp (Var str) = Variable str
termToTptp (Fun str terms) = Function str (map termToTptp terms)

instance Show TPTPFormula where
    show rep = case rep of
        TPTPForAll terms formula -> "! [" ++ arguments' terms ++ "] : (" ++ show formula ++ ")"
        TPTPExists terms formula -> "? [" ++ arguments' terms ++ "] : (" ++ show formula ++ ")"
        TPTPImplication formula1 formula2 -> "(" ++ show formula1 ++ ") => (" ++ show formula2 ++ ")"
        TPTPConjunction formula1 formula2 -> "(" ++ show formula1 ++ ") & (" ++ show formula2 ++ ")"
        TPTPDisjunction formula1 formula2 -> "(" ++ show formula1 ++ ") | (" ++ show formula2 ++ ")"
        TPTPNot formula -> "~ (" ++ show formula ++ ")"
        TPTPPredicate pred terms -> pred ++ arguments terms
        TPTPEqual term1 term2 -> show term1 ++ " = " ++ show term2

instance Show TPTPTerm where
    show (Function f ts) = f ++ arguments ts
    show (Variable s)    = removeSpaces s

removeSpaces :: String -> String
removeSpaces = filter (/= ' ')

folToTPTPString :: String -> FOLFormula -> String
folToTPTPString name formula =
  "fof(" ++ name ++ ", axiom, (" ++ show (folToTPTP formula) ++ "))."
    
runTptpConversionContract :: Contract -> String
runTptpConversionContract contract = folToTPTPString "contract" (runFOLConversion contract)

runTptpConversionPerformance :: Contract -> String
runTptpConversionPerformance performance = folToTPTPString "performance" (runFOLConversion performance)



--build axioms for must may, before after
-- add to dictionary for nh statements
-- change simplestatement verb status to check dictionary
-- might need to pass in new parameter

--ask user to input performance of contract
--put output into one file and run vampire
--check vampire to see how to output proof can be clearer


