module Main where

import Prelude
  ( ($), (.)
  , Either(..)
  , Int, (>)
  , String, (++), concat, unlines
  , Show, show
  , IO, (>>), (>>=), mapM_, putStrLn
  , FilePath
  , getContents, readFile
  , Eq
  , Read
  )

import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad      ( when )

import AbsCola   
import LexCola   ( Token, mkPosToken )
import ParCola   ( pContract, myLexer )
import PrintCola ( Print, printTree )
import SkelCola  ()

-- Define a custom data type to represent FOL formulas
data Term 
    = Var String
    | Fun String [Term]
  deriving (Eq, Show, Read)

data FOLFormula 
    = Pred String [Term]
    | Equal Term Term
    | Not FOLFormula
    | And FOLFormula FOLFormula
    | Or FOLFormula FOLFormula
    | Implies FOLFormula FOLFormula
    | Exists String FOLFormula
    | ForAll String FOLFormula
  deriving (Eq, Show, Read)

-- Function to convert the contract to FOL formula
contractToFOL :: Contract -> FOLFormula
contractToFOL (ConEmpty) = Pred "empty" [Var "empty"]
contractToFOL (ConComp component) = componentToFOL component
contractToFOL (ConAnd component contract) = And (componentToFOL component) (contractToFOL contract)

componentToFOL :: Component -> FOLFormula
componentToFOL (ComDef definition) = definitionToFOL definition
componentToFOL (ComConDef conditionalDefinition) = conditionalDefinitionToFOL conditionalDefinition
componentToFOL (ComState statement) = statementToFOL statement
componentToFOL (ComConState conditionalStatement) = conditionalStatementToFOL consitionalStatement

definitionToFOL :: Definition -> FOLFormula
definitionToFOL (DefSim simpleDefinition) = simpleDefinitionToFOL simpleDefinition
definitionToFOL (DefAnd simpleDefinition definition) = And (simpleDefinitionToFOL simpleDefinition) (definitionToFOL definition)

conditionalDefinitionToFOL :: ConditionalDefinition -> FOLFormula
conditionalDefinitionToFOL (ConDefIf definition condition) = Implies (conditiontoFOL condition) (definitionToFOL definition)
conditionalDefinitionToFOL (ConDefIfThen condition definition) = Implies (conditiontoFOL condition) (definitionToFOL definition)

statementToFOL :: Statement -> FOLFormula
statementToFOL (StateSim simpleStatement) = simpleStatementToFOL simpleStatement
statementToFOL (StateOr simpleStatement statement) = Or (simpleStatementToFOL simpleStatement) (statementToFOL statement)
statementToFOL (StateAnd simpleStatement statement) = And (simpleStatementToFOL simpleStatement) (statementToFOL statement)

conditionalStatementToFOL :: ConditionalStatement -> FOLFormula
conditionalStatementToFOL (ConStateIf statement condition) = Implies (conditionToFOL condition) (statementToFOL statement)
conditionalStatementToFOl (ConStateIfThen condition statement) = Implies (conditionToFOL condition) (statementToFOL statement)

simpleDefinitionToFOL :: SimpleDefinition -> FOLFormula
simpleDefinitionToFOL (SimDefIs id subject1 subject2) =
    case (subject1, subject2) of
        (SubQuoted str1, SubQuoted str2) -> Equal (Var str1) (Var str2)
        (SubUnQuoted ident1, SubQuoted str2) -> Equal (Var (getIdentString ident1)) (Var str2)
        (SubQuoted str1, SubUnQuoted ident2) -> Equal (Var str1) (Var (getIdentString ident2))
        (SubUnQuoted ident1, SubUnQuoted ident2) -> Equal (Var (getIdentString ident1)) (Var (getIdentString ident2))
simpleDefinitionToFOL (SimDefEq id subject numericalExpression) =
    let term1 = subjectToTerm subject
        term2 = numericalExpressionToTerm numericalExpression
    in Equal term1 term2

getIdentString :: Ident -> String
getIdentString (Ident str) = str
        
subjectToTerm :: Subject -> Term
subjectToTerm (SubQuoted str) = Var str
subjectToTerm (SubUnQuoted ident) = Var (getIdentString ident)

subjectToString :: Subject -> String
subjectToString (SubQuoted str) = str
subjectToString (SubUnQuoted ident) = getIdentString ident

numericalExpressionToTerm :: NumericalExpression -> Term
numericalExpressionToTerm (NumExpNum (NumInt n)) = Var (show n)  -- Assuming you want to represent numbers as variables
numericalExpressionToTerm (NumExpObj numObj) = numericalObjectToTerm numObj
numericalExpressionToTerm (NumExpOp expr1 operator expr2) =
    let term1 = numericalExpressionToTerm expr1
        term2 = numericalExpressionToTerm expr2
        operatorStr = case operator of
            OpPlus -> "+"
            OpMin -> "-"
            OpMult -> "*"
            OpDiv -> "/"
    in Fun operatorStr [term1, term2]

numericalObjectToTerm :: NumericalObject -> Term
numericalObjectToTerm (NumPound _ (NumInt n)) = Var ("£" ++ show n)
numericalObjectToTerm (NumDol _ (NumInt n)) = Var ("$" ++ show n)
numericalObjectToTerm (NumEur _ (NumInt n)) = Var ("€" ++ show n)
numericalObjectToTerm (NumAmount subject) = subjectToTerm subject

conditionToFOL :: Condition -> FOLFormula
conditionToFOL (CondiSim simpleCondition) = simpleConditionToFOL simpleConditon
conditionToFOL (CondiOr simpleCondition condition) = Or (simpleConditionToFOL simpleCondition) (conditionToFOL condition)
conditionToFOL (CondiAnd simpleCondition condition) = And (simpleConditionToFOL simpleCondition) (conditionToFOL condition)

receiverToTerm :: Receiver -> Term
receiverToTerm (Rec subject) = subjectToterm subject

numToString :: Num -> String
numToString (NumInt num) = show num

monthToString :: Month -> String
monthToString MJan = "January"
monthToString MFeb = "February"
monthToString MMar = "March"
monthToString MApr = "April"
monthToString MMay = "May"
monthToString MJun = "June"
monthToString MJul = "July"
monthToString MAug = "August"
monthToString MSep = "September"
monthToString MOct = "October"
monthToString MNov = "November"
monthToString MDec = "December"

tempQuanToString :: TemporalQuantifier -> String
tempQuanToString TempWithin = "Within"
tempQuanToString TempAfter = "After"
tempQuanToString TempBefore = "Before"
tempQuanToString TempIn = "In"

dateToTerm :: Date -> Term
dateToTerm (DateSpe day month year) = Var ((numToString day) ++ (monthToString month) ++ (numToString year))
dateToTerm (DateAny) = Var "AnyDate"
dateToTerm (DateA) = Var "ADate"
dateToTerm (DateThe)= Var "TheDate"
dateToTerm (DateMonRange temporalQuantifier month) = Fun (tempQuanToString temporalQuantifier) []
dateToTerm (DateYearRange temporalQuantifier year) = 
dateToTerm (DateRange temporalRange) =

simpleStatementToFOL :: SimpleStatement -> FOLFormula
simpleStatementToFOL (SimStateOne _ Holds subject ModalPermi verb object receiver date) =
    Exists (subjectToString subject) $ Predicate (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleStatementToFOL (SimStateOne _ Holds subject ModalForbi verb object receiver date) =
    Not $ Exists "subject" $ Predicate (getVerbString verb) [Var "subject", getTerm receiver, getTerm date]

simpleStatementToFOL (SimStateOne _ Holds subject ModalObli verb object receiver date) =
    ForAll "subject" $ Predicate (getVerbString verb) [Var "subject", getTerm receiver, getTerm date]

simpleConditionToFOL :: SimpleCondition -> FOLFormula