module AstToFOL where

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
  , Integer
  , fromInteger
  )

import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad      ( when )
import Data.Time

import AbsCoLa   
import LexCoLa   ( Token, mkPosToken )
import ParCoLa   ( pContract, myLexer )
import PrintCoLa ( Print, printTree )
import SkelCoLa  ()

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
    | Exists [Term] FOLFormula
    | ForAll [Term] FOLFormula
  deriving (Eq, Show, Read)

-- Function to convert a specific date to an integer
dateToInt :: Integer -> Int -> Int -> Integer
dateToInt year month day = toModifiedJulianDay (fromGregorian year month day)

-- Function to convert the contract to FOL formula
contractToFOL :: Contract -> FOLFormula
contractToFOL (ConEmpty) = Pred "empty" [Var "empty"]
contractToFOL (ConComp component) = componentToFOL component
contractToFOL (ConAnd component contract) = And (componentToFOL component) (contractToFOL contract)

componentToFOL :: Component -> FOLFormula
componentToFOL (ComDef definition) = definitionToFOL definition
componentToFOL (ComConDef conditionalDefinition) = conditionalDefinitionToFOL conditionalDefinition
componentToFOL (ComState statement) = statementToFOL statement
componentToFOL (ComConState conditionalStatement) = conditionalStatementToFOL conditionalStatement

definitionToFOL :: Definition -> FOLFormula
definitionToFOL (DefSim simpleDefinition) = simpleDefinitionToFOL simpleDefinition
definitionToFOL (DefAnd simpleDefinition definition) = And (simpleDefinitionToFOL simpleDefinition) (definitionToFOL definition)

conditionalDefinitionToFOL :: ConditionalDefinition -> FOLFormula
conditionalDefinitionToFOL (ConDefIf definition condition) = Implies (conditionToFOL condition) (definitionToFOL definition)
conditionalDefinitionToFOL (ConDefIfThen condition definition) = Implies (conditionToFOL condition) (definitionToFOL definition)

statementToFOL :: Statement -> FOLFormula
statementToFOL (StateSim simpleStatement) = simpleStatementToFOL simpleStatement
statementToFOL (StateOr simpleStatement statement) = Or (simpleStatementToFOL simpleStatement) (statementToFOL statement)
statementToFOL (StateAnd simpleStatement statement) = And (simpleStatementToFOL simpleStatement) (statementToFOL statement)

conditionalStatementToFOL :: ConditionalStatement -> FOLFormula
conditionalStatementToFOL (ConStateIf statement condition) = Implies (conditionToFOL condition) (statementToFOL statement)
conditionalStatementToFOL (ConStateIfThen condition statement) = Implies (conditionToFOL condition) (statementToFOL statement)

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
conditionToFOL (CondiSim simpleCondition) = simpleConditionToFOL simpleCondition
conditionToFOL (CondiOr simpleCondition condition) = Or (simpleConditionToFOL simpleCondition) (conditionToFOL condition)
conditionToFOL (CondiAnd simpleCondition condition) = And (simpleConditionToFOL simpleCondition) (conditionToFOL condition)

receiverToTerm :: Receiver -> Term
receiverToTerm (Rec subject) = subjectToTerm subject

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

monthToInt :: Month -> Int
monthToInt MJan = 1
monthToInt MFeb = 1
monthToInt MMar = 3
monthToInt MApr = 4
monthToInt MMay = 5
monthToInt MJun = 6
monthToInt MJul = 7
monthToInt MAug = 8
monthToInt MSep = 9
monthToInt MOct = 10
monthToInt MNov = 11
monthToInt MDec = 12

tempQuanToString :: TemporalQuantifier -> String
tempQuanToString TempAfter = "After"
tempQuanToString TempBefore = "Before"

dateToTerm :: Date -> Term
dateToTerm (DateSpe day month year) = Var ((numToString day) ++ (monthToString month) ++ (numToString year))
dateToTerm (DateAny) = Var "AnyDate"
dateToTerm (DateSome str) = Var "SomeDate"
dateToTerm (DateThe str)= Var "TheDate"

getVerbString :: Verb -> String
getVerbString VDel = "deliver"
getVerbString VPay = "pay"
getVerbString VCharge = "charge"
getVerbString VRefund = "refund"

getVerbStatusString :: VerbStatus -> String
getVerbStatusString VSDel = "delivered"
getVerbStatusString VSPay = "payed"
getVerbStatusString VSCharge = "charged"
getVerbStatusString VSRefund = "refunded"

dateSpeToTerm :: Num -> Month -> Num -> Term
dateSpeToTerm (NumInt year) month (NumInt day) = Var $ show $ dateToInt year (monthToInt month) (fromInteger day)

simpleStatementToFOL :: SimpleStatement -> FOLFormula
simpleStatementToFOL (SimStateOne id holds subject modalVerb verb object receiver (DateSpe day month year)) =
    case (holds, modalVerb, verb) of
        (HoldYes, ModalObli _, VDel) -> yesMustDel 
        (HoldNo, ModalObli _, VDel) -> noMustDel 
        (HoldYes, ModalPermi, VDel) -> yesMayDel 
        (HoldNo, ModalPermi, VDel) -> noMayDel 
        (HoldYes, ModalForbi, VDel) -> noMayDel 
        (HoldNo, ModalForbi, VDel) -> yesMayDel 
        (HoldYes, ModalObli _, VPay) -> yesMustPay 
        (HoldNo, ModalObli _, VPay) -> noMustPay 
        (HoldYes, ModalPermi, VPay) -> yesMayPay 
        (HoldNo, ModalPermi, VPay) -> noMayPay 
        (HoldYes, ModalForbi, VPay) -> noMayPay 
        (HoldNo, ModalForbi, VPay) -> yesMayPay 
        (HoldYes, ModalObli _, VCharge) -> yesMustCharge 
        (HoldNo, ModalObli _, VCharge) -> noMustCharge 
        (HoldYes, ModalPermi, VCharge) -> yesMayCharge 
        (HoldNo, ModalPermi, VCharge) -> noMayCharge 
        (HoldYes, ModalForbi, VCharge) -> noMayCharge 
        (HoldNo, ModalForbi, VCharge) -> yesMayCharge 
        (HoldYes, ModalObli _, VRefund) -> yesMustRefund 
        (HoldNo, ModalObli _, VRefund) -> noMustRefund 
        (HoldYes, ModalPermi, VRefund) -> yesMayRefund
        (HoldNo, ModalPermi, VRefund) -> noMayRefund 
        (HoldYes, ModalForbi, VRefund) -> noMayRefund 
        (HoldNo, ModalForbi, VRefund) -> yesMayRefund 


    where
        -- Common base predicates
        basePredicates =  ForAll [Var "x", Var "y", Var "d", Var "o"] $
                And
                    (And
                        (And (Pred "Name" [Var "x", subjectToTerm subject]) (Pred "Name" [Var "y", receiverToTerm receiver]))
                        (Pred "Date" [Var "d", dateSpeToTerm year month day])
                    )
                    (objectToPredicate object)
                
        -- Predicates for different cases
        yesMustDel = And basePredicates (Pred "MustDeliver" [Var "x", Var "y", Var "o", Var "d"])
        noMustDel = Not $ And basePredicates (Pred "MustDeliver" [Var "x", Var "y", Var "o", Var "d"])
        yesMayDel = And basePredicates (Pred "MayDeliver" [Var "x", Var "y", Var "o", Var "d"])
        noMayDel = Not $ And basePredicates (Pred "MayDeliver" [Var "x", Var "y", Var "o", Var "d"])
        yesMustPay = And basePredicates (Pred "MustPay" [Var "x", Var "y", Var "o", Var "d"])
        noMustPay = Not $ And basePredicates (Pred "MustPay" [Var "x", Var "y", Var "o", Var "d"])
        yesMayPay = And basePredicates (Pred "MayPay" [Var "x", Var "y", Var "o", Var "d"])
        noMayPay = Not $ And basePredicates (Pred "MayPay" [Var "x", Var "y", Var "o", Var "d"])
        yesMustCharge = And basePredicates (Pred "MustCharge" [Var "x", Var "y", Var "o", Var "d"])
        noMustCharge = Not $ And basePredicates (Pred "MustCharge" [Var "x", Var "y", Var "o", Var "d"])
        yesMayCharge = And basePredicates (Pred "MayCharge" [Var "x", Var "y", Var "o", Var "d"])
        noMayCharge = Not $ And basePredicates (Pred "MayCharge" [Var "x", Var "y", Var "o", Var "d"])
        yesMustRefund = And basePredicates (Pred "MustRefund" [Var "x", Var "y", Var "o", Var "d"])
        noMustRefund = Not $ And basePredicates (Pred "MustRefund" [Var "x", Var "y", Var "o", Var "d"])
        yesMayRefund = And basePredicates (Pred "MayRefund" [Var "x", Var "y", Var "o", Var "d"])
        noMayRefund = Not $ And basePredicates (Pred "MayRefund" [Var "x", Var "y", Var "o", Var "d"])

        -- Convert different kinds of objects to predicates
        objectToPredicate (ObjNu (NumPound _ (NumInt num))) = Pred "ObjectPound" [Var "o", Var (show num)]
        objectToPredicate (ObjNu (NumDol _ (NumInt num))) = Pred "ObjectDollar" [Var "o", Var (show num)]
        objectToPredicate (ObjNu (NumEur _ (NumInt num))) = Pred "ObjectEuro" [Var "o", Var (show num)]
        objectToPredicate (ObjNu (NumAmount subject)) = Pred "ObjectAmount" [Var "o", subjectToTerm subject]
        objectToPredicate (ObjNonNu (NonNumCurr subject)) = Pred "ObjectSomeCurrency" [Var "o", subjectToTerm subject]
        objectToPredicate (ObjNonNu (NonNumRep subject)) = Pred "ObjectReport" [Var "o", subjectToTerm subject]
        objectToPredicate (ObjNonNu (NonNumNamed subject)) = Pred "ObjectNamedObject" [Var "o", subjectToTerm subject]
        objectToPredicate (ObjNonNu (NonNumOther subject)) = Pred "ObjectOtherObject" [Var "o", subjectToTerm subject]

simpleConditionToFOL :: SimpleCondition -> FOLFormula
simpleConditionToFOL (SimConOne id HoldYes subject VSDel object receiver (DateSpe day month year)) =
    ForAll [Var "x", Var "y", Var "d", Var "o"] $
    And
        (And
            (And
                (And (Pred "Name" [Var "x", subjectToTerm subject]) (Pred "Name" [Var "y", receiverToTerm receiver]))
                (Pred "Date" [Var "d", dateSpeToTerm year month day])
            )
            (Pred "Object" [Var "o"])
        )
        (Pred "Delivered" [Var "x", Var "y", Var "o", Var "d"])


boolExToFOL :: BooleanExpression -> FOLFormula
boolExToFOL (BoolEx subject1 VSDel CompareLess subject2) = ForAll [subjectToTerm subject1] $ Pred "DeliveredLess" [subjectToTerm subject1, subjectToTerm subject2]
