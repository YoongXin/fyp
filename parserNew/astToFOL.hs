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
    createFormulaSimpleStatement holds modalVerb verb subject object receiver day month year
simpleStatementToFOL (SimStateTwo id holds subject (DateSpe day month year) modalVerb verb object receiver) =
    createFormulaSimpleStatement holds modalVerb verb subject object receiver day month year
simpleStatementToFOL (SimStateThree id holds (DateSpe day month year) subject modalVerb verb object receiver) =
    createFormulaSimpleStatement holds modalVerb verb subject object receiver day month year
simpleStatementToFOL (SimStateOneNH id subject modalVerb verb object receiver (DateSpe day month year)) =
    createFormulaSimpleStatementNH modalVerb verb subject object receiver day month year
simpleStatementToFOL (SimStateTwoNH id subject (DateSpe day month year) modalVerb verb object receiver) =
    createFormulaSimpleStatementNH modalVerb verb subject object receiver day month year
simpleStatementToFOL (SimStateThreeNH id (DateSpe day month year) subject modalVerb verb object receiver) =
    createFormulaSimpleStatementNH modalVerb verb subject object receiver day month year

createFormulaSimpleStatement :: Holds -> ModalVerb -> Verb -> Subject -> Object -> Receiver -> Num -> Month -> Num -> FOLFormula
createFormulaSimpleStatement holds modalVerb verb subject object receiver day month year =
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
        basePredicates = 
            And
                (And
                    (And (Pred "Name" [Var "x", subjectToTerm subject]) (Pred "Name" [Var "y", receiverToTerm receiver]))
                    (Pred "Date" [Var "d", dateSpeToTerm year month day])
                )
                (objectToPredicate object)
                
        -- Predicates for different cases
        yesMustDel = ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "MustDeliver" [Var "x", Var "y", Var "o", Var "d"])
        noMustDel = Not $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "MustDeliver" [Var "x", Var "y", Var "o", Var "d"])
        yesMayDel = ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "MayDeliver" [Var "x", Var "y", Var "o", Var "d"])
        noMayDel = Not $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "MayDeliver" [Var "x", Var "y", Var "o", Var "d"])
        yesMustPay = ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "MustPay" [Var "x", Var "y", Var "o", Var "d"])
        noMustPay = Not $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "MustPay" [Var "x", Var "y", Var "o", Var "d"])
        yesMayPay = ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "MayPay" [Var "x", Var "y", Var "o", Var "d"])
        noMayPay = Not $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "MayPay" [Var "x", Var "y", Var "o", Var "d"])
        yesMustCharge = ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "MustCharge" [Var "x", Var "y", Var "o", Var "d"])
        noMustCharge = Not $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "MustCharge" [Var "x", Var "y", Var "o", Var "d"])
        yesMayCharge = ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "MayCharge" [Var "x", Var "y", Var "o", Var "d"])
        noMayCharge = Not $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "MayCharge" [Var "x", Var "y", Var "o", Var "d"])
        yesMustRefund = ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "MustRefund" [Var "x", Var "y", Var "o", Var "d"])
        noMustRefund = Not $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "MustRefund" [Var "x", Var "y", Var "o", Var "d"])
        yesMayRefund = ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "MayRefund" [Var "x", Var "y", Var "o", Var "d"])
        noMayRefund = Not $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "MayRefund" [Var "x", Var "y", Var "o", Var "d"])

createFormulaSimpleStatementNH :: ModalVerb -> Verb -> Subject -> Object -> Receiver -> Num -> Month -> Num -> FOLFormula
createFormulaSimpleStatementNH modalVerb verb subject object receiver day month year =
    case (modalVerb, verb) of
        (ModalObli _, VDel) -> yesMustDel 
        (ModalPermi, VDel) -> yesMayDel 
        (ModalForbi, VDel) -> noMayDel  
        (ModalObli _, VPay) -> yesMustPay 
        (ModalPermi, VPay) -> yesMayPay 
        (ModalForbi, VPay) -> noMayPay 
        (ModalObli _, VCharge) -> yesMustCharge 
        (ModalPermi, VCharge) -> yesMayCharge 
        (ModalForbi, VCharge) -> noMayCharge 
        (ModalObli _, VRefund) -> yesMustRefund 
        (ModalPermi, VRefund) -> yesMayRefund
        (ModalForbi, VRefund) -> noMayRefund 

    where
        -- Common base predicates
        basePredicates = 
            And
                (And
                    (And (Pred "Name" [Var "x", subjectToTerm subject]) (Pred "Name" [Var "y", receiverToTerm receiver]))
                    (Pred "Date" [Var "d", dateSpeToTerm year month day])
                )
                (objectToPredicate object)
                
        -- Predicates for different cases
        yesMustDel = ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "MustDeliver" [Var "x", Var "y", Var "o", Var "d"])
        yesMayDel = ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "MayDeliver" [Var "x", Var "y", Var "o", Var "d"])
        noMayDel = Not $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "MayDeliver" [Var "x", Var "y", Var "o", Var "d"])
        yesMustPay = ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "MustPay" [Var "x", Var "y", Var "o", Var "d"])
        yesMayPay = ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "MayPay" [Var "x", Var "y", Var "o", Var "d"])
        noMayPay = Not $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "MayPay" [Var "x", Var "y", Var "o", Var "d"])
        yesMustCharge = ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "MustCharge" [Var "x", Var "y", Var "o", Var "d"])
        yesMayCharge = ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "MayCharge" [Var "x", Var "y", Var "o", Var "d"])
        noMayCharge = Not $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "MayCharge" [Var "x", Var "y", Var "o", Var "d"])
        yesMustRefund = ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "MustRefund" [Var "x", Var "y", Var "o", Var "d"])
        yesMayRefund = ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "MayRefund" [Var "x", Var "y", Var "o", Var "d"])
        noMayRefund = Not $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "MayRefund" [Var "x", Var "y", Var "o", Var "d"])

-- Convert different kinds of objects to predicates
objectToPredicate :: Object -> FOLFormula
objectToPredicate (ObjNu (NumPound _ (NumInt num))) = Pred "ObjectPound" [Var "o", Var (show num)]
objectToPredicate (ObjNu (NumDol _ (NumInt num))) = Pred "ObjectDollar" [Var "o", Var (show num)]
objectToPredicate (ObjNu (NumEur _ (NumInt num))) = Pred "ObjectEuro" [Var "o", Var (show num)]
objectToPredicate (ObjNu (NumAmount subject)) = Pred "ObjectAmount" [Var "o", subjectToTerm subject]
objectToPredicate (ObjNonNu (NonNumCurr subject)) = Pred "ObjectSomeCurrency" [Var "o", subjectToTerm subject]
objectToPredicate (ObjNonNu (NonNumRep subject)) = Pred "ObjectReport" [Var "o", subjectToTerm subject]
objectToPredicate (ObjNonNu (NonNumNamed subject)) = Pred "ObjectNamedObject" [Var "o", subjectToTerm subject]
objectToPredicate (ObjNonNu (NonNumOther subject)) = Pred "ObjectOtherObject" [Var "o", subjectToTerm subject]

simpleConditionToFOL :: SimpleCondition -> FOLFormula
simpleConditionToFOL (SimConOne id holds subject verbStatus object receiver (DateSpe day month year)) =
    createFormulaSimpleCondition holds verbStatus subject object receiver day month year
simpleConditionToFOL (SimConTwo id holds subject (DateSpe day month year) verbStatus object receiver) = 
    createFormulaSimpleCondition holds verbStatus subject object receiver day month year
simpleConditionToFOL (SimConThree id holds (DateSpe day month year) subject verbStatus object receiver) = 
    createFormulaSimpleCondition holds verbStatus subject object receiver day month year 
simpleConditionToFOL (SimConFour id holds subject modalVerb verb object receiver (DateSpe day month year)) =
    createFormulaSimpleStatement holds modalVerb verb subject object receiver day month year
simpleConditionToFOL (SimConFive id HoldYes booleanExpression) = boolExToFOL booleanExpression
simpleConditionToFOL (SimConFive id HoldNo booleanExpression) = Not $ (boolExToFOL booleanExpression)
simpleConditionToFOL (SimConOneNH id subject verbStatus object receiver (DateSpe day month year)) =
    createFormulaSimpleConditionNH verbStatus subject object receiver day month year
simpleConditionToFOL (SimConTwoNH id subject (DateSpe day month year) verbStatus object receiver) = 
    createFormulaSimpleConditionNH verbStatus subject object receiver day month year
simpleConditionToFOL (SimConThreeNH id (DateSpe day month year) subject verbStatus object receiver) = 
    createFormulaSimpleConditionNH verbStatus subject object receiver day month year 
simpleConditionToFOL (SimConFourNH id subject modalVerb verb object receiver (DateSpe day month year)) =
    createFormulaSimpleStatementNH modalVerb verb subject object receiver day month year
simpleConditiontoFOL (SimConFiveNH id booleanExpression) = boolExToFOL booleanExpression

createFormulaSimpleCondition :: Holds -> VerbStatus -> Subject -> Object -> Receiver -> Num -> Month -> Num -> FOLFormula
createFormulaSimpleCondition holds verbStatus subject object receiver day month year =
    case (holds, verbStatus) of
        (HoldYes, VSDel) -> yesDelivered
        (HoldNo, VSDel) -> noDelivered
        (HoldYes, VSPay) -> yesPaid
        (HoldNo, VSPay) -> noPaid
        (HoldYes, VSCharge) -> yesCharged
        (HoldNo, VSCharge) -> noCharged
        (HoldYes, VSRefund) -> yesRefunded
        (HoldNo, VSRefund) -> noRefunded

    where
        -- Common base predicates
        basePredicates =
            And
                (And
                    (And (Pred "Name" [Var "x", subjectToTerm subject]) (Pred "Name" [Var "y", receiverToTerm receiver]))
                    (Pred "Date" [Var "d", dateSpeToTerm year month day])
                )
                (objectToPredicate object)
                
        -- Predicates for different cases
        yesDelivered = ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "Delivered" [Var "x", Var "y", Var "o", Var "d"])
        noDelivered = Not $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "Delivered" [Var "x", Var "y", Var "o", Var "d"])
        yesPaid = ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "Paid" [Var "x", Var "y", Var "o", Var "d"])
        noPaid = Not $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "Paid" [Var "x", Var "y", Var "o", Var "d"])
        yesCharged = ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "Charged" [Var "x", Var "y", Var "o", Var "d"])
        noCharged = Not $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "Charged" [Var "x", Var "y", Var "o", Var "d"])
        yesRefunded = ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "Refunded" [Var "x", Var "y", Var "o", Var "d"])
        noRefunded = Not $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ And basePredicates (Pred "Refunded" [Var "x", Var "y", Var "o", Var "d"])

createFormulaSimpleConditionNH :: VerbStatus -> Subject -> Object -> Receiver -> Num -> Month -> Num -> FOLFormula
createFormulaSimpleConditionNH verbStatus subject object receiver day month year =
    case verbStatus of
        VSDel -> yesDelivered
        VSPay -> yesPaid
        VSCharge -> yesCharged
        VSRefund -> yesRefunded

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
        yesDelivered = And basePredicates (Pred "Delivered" [Var "x", Var "y", Var "o", Var "d"])
        yesPaid = And basePredicates (Pred "Paid" [Var "x", Var "y", Var "o", Var "d"])
        yesCharged = And basePredicates (Pred "Charged" [Var "x", Var "y", Var "o", Var "d"])
        yesRefunded = And basePredicates (Pred "Refunded" [Var "x", Var "y", Var "o", Var "d"])

boolExToFOL :: BooleanExpression -> FOLFormula
boolExToFOL (BoolEx subject1 verbStatus comparison subject2) = 
    case (verbStatus, comparison) of
        (VSDel, CompareLess) -> deliveredLess
        (VSDel, CompareEq _) -> deliveredEqual
        (VSDel, CompareMore _) -> deliveredMore
        (VSPay, CompareLess) -> paidLess
        (VSPay, CompareEq _) -> paidEqual
        (VSPay, CompareMore _) -> paidMore
        (VSCharge, CompareLess) -> chargedLess
        (VSCharge, CompareEq _) -> chargedEqual
        (VSCharge, CompareMore _) -> chargedMore
        (VSRefund, CompareLess) -> refundedLess
        (VSRefund, CompareEq _) -> refundedEqual
        (VSRefund, CompareMore _) -> refundedMore

    where
        -- Common base predicates
        basePredicates = (And (Pred "Name" [Var "x", subjectToTerm subject1]) (Pred "Name" [Var "y", subjectToTerm subject2]))
                          
        -- Predicates for different cases
        deliveredLess = ForAll [Var "x", Var "y"] $ And basePredicates (Pred "DeliveredLess" [Var "x", Var "y"])
        deliveredEqual = ForAll [Var "x", Var "y"] $ And basePredicates (Pred "DeliveredEqual" [Var "x", Var "y"])
        deliveredMore = ForAll [Var "x", Var "y"] $ And basePredicates (Pred "DeliveredMore" [Var "x", Var "y"])
        paidLess = ForAll [Var "x", Var "y"] $ And basePredicates (Pred "PaidLess" [Var "x", Var "y"])
        paidEqual = ForAll [Var "x", Var "y"] $ And basePredicates (Pred "PaidEqual" [Var "x", Var "y"])
        paidMore = ForAll [Var "x", Var "y"] $ And basePredicates (Pred "PaidMore" [Var "x", Var "y"])
        chargedLess = ForAll [Var "x", Var "y"] $ And basePredicates (Pred "ChargedLess" [Var "x", Var "y"])
        chargedEqual = ForAll [Var "x", Var "y"] $ And basePredicates (Pred "ChargedEqual" [Var "x", Var "y"])
        chargedMore = ForAll [Var "x", Var "y"] $ And basePredicates (Pred "ChargedMore" [Var "x", Var "y"])
        refundedLess = ForAll [Var "x", Var "y"] $ And basePredicates (Pred "RefundedLess" [Var "x", Var "y"])
        refundedEqual = ForAll [Var "x", Var "y"] $ And basePredicates (Pred "RefundedEqual" [Var "x", Var "y"])
        refundedMore = ForAll [Var "x", Var "y"] $ And basePredicates (Pred "RefundedMore" [Var "x", Var "y"])
