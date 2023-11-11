
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

tempQuanToString :: TemporalQuantifier -> String
tempQuanToString TempWithin = "Within"
tempQuanToString TempAfter = "After"
tempQuanToString TempBefore = "Before"
tempQuanToString TempIn = "In"

tempRangeToTerm :: TemporalRange -> Term
tempRangeToTerm (BtwMonth month1 month2) = Fun "between" [Var (monthToString month1), Var (monthToString month2)]
tempRangeToTerm (BtwYear year1 year2) = Fun "between" [Var (numToString year1), Var (numToString year2)]

dateToTerm :: Date -> Term
dateToTerm (DateSpe day month year) = Var ((numToString day) ++ (monthToString month) ++ (numToString year))
dateToTerm (DateAny) = Var "AnyDate"
dateToTerm (DateA) = Var "ADate"
dateToTerm (DateThe)= Var "TheDate"
dateToTerm (DateMonRange temporalQuantifier month) = Fun (tempQuanToString temporalQuantifier) [Var (monthToString month)]
dateToTerm (DateYearRange temporalQuantifier year) = Fun (tempQuanToString temporalQuantifier) [Var (numToString year)]
dateToTerm (DateRange temporalRange) = tempRangeToTerm temporalRange

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

simpleStatementToFOL :: SimpleStatement -> FOLFormula
simpleStatementToFOL (SimStateOne id HoldYes subject ModalPermi verb object receiver date) =
    Exists (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleStatementToFOL (SimStateOne id HoldNo subject ModalPermi verb object receiver date) =
    Not $ Exists (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleStatementToFOL (SimStateOne id HoldYes subject ModalForbi verb object receiver date) =
    Not $ Exists (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleStatementToFOL (SimStateOne id HoldNo subject ModalForbi verb object receiver date) =
    Exists (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleStatementToFOL (SimStateOne id HoldYes subject (ModalObli obli) verb object receiver date) =
    ForAll (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleStatementToFOL (SimStateOne id HoldNo subject (ModalObli obli) verb object receiver date) =
    Not $ ForAll (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleStatementToFOL (SimStateTwo id HoldYes subject date ModalPermi verb object receiver) = 
    Exists (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleStatementToFOL (SimStateTwo id HoldNo subject date ModalPermi verb object receiver) = 
    Not $ Exists (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleStatementToFOL (SimStateTwo id HoldYes subject date ModalForbi verb object receiver) = 
    Not $ Exists (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleStatementToFOL (SimStateTwo id HoldNo subject date ModalForbi verb object receiver) = 
    Exists (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleStatementToFOL (SimStateTwo id HoldYes subject date (ModalObli obli) verb object receiver) = 
    ForAll (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleStatementToFOL (SimStateTwo id HoldNo subject date (ModalObli obli) verb object receiver) = 
    Not $ ForAll (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleStatementToFOL (SimStateThree id HoldYes date subject ModalPermi verb object receiver) = 
    Exists (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleStatementToFOL (SimStateThree id HoldNo date subject ModalPermi verb object receiver) = 
    Not $ Exists (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleStatementToFOL (SimStateThree id HoldYes date subject ModalForbi verb object receiver) = 
    Not $ Exists (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleStatementToFOL (SimStateThree id HoldNo date subject ModalForbi verb object receiver) = 
    Exists (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleStatementToFOL (SimStateThree id HoldYes date subject (ModalObli obli) verb object receiver) = 
    ForAll (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleStatementToFOL (SimStateThree id HoldNo date subject (ModalObli obli) verb object receiver) = 
    Not $ ForAll (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleStatementToFOL (SimStateOneNH id subject ModalPermi verb object receiver date) =
    Exists (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleStatementToFOL (SimStateOneNH id subject ModalForbi verb object receiver date) =
    Not $ Exists (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]
    
simpleStatementToFOL (SimStateOneNH id subject (ModalObli obli) verb object receiver date) =
    ForAll (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleStatementToFOL (SimStateTwoNH id subject date ModalPermi verb object receiver) =
    Exists (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleStatementToFOL (SimStateTwoNH id subject date ModalForbi verb object receiver) = 
    Not $ Exists (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]
    
simpleStatementToFOL (SimStateTwoNH id subject date (ModalObli obli) verb object receiver) = 
    ForAll (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleStatementToFOL (SimStateThreeNH id date subject ModalPermi verb object receiver) =
    Exists (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleStatementToFOL (SimStateThreeNH id date subject ModalForbi verb object receiver) = 
    Not $ Exists (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]
    
simpleStatementToFOL (SimStateThreeNH id date subject (ModalObli obli) verb object receiver) = 
    ForAll (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleConditionToFOL :: SimpleCondition -> FOLFormula
simpleConditionToFOL (SimConOne id HoldYes subject verbStatus object receiver date) =
    ForAll (subjectToString subject) $ Pred (getVerbStatusString verbStatus) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleConditionToFOL (SimConOne id HoldNo subject verbStatus object receiver date) =
    Not $ ForAll (subjectToString subject) $ Pred (getVerbStatusString verbStatus) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleConditionToFOL (SimConTwo id HoldYes subject date verbStatus object receiver) =
    ForAll (subjectToString subject) $ Pred (getVerbStatusString verbStatus) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleConditionToFOL (SimConTwo id HoldNo subject date verbStatus object receiver) =
    Not $ ForAll (subjectToString subject) $ Pred (getVerbStatusString verbStatus) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleConditionToFOL (SimConThree id HoldYes date subject verbStatus object receiver) =
    ForAll (subjectToString subject) $ Pred (getVerbStatusString verbStatus) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleConditionToFOL (SimConThree id HoldNo date subject verbStatus object receiver) =
    Not $ ForAll (subjectToString subject) $ Pred (getVerbStatusString verbStatus) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleConditionToFOL (SimConFour id HoldYes subject ModalPermi verb object receiver date) =
    Exists (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleConditionToFOL (SimConFour id HoldNo subject ModalPermi verb object receiver date) =
    Not $ Exists (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleConditionToFOL (SimConFour id HoldYes subject ModalForbi verb object receiver date) =
    Not $ Exists (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleConditionToFOL (SimConFour id HoldNo subject ModalForbi verb object receiver date) =
    Exists (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleConditionToFOL (SimConFour id HoldYes subject (ModalObli obli) verb object receiver date) =
    ForAll (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleConditionToFOL (SimConFour id HoldNo subject (ModalObli obli) verb object receiver date) =
    Not $ ForAll (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date] 

simpleConditionToFOL (SimConFive id HoldYes booleanExpression) = boolExToFOL booleanExpression

simpleConditionToFOL (SimConFive id HoldNo booleanExpression) = Not $ boolExToFOL booleanExpression

simpleConditionToFOL (SimConOneNH id subject verbStatus object receiver date) =
    ForAll (subjectToString subject) $ Pred (getVerbStatusString verbStatus) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleConditionToFOL (SimConTwoNH id subject date verbStatus object receiver) =
    ForAll (subjectToString subject) $ Pred (getVerbStatusString verbStatus) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleConditionToFOL (SimConThreeNH id date subject verbStatus object receiver) =
    ForAll (subjectToString subject) $ Pred (getVerbStatusString verbStatus) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleConditionToFOL (SimConFourNH id subject ModalPermi verb object receiver date) =
    Exists (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleConditionToFOL (SimConFourNH id subject ModalForbi verb object receiver date) =
    Not $ Exists (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleConditionToFOL (SimConFourNH id subject (ModalObli obli) verb object receiver date) =
    ForAll (subjectToString subject) $ Pred (getVerbString verb) [subjectToTerm subject, receiverToTerm receiver, dateToTerm date]

simpleConditionToFOL (SimConFiveNH id booleanExpression) = boolExToFOL booleanExpression

boolExToFOL :: BooleanExpression -> FOLFormula
boolExToFOL (BoolEx subject1 VSDel CompareLess subject2) = ForAll (subjectToString subject1) $ Pred "DeliveredLess" [subjectToTerm subject1, subjectToTerm subject2]
boolExToFOL (BoolEx subject1 VSDel (CompareEq eq) subject2) = ForAll (subjectToString subject1) $ Pred "DeliveredEqual" [subjectToTerm subject1, subjectToTerm subject2]
boolExToFOL (BoolEx subject1 VSDel (CompareMore more) subject2) = ForAll (subjectToString subject1) $ Pred "DeliveredMore" [subjectToTerm subject1, subjectToTerm subject2]
boolExToFOL (BoolEx subject1 VSPay CompareLess subject2) = ForAll (subjectToString subject1) $ Pred "PaidLess" [subjectToTerm subject1, subjectToTerm subject2]
boolExToFOL (BoolEx subject1 VSPay (CompareEq eq) subject2) = ForAll (subjectToString subject1) $ Pred "PaidEqual" [subjectToTerm subject1, subjectToTerm subject2]
boolExToFOL (BoolEx subject1 VSPay (CompareMore more) subject2) = ForAll (subjectToString subject1) $ Pred "PaidMore" [subjectToTerm subject1, subjectToTerm subject2]
boolExToFOL (BoolEx subject1 VSCharge CompareLess subject2) = ForAll (subjectToString subject1) $ Pred "ChargedLess" [subjectToTerm subject1, subjectToTerm subject2]
boolExToFOL (BoolEx subject1 VSCharge (CompareEq eq) subject2) = ForAll (subjectToString subject1) $ Pred "ChargedEqual" [subjectToTerm subject1, subjectToTerm subject2]
boolExToFOL (BoolEx subject1 VSCharge (CompareMore more) subject2) = ForAll (subjectToString subject1) $ Pred "ChargedMore" [subjectToTerm subject1, subjectToTerm subject2]
boolExToFOL (BoolEx subject1 VSRefund CompareLess subject2) = ForAll (subjectToString subject1) $ Pred "RefundedLess" [subjectToTerm subject1, subjectToTerm subject2]
boolExToFOL (BoolEx subject1 VSRefund (CompareEq eq) subject2) = ForAll (subjectToString subject1) $ Pred "RefundedEqual" [subjectToTerm subject1, subjectToTerm subject2]
boolExToFOL (BoolEx subject1 VSRefund (CompareMore more) subject2) = ForAll (subjectToString subject1) $ Pred "RefundedMore" [subjectToTerm subject1, subjectToTerm subject2]