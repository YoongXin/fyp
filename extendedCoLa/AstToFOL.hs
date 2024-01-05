module AstToFOL where

import Prelude
  ( ($), (<$>), (++), (+), (*), (>=), (<=)
  , Int, Integer, fromInteger
  , String
  , Show, show
  , Eq
  , Read
  , fst
  , snd
  , Bool(..)
  , Maybe(..)
  )

import Control.Monad (when)
import Data.Time
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)

import AbsCoLa   
import qualified Data.Map as Map

type DateDictionary = Map.Map String String
type TempQuanDictionary = Map.Map String (String, Integer)

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
    | Brackets FOLFormula
    | Falser
  deriving (Eq, Show, Read)

-- Function to convert a specific date to an integer
dateToInt :: Integer -> Int -> Int -> Integer
dateToInt year month day = toModifiedJulianDay (fromGregorian year month day)

connectTerms :: Subject -> Verb -> Receiver -> Object -> String
connectTerms subject verb receiver object = subjectToString subject ++ verbToString verb ++ receiverToString receiver ++ objectToString object

verbStatusToVerb :: VerbStatus -> Verb
verbStatusToVerb VSDel = VDel
verbStatusToVerb VSPay = VPay
verbStatusToVerb VSCharge = VCharge
verbStatusToVerb VSRefund = VRefund

-- Function to convert the contract to FOL formula
contractToFOL :: Contract -> State (DateDictionary, TempQuanDictionary) FOLFormula
contractToFOL (ConEmpty) = return $ Brackets $ Pred "Empty" [Var "Empty"]
contractToFOL (ConComp component) = componentToFOL component 
contractToFOL (ConAnd component contract) = do
    c1 <- Brackets <$> componentToFOL component
    c2 <- Brackets <$> contractToFOL contract
    return $ And c1 c2

componentToFOL :: Component -> State (DateDictionary, TempQuanDictionary) FOLFormula
componentToFOL (ComDef definition) = definitionToFOL definition
componentToFOL (ComConDef conditionalDefinition) = conditionalDefinitionToFOL conditionalDefinition
componentToFOL (ComState statement) = statementToFOL statement
componentToFOL (ComConState conditionalStatement) = conditionalStatementToFOL conditionalStatement

definitionToFOL :: Definition -> State (DateDictionary, TempQuanDictionary) FOLFormula
definitionToFOL (DefSim simpleDefinition) = simpleDefinitionToFOL simpleDefinition
definitionToFOL (DefAnd simpleDefinition definition) = liftM2 And (simpleDefinitionToFOL simpleDefinition) (definitionToFOL definition)

conditionalDefinitionToFOL :: ConditionalDefinition -> State (DateDictionary, TempQuanDictionary) FOLFormula
conditionalDefinitionToFOL (ConDefIf definition condition) = liftM2 Implies (conditionToFOL condition) (definitionToFOL definition)
conditionalDefinitionToFOL (ConDefIfThen condition definition) = liftM2 Implies (conditionToFOL condition) (definitionToFOL definition)
conditionalDefinitionToFOL (ConDefIfElse definition1 condition definition2) = 
    liftM2 And (liftM2 Implies (conditionToFOL condition) (definitionToFOL definition1)) (liftM2 Implies (fmap Not (conditionToFOL condition)) (definitionToFOL definition2))
conditionalDefinitionToFOL (ConDefIfThenElse condition definition1 definition2) =
    liftM2 And (liftM2 Implies (conditionToFOL condition) (definitionToFOL definition1)) (liftM2 Implies (fmap Not (conditionToFOL condition)) (definitionToFOL definition2))

statementToFOL :: Statement -> State (DateDictionary, TempQuanDictionary) FOLFormula
statementToFOL (StateSim simpleStatement) = simpleStatementToFOL simpleStatement
statementToFOL (StateOr simpleStatement statement) = liftM2 Or (simpleStatementToFOL simpleStatement) (statementToFOL statement)
statementToFOL (StateAnd simpleStatement statement) = liftM2 And (simpleStatementToFOL simpleStatement) (statementToFOL statement)

conditionalStatementToFOL :: ConditionalStatement -> State (DateDictionary, TempQuanDictionary) FOLFormula
conditionalStatementToFOL (ConStateIf statement condition) = liftM2 Implies (conditionToFOL condition) (statementToFOL statement)
conditionalStatementToFOL (ConStateIfThen condition statement) = liftM2 Implies (conditionToFOL condition) (statementToFOL statement)
conditionalStatementToFOL (ConStateIfElse statement1 condition statement2) = 
    liftM2 And (liftM2 Implies (conditionToFOL condition) (statementToFOL statement1)) (liftM2 Implies (fmap Not (conditionToFOL condition)) (statementToFOL statement2))
conditionalStatementToFOL (ConStateIfThenElse condition statement1 statement2) =
    liftM2 And (liftM2 Implies (conditionToFOL condition) (statementToFOL statement1)) (liftM2 Implies (fmap Not (conditionToFOL condition)) (statementToFOL statement2))

simpleDefinitionToFOL :: SimpleDefinition -> State (DateDictionary, TempQuanDictionary) FOLFormula
simpleDefinitionToFOL (SimDefIs id subject1 subject2) =
    case (subject1, subject2) of
        (SubQuoted str1, SubQuoted str2) -> return $ Equal (Var str1) (Var str2)
        (SubUnQuoted ident1, SubQuoted str2) -> return $ Equal (Var (getIdentString ident1)) (Var str2)
        (SubQuoted str1, SubUnQuoted ident2) -> return $ Equal (Var str1) (Var (getIdentString ident2))
        (SubUnQuoted ident1, SubUnQuoted ident2) -> return $ Equal (Var (getIdentString ident1)) (Var (getIdentString ident2))
simpleDefinitionToFOL (SimDefEq id subject numericalExpression) =
    let term1 = subjectToTerm subject
        term2 = numericalExpressionToTerm numericalExpression
    in return $ Equal term1 term2
simpleDefinitionToFOL (SimDefDate id subject day month year) =
    let term1 = subjectToTerm subject
        term2 = dateSpeToTerm year month day
    in return $ Equal term1 term2

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
            OpPlus -> "plus"
            OpMin -> "minus"
            OpMult -> "multiplication"
            OpDiv -> "division"
    --in Var (termToString term1 ++ operatorStr ++ termToString term2)
    in Fun operatorStr [term1, term2]

termToString :: Term -> String
termToString (Var str) = str
termToString (Fun name _) = name 

numericalObjectToTerm :: NumericalObject -> Term
numericalObjectToTerm (NumPound _ (NumInt n)) = Var ("Pound" ++ show n)
numericalObjectToTerm (NumDol _ (NumInt n)) = Var ("Dollar" ++ show n)
numericalObjectToTerm (NumEur _ (NumInt n)) = Var ("Euro" ++ show n)
numericalObjectToTerm (NumAmount subject) = subjectToTerm subject

conditionToFOL :: Condition -> State (DateDictionary, TempQuanDictionary) FOLFormula
conditionToFOL (CondiSim simpleCondition) = simpleConditionToFOL simpleCondition
conditionToFOL (CondiOr simpleCondition condition) = liftM2 Or (simpleConditionToFOL simpleCondition) (conditionToFOL condition)
conditionToFOL (CondiAnd simpleCondition condition) = liftM2 And (simpleConditionToFOL simpleCondition) (conditionToFOL condition)

receiverToTerm :: Receiver -> Term
receiverToTerm (Rec subject) = subjectToTerm subject

receiverToString :: Receiver -> String
receiverToString (Rec subject) = subjectToString subject

verbToString :: Verb -> String
verbToString VDel = "Deliver"
verbToString VPay = "Pay"
verbToString VCharge = "Charge"
verbToString VRefund = "Refund"

numToString :: Num -> String
numToString (NumInt num) = show num

temporalQuantifierToString :: TemporalQuantifier -> String
temporalQuantifierToString TempAfter = "After"
temporalQuantifierToString TempBefore = "Before"

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

temporalOffsetToTerm :: TemporalOffset -> Term
temporalOffsetToTerm (TempOffDay (NumInt num)) = Var $ show num
temporalOffsetToTerm (TempOffDays (NumInt num)) = Var $ show num
temporalOffsetToTerm (TempOffWeek (NumInt num)) = Var $ show (num * 7)
temporalOffsetToTerm (TempOffWeeks (NumInt num)) = Var $ show (num * 7)
temporalOffsetToTerm (TempOffYear (NumInt num)) = Var $ show (num * 365)
temporalOffsetToTerm (TempOffYears (NumInt num)) = Var $ show (num * 365)

temporalOffsetToInt :: TemporalOffset -> Integer
temporalOffsetToInt (TempOffDay (NumInt num)) = num
temporalOffsetToInt (TempOffDays (NumInt num)) = num
temporalOffsetToInt (TempOffWeek (NumInt num)) = num * 7
temporalOffsetToInt (TempOffWeeks (NumInt num)) = num * 7
temporalOffsetToInt (TempOffYear (NumInt num)) = num * 365
temporalOffsetToInt (TempOffYears (NumInt num)) = num * 365

dateSpeToTerm :: Num -> Month -> Num -> Term
dateSpeToTerm (NumInt year) month (NumInt day) = Var $ show $ dateToInt year (monthToInt month) (fromInteger day)

dateSpeToInt :: Num -> Month -> Num -> Integer
dateSpeToInt (NumInt year) month (NumInt day) = dateToInt year (monthToInt month) (fromInteger day)

-- lookupDate :: String -> State DateDictionary String
-- lookupDate date = do
--     -- Get the current state of the date dictionary
--     dateDictionary <- get

--     -- Check if the date is already in the dictionary
--     case Map.lookup date dateDictionary of
--         Just value -> return value
--         Nothing -> do
--             -- If it doesn't exist, add a new entry to the dictionary
--             let newValue = "D" ++ show (Map.size dateDictionary + 1)
--             let newDateDict = Map.insert date newValue dateDictionary
--             put newDateDict
--             return newValue

lookupDate :: String -> State (DateDictionary, TempQuanDictionary) String
lookupDate date = do
    -- Get the current state of the dictionaries
    (dateDictionary, tempQuanDictionary) <- get

    -- Check if the date is already in the dictionary
    case Map.lookup date dateDictionary of
        Just value -> return value
        Nothing -> do
            -- If it doesn't exist, add a new entry to the date dictionary
            let newValue = "D" ++ show (Map.size dateDictionary + 1)
            let newDateDict = Map.insert date newValue dateDictionary
            -- Update only the DateDictionary
            put (newDateDict, tempQuanDictionary)
            return newValue

updateTempQuanDictionary :: String -> String -> Integer -> State (DateDictionary, TempQuanDictionary) ()
updateTempQuanDictionary key value intValue = do
    -- Get the current state of the TempQuanDictionary
    (dateDictionary, tempQuanDictionary) <- get

    -- Update the TempQuanDictionary
    let newTempQuanDict = Map.insert key (value, intValue) tempQuanDictionary

    -- Update the state with the new TempQuanDictionary
    put (dateDictionary, newTempQuanDict)

checkTempQuanDictionary :: String -> Integer -> State (DateDictionary, TempQuanDictionary) Bool
checkTempQuanDictionary key intValue = do
    -- Get the current state of the TempQuanDictionary
    (dateDictionary, tempQuanDictionary) <- get

    -- Check if the key exists in the TempQuanDictionary
    case Map.lookup key tempQuanDictionary of
        Just (tempStr, tempInt) -> do
            -- Compare based on the specified condition
            case tempStr of
                "Before" -> return (intValue <= tempInt)
                "After"  -> return (intValue >= tempInt)
        Nothing -> return True -- Key not found, consider it true

simpleStatementToFOL :: SimpleStatement -> State (DateDictionary, TempQuanDictionary) FOLFormula
simpleStatementToFOL (SimStateOne id holds subject modalVerb verb object receiver (DateSpe (DateSpeOnThe day month year))) =
    createFormulaSimpleStatement holds modalVerb verb subject object receiver day month year
simpleStatementToFOL (SimStateOne id holds subject modalVerb verb object receiver (DateSpe (DateSpeOn day month year))) =
        createFormulaSimpleStatement holds modalVerb verb subject object receiver day month year
simpleStatementToFOL (SimStateOne id holds subject modalVerb verb object receiver DateAny) =
    createFormulaSimpleStatementDAny holds modalVerb verb subject object receiver 
simpleStatementToFOL (SimStateOne id holds subject modalVerb verb object receiver (DateSome date)) =
    createFormulaSimpleStatementDSomeThe holds modalVerb verb subject object receiver date (DateSome date)
simpleStatementToFOL (SimStateOne id holds subject modalVerb verb object receiver (DateThe date)) =
    createFormulaSimpleStatementDSomeThe holds modalVerb verb subject object receiver date (DateThe date)
simpleStatementToFOL (SimStateOne id holds subject modalVerb verb object receiver (DateQuanSpecific temporalQuantifier day month year)) =
    createFormulaSimpleStatementDQuanSpe holds modalVerb verb temporalQuantifier subject object receiver day month year
simpleStatementToFOL (SimStateOne id holds subject modalVerb verb object receiver (DateQuanSome temporalQuantifier date)) =
    createFormulaSimpleStatementDQuanSomeThe holds modalVerb verb temporalQuantifier subject object receiver date (DateQuanSome temporalQuantifier date)
simpleStatementToFOL (SimStateOne id holds subject modalVerb verb object receiver (DateQuanThe temporalQuantifier date)) =
    createFormulaSimpleStatementDQuanSomeThe holds modalVerb verb temporalQuantifier subject object receiver date (DateQuanSome temporalQuantifier date)
simpleStatementToFOL (SimStateOne id holds subject modalVerb verb object receiver (DateQuanTempSome temporalQuantifier temporalOffset tq date)) =
    createFormulaSimpleStatementDQuanTempSomeThe holds modalVerb verb temporalQuantifier subject object receiver temporalOffset tq date (DateQuanTempSome temporalQuantifier temporalOffset tq date)
simpleStatementToFOL (SimStateOne id holds subject modalVerb verb object receiver (DateQuanTempThe temporalQuantifier temporalOffset tq date)) =
    createFormulaSimpleStatementDQuanTempSomeThe holds modalVerb verb temporalQuantifier subject object receiver temporalOffset tq date (DateQuanTempThe temporalQuantifier temporalOffset tq date)
simpleStatementToFOL (SimStateOne id holds subject modalVerb verb object receiver (DateQuanSomeWO temporalOffset tq date)) =
    createFormulaSimpleStatementDQuanSomeTheWO holds modalVerb verb subject object receiver temporalOffset tq date (DateQuanSomeWO temporalOffset tq date)
simpleStatementToFOL (SimStateOne id holds subject modalVerb verb object receiver (DateQuanTheWO temporalOffset tq date)) =
    createFormulaSimpleStatementDQuanSomeTheWO holds modalVerb verb subject object receiver temporalOffset tq date (DateQuanTheWO temporalOffset tq date)

simpleStatementToFOL (SimStateTwo id holds subject (DateSpe (DateSpeOnThe day month year)) modalVerb verb object receiver) =
    createFormulaSimpleStatement holds modalVerb verb subject object receiver day month year
simpleStatementToFOL (SimStateTwo id holds subject (DateSpe (DateSpeOn day month year)) modalVerb verb object receiver) =
    createFormulaSimpleStatement holds modalVerb verb subject object receiver day month year
simpleStatementToFOL (SimStateTwo id holds subject DateAny modalVerb verb object receiver) =
    createFormulaSimpleStatementDAny holds modalVerb verb subject object receiver 
simpleStatementToFOL (SimStateTwo id holds subject (DateSome date) modalVerb verb object receiver) =
    createFormulaSimpleStatementDSomeThe holds modalVerb verb subject object receiver date (DateSome date)
simpleStatementToFOL (SimStateTwo id holds subject (DateThe date) modalVerb verb object receiver) =
    createFormulaSimpleStatementDSomeThe holds modalVerb verb subject object receiver date (DateThe date)
simpleStatementToFOL (SimStateTwo id holds subject (DateQuanSpecific temporalQuantifier day month year) modalVerb verb object receiver) =
    createFormulaSimpleStatementDQuanSpe holds modalVerb verb temporalQuantifier subject object receiver day month year
simpleStatementToFOL (SimStateTwo id holds subject (DateQuanSome temporalQuantifier date) modalVerb verb object receiver) =
    createFormulaSimpleStatementDQuanSomeThe holds modalVerb verb temporalQuantifier subject object receiver date (DateQuanSome temporalQuantifier date)
simpleStatementToFOL (SimStateTwo id holds subject (DateQuanThe temporalQuantifier date) modalVerb verb object receiver) =
    createFormulaSimpleStatementDQuanSomeThe holds modalVerb verb temporalQuantifier subject object receiver date (DateQuanSome temporalQuantifier date)
simpleStatementToFOL (SimStateTwo id holds subject (DateQuanTempSome temporalQuantifier temporalOffset tq date) modalVerb verb object receiver) =
    createFormulaSimpleStatementDQuanTempSomeThe holds modalVerb verb temporalQuantifier subject object receiver temporalOffset tq date (DateQuanTempSome temporalQuantifier temporalOffset tq date)
simpleStatementToFOL (SimStateTwo id holds subject (DateQuanTempThe temporalQuantifier temporalOffset tq date) modalVerb verb object receiver) =
    createFormulaSimpleStatementDQuanTempSomeThe holds modalVerb verb temporalQuantifier subject object receiver temporalOffset tq date (DateQuanTempThe temporalQuantifier temporalOffset tq date)
simpleStatementToFOL (SimStateTwo id holds subject (DateQuanSomeWO temporalOffset tq date) modalVerb verb object receiver) =
    createFormulaSimpleStatementDQuanSomeTheWO holds modalVerb verb subject object receiver temporalOffset tq date (DateQuanSomeWO temporalOffset tq date)
simpleStatementToFOL (SimStateTwo id holds subject (DateQuanTheWO temporalOffset tq date) modalVerb verb object receiver) =
    createFormulaSimpleStatementDQuanSomeTheWO holds modalVerb verb subject object receiver temporalOffset tq date (DateQuanTheWO temporalOffset tq date)

simpleStatementToFOL (SimStateThree id holds (DateSpe (DateSpeOnThe day month year)) subject modalVerb verb object receiver) =
    createFormulaSimpleStatement holds modalVerb verb subject object receiver day month year
simpleStatementToFOL (SimStateThree id holds (DateSpe (DateSpeOn day month year)) subject modalVerb verb object receiver) =
    createFormulaSimpleStatement holds modalVerb verb subject object receiver day month year
simpleStatementToFOL (SimStateThree id holds DateAny subject modalVerb verb object receiver) =
    createFormulaSimpleStatementDAny holds modalVerb verb subject object receiver
simpleStatementToFOL (SimStateThree id holds (DateSome date) subject modalVerb verb object receiver) =
    createFormulaSimpleStatementDSomeThe holds modalVerb verb subject object receiver date (DateSome date)
simpleStatementToFOL (SimStateThree id holds (DateThe date) subject modalVerb verb object receiver) =
    createFormulaSimpleStatementDSomeThe holds modalVerb verb subject object receiver date (DateThe date)
simpleStatementToFOL (SimStateThree id holds (DateQuanSpecific temporalQuantifier day month year) subject modalVerb verb object receiver) =
    createFormulaSimpleStatementDQuanSpe holds modalVerb verb temporalQuantifier subject object receiver day month year
simpleStatementToFOL (SimStateThree id holds (DateQuanSome temporalQuantifier date) subject modalVerb verb object receiver) =
    createFormulaSimpleStatementDQuanSomeThe holds modalVerb verb temporalQuantifier subject object receiver date (DateQuanSome temporalQuantifier date)
simpleStatementToFOL (SimStateThree id holds (DateQuanThe temporalQuantifier date) subject modalVerb verb object receiver) =
    createFormulaSimpleStatementDQuanSomeThe holds modalVerb verb temporalQuantifier subject object receiver date (DateQuanSome temporalQuantifier date)
simpleStatementToFOL (SimStateThree id holds (DateQuanTempSome temporalQuantifier temporalOffset tq date) subject modalVerb verb object receiver) =
    createFormulaSimpleStatementDQuanTempSomeThe holds modalVerb verb temporalQuantifier subject object receiver temporalOffset tq date (DateQuanTempSome temporalQuantifier temporalOffset tq date)
simpleStatementToFOL (SimStateThree id holds (DateQuanTempThe temporalQuantifier temporalOffset tq date) subject modalVerb verb object receiver) =
    createFormulaSimpleStatementDQuanTempSomeThe holds modalVerb verb temporalQuantifier subject object receiver temporalOffset tq date (DateQuanTempThe temporalQuantifier temporalOffset tq date)
simpleStatementToFOL (SimStateThree id holds (DateQuanSomeWO temporalOffset tq date) subject modalVerb verb object receiver) =
    createFormulaSimpleStatementDQuanSomeTheWO holds modalVerb verb subject object receiver temporalOffset tq date (DateQuanSomeWO temporalOffset tq date)
simpleStatementToFOL (SimStateThree id holds (DateQuanTheWO temporalOffset tq date) subject modalVerb verb object receiver) =
    createFormulaSimpleStatementDQuanSomeTheWO holds modalVerb verb subject object receiver temporalOffset tq date (DateQuanTheWO temporalOffset tq date)

simpleStatementToFOL (SimStateFour id holds subject verbStatus object receiver (DateSpe (DateSpeOnThe day month year))) =
    createFormulaSimpleStatementDateCheck holds verbStatus subject object receiver day month year
simpleStatementToFOL (SimStateFour id holds subject verbStatus object receiver (DateSpe (DateSpeOn day month year))) =
    createFormulaSimpleStatementDateCheck holds verbStatus subject object receiver day month year
simpleStatementToFOL (SimStateFour id holds subject verbStatus object receiver DateAny) =
    createFormulaSimpleConditionDAny holds verbStatus subject object receiver
simpleStatementToFOL (SimStateFour id holds subject verbStatus object receiver (DateSome date)) =
    createFormulaSimpleConditionDSomeThe holds verbStatus subject object receiver date (DateSome date)
simpleStatementToFOL (SimStateFour id holds subject verbStatus object receiver (DateThe date)) =
    createFormulaSimpleConditionDSomeThe holds verbStatus subject object receiver date (DateThe date)
simpleStatementToFOL (SimStateFour id holds subject verbStatus object receiver (DateQuanSpecific temporalQuantifier day month year)) =
    createFormulaSimpleConditionDQuanSpe holds verbStatus temporalQuantifier subject object receiver day month year
simpleStatementToFOL (SimStateFour id holds subject verbStatus object receiver (DateQuanSome temporalQuantifier date)) =
    createFormulaSimpleConditionDQuanSomeThe holds verbStatus temporalQuantifier subject object receiver date (DateQuanSome temporalQuantifier date)
simpleStatementToFOL (SimStateFour id holds subject verbStatus object receiver (DateQuanThe temporalQuantifier date)) =
    createFormulaSimpleConditionDQuanSomeThe holds verbStatus temporalQuantifier subject object receiver date (DateQuanThe temporalQuantifier date)
simpleStatementToFOL (SimStateFour id holds subject verbStatus object receiver (DateQuanTempSome temporalQuantifier temporalOffset tq date)) =
    createFormulaSimpleConditionDQuanTempSomeThe holds verbStatus temporalQuantifier subject object receiver temporalOffset tq date (DateQuanTempSome temporalQuantifier temporalOffset tq date)
simpleStatementToFOL (SimStateFour id holds subject verbStatus object receiver (DateQuanTempThe temporalQuantifier temporalOffset tq date)) =
    createFormulaSimpleConditionDQuanTempSomeThe holds verbStatus temporalQuantifier subject object receiver temporalOffset tq date (DateQuanTempThe temporalQuantifier temporalOffset tq date)
simpleStatementToFOL (SimStateFour id holds subject verbStatus object receiver (DateQuanSomeWO temporalOffset tq date)) =
    createFormulaSimpleStatementDateCheckSomeThe holds verbStatus subject object receiver temporalOffset tq date (DateQuanSomeWO temporalOffset tq date)
simpleStatementToFOL (SimStateFour id holds subject verbStatus object receiver (DateQuanTheWO temporalOffset tq date)) =
    createFormulaSimpleStatementDateCheckSomeThe holds verbStatus subject object receiver temporalOffset tq date (DateQuanTheWO temporalOffset tq date)

simpleStatementToFOL (SimStateOneNH id subject modalVerb verb object receiver (DateSpe (DateSpeOnThe day month year))) =
    createFormulaSimpleStatementNH modalVerb verb subject object receiver day month year
simpleStatementToFOL (SimStateOneNH id subject modalVerb verb object receiver (DateSpe (DateSpeOn day month year))) =
    createFormulaSimpleStatementNH modalVerb verb subject object receiver day month year
simpleStatementToFOL (SimStateOneNH id subject modalVerb verb object receiver DateAny) =
    createFormulaSimpleStatementNHDAny modalVerb verb subject object receiver
simpleStatementToFOL (SimStateOneNH id subject modalVerb verb object receiver (DateSome date)) =
    createFormulaSimpleStatementNHDSomeThe modalVerb verb subject object receiver date (DateSome date)
simpleStatementToFOL (SimStateOneNH id subject modalVerb verb object receiver (DateThe date)) =
    createFormulaSimpleStatementNHDSomeThe modalVerb verb subject object receiver date (DateThe date)
simpleStatementToFOL (SimStateOneNH id subject modalVerb verb object receiver (DateQuanSpecific temporalQuantifier day month year)) =
    createFormulaSimpleStatementNHDQuanSpe modalVerb verb temporalQuantifier subject object receiver day month year
simpleStatementToFOL (SimStateOneNH id subject modalVerb verb object receiver (DateQuanSome temporalQuantifier date)) =
    createFormulaSimpleStatementNHDQuanSomeThe modalVerb verb temporalQuantifier subject object receiver date (DateQuanSome temporalQuantifier date)
simpleStatementToFOL (SimStateOneNH id subject modalVerb verb object receiver (DateQuanThe temporalQuantifier date)) =
    createFormulaSimpleStatementNHDQuanSomeThe modalVerb verb temporalQuantifier subject object receiver date (DateQuanThe temporalQuantifier date)
simpleStatementToFOL (SimStateOneNH id subject modalVerb verb object receiver (DateQuanTempSome temporalQuantifier temporalOffset tq date)) =
    createFormulaSimpleStatementNHDQuanTempSomeThe modalVerb verb temporalQuantifier subject object receiver temporalOffset tq date (DateQuanTempSome temporalQuantifier temporalOffset tq date)
simpleStatementToFOL (SimStateOneNH id subject modalVerb verb object receiver (DateQuanTempThe temporalQuantifier temporalOffset tq date)) =
    createFormulaSimpleStatementNHDQuanTempSomeThe modalVerb verb temporalQuantifier subject object receiver temporalOffset tq date (DateQuanTempThe temporalQuantifier temporalOffset tq date)
simpleStatementToFOL (SimStateOneNH id subject modalVerb verb object receiver (DateQuanSomeWO temporalOffset tq date)) =
    createFormulaSimpleStatementNHDQuanSomeTheWO modalVerb verb subject object receiver temporalOffset tq date (DateQuanSomeWO temporalOffset tq date)
simpleStatementToFOL (SimStateOneNH id subject modalVerb verb object receiver (DateQuanTheWO temporalOffset tq date)) =
    createFormulaSimpleStatementNHDQuanSomeTheWO modalVerb verb subject object receiver temporalOffset tq date (DateQuanTheWO temporalOffset tq date)

simpleStatementToFOL (SimStateTwoNH id subject (DateSpe (DateSpeOnThe day month year)) modalVerb verb object receiver) =
    createFormulaSimpleStatementNH modalVerb verb subject object receiver day month year
simpleStatementToFOL (SimStateTwoNH id subject (DateSpe (DateSpeOn day month year)) modalVerb verb object receiver) =
    createFormulaSimpleStatementNH modalVerb verb subject object receiver day month year
simpleStatementToFOL (SimStateTwoNH id subject DateAny modalVerb verb object receiver) =
    createFormulaSimpleStatementNHDAny modalVerb verb subject object receiver
simpleStatementToFOL (SimStateTwoNH id subject (DateSome date) modalVerb verb object receiver) =
    createFormulaSimpleStatementNHDSomeThe modalVerb verb subject object receiver date (DateSome date)
simpleStatementToFOL (SimStateTwoNH id subject (DateThe date) modalVerb verb object receiver) =
    createFormulaSimpleStatementNHDSomeThe modalVerb verb subject object receiver date (DateThe date)
simpleStatementToFOL (SimStateTwoNH id subject (DateQuanSpecific temporalQuantifier day month year) modalVerb verb object receiver) =
    createFormulaSimpleStatementNHDQuanSpe modalVerb verb temporalQuantifier subject object receiver day month year
simpleStatementToFOL (SimStateTwoNH id subject (DateQuanSome temporalQuantifier date) modalVerb verb object receiver) =
    createFormulaSimpleStatementNHDQuanSomeThe modalVerb verb temporalQuantifier subject object receiver date (DateQuanSome temporalQuantifier date)
simpleStatementToFOL (SimStateTwoNH id subject (DateQuanThe temporalQuantifier date) modalVerb verb object receiver) =
    createFormulaSimpleStatementNHDQuanSomeThe modalVerb verb temporalQuantifier subject object receiver date (DateQuanThe temporalQuantifier date)
simpleStatementToFOL (SimStateTwoNH id subject (DateQuanTempSome temporalQuantifier temporalOffset tq date) modalVerb verb object receiver) =
    createFormulaSimpleStatementNHDQuanTempSomeThe modalVerb verb temporalQuantifier subject object receiver temporalOffset tq date (DateQuanTempSome temporalQuantifier temporalOffset tq date)
simpleStatementToFOL (SimStateTwoNH id subject (DateQuanTempThe temporalQuantifier temporalOffset tq date) modalVerb verb object receiver) =
    createFormulaSimpleStatementNHDQuanTempSomeThe modalVerb verb temporalQuantifier subject object receiver temporalOffset tq date (DateQuanTempThe temporalQuantifier temporalOffset tq date)
simpleStatementToFOL (SimStateTwoNH id subject (DateQuanSomeWO temporalOffset tq date) modalVerb verb object receiver) =
    createFormulaSimpleStatementNHDQuanSomeTheWO modalVerb verb subject object receiver temporalOffset tq date (DateQuanSomeWO temporalOffset tq date)
simpleStatementToFOL (SimStateTwoNH id subject (DateQuanTheWO temporalOffset tq date) modalVerb verb object receiver) =
    createFormulaSimpleStatementNHDQuanSomeTheWO modalVerb verb subject object receiver temporalOffset tq date (DateQuanTheWO temporalOffset tq date)
    
simpleStatementToFOL (SimStateThreeNH id (DateSpe (DateSpeOnThe day month year)) subject modalVerb verb object receiver) =
    createFormulaSimpleStatementNH modalVerb verb subject object receiver day month year
simpleStatementToFOL (SimStateThreeNH id (DateSpe (DateSpeOn day month year)) subject modalVerb verb object receiver) =
    createFormulaSimpleStatementNH modalVerb verb subject object receiver day month year
simpleStatementToFOL (SimStateThreeNH id DateAny subject modalVerb verb object receiver) =
    createFormulaSimpleStatementNHDAny modalVerb verb subject object receiver
simpleStatementToFOL (SimStateThreeNH id (DateSome date) subject modalVerb verb object receiver) =
    createFormulaSimpleStatementNHDSomeThe modalVerb verb subject object receiver date (DateSome date)
simpleStatementToFOL (SimStateThreeNH id (DateThe date) subject modalVerb verb object receiver) =
    createFormulaSimpleStatementNHDSomeThe modalVerb verb subject object receiver date (DateThe date)
simpleStatementToFOL (SimStateThreeNH id (DateQuanSpecific temporalQuantifier day month year) subject modalVerb verb object receiver) =
    createFormulaSimpleStatementNHDQuanSpe modalVerb verb temporalQuantifier subject object receiver day month year
simpleStatementToFOL (SimStateThreeNH id (DateQuanSome temporalQuantifier date) subject modalVerb verb object receiver) =
    createFormulaSimpleStatementNHDQuanSomeThe modalVerb verb temporalQuantifier subject object receiver date (DateQuanSome temporalQuantifier date)
simpleStatementToFOL (SimStateThreeNH id (DateQuanThe temporalQuantifier date) subject modalVerb verb object receiver) =
    createFormulaSimpleStatementNHDQuanSomeThe modalVerb verb temporalQuantifier subject object receiver date (DateQuanThe temporalQuantifier date)
simpleStatementToFOL (SimStateThreeNH id (DateQuanTempSome temporalQuantifier temporalOffset tq date) subject modalVerb verb object receiver) =
    createFormulaSimpleStatementNHDQuanTempSomeThe modalVerb verb temporalQuantifier subject object receiver temporalOffset tq date (DateQuanTempSome temporalQuantifier temporalOffset tq date)
simpleStatementToFOL (SimStateThreeNH id (DateQuanTempThe temporalQuantifier temporalOffset tq date) subject modalVerb verb object receiver) =
    createFormulaSimpleStatementNHDQuanTempSomeThe modalVerb verb temporalQuantifier subject object receiver temporalOffset tq date (DateQuanTempThe temporalQuantifier temporalOffset tq date)
simpleStatementToFOL (SimStateThreeNH id (DateQuanSomeWO temporalOffset tq date) subject modalVerb verb object receiver) =
    createFormulaSimpleStatementNHDQuanSomeTheWO modalVerb verb subject object receiver temporalOffset tq date (DateQuanSomeWO temporalOffset tq date)
simpleStatementToFOL (SimStateThreeNH id (DateQuanTheWO temporalOffset tq date) subject modalVerb verb object receiver) =
    createFormulaSimpleStatementNHDQuanSomeTheWO modalVerb verb subject object receiver temporalOffset tq date (DateQuanTheWO temporalOffset tq date)

simpleStatementToFOL (SimStateFourNH id subject verbStatus object receiver (DateSpe (DateSpeOnThe day month year))) =
    createFormulaSimpleStatementNHDateCheck verbStatus subject object receiver day month year
simpleStatementToFOL (SimStateFourNH id subject verbStatus object receiver (DateSpe (DateSpeOn day month year))) =
        createFormulaSimpleStatementNHDateCheck verbStatus subject object receiver day month year 
simpleStatementToFOL (SimStateFourNH id subject verbStatus object receiver DateAny) =
    createFormulaSimpleConditionNHDAny verbStatus subject object receiver 
simpleStatementToFOL (SimStateFourNH id subject verbStatus object receiver (DateSome date)) =
    createFormulaSimpleConditionNHDSomeThe verbStatus subject object receiver date (DateSome date)
simpleStatementToFOL (SimStateFourNH id subject verbStatus object receiver (DateThe date)) =
    createFormulaSimpleConditionNHDSomeThe verbStatus subject object receiver date (DateThe date)
simpleStatementToFOL (SimStateFourNH id subject verbStatus object receiver (DateQuanSpecific temporalQuantifier day month year)) =
    createFormulaSimpleConditionNHDQuanSpe verbStatus temporalQuantifier subject object receiver day month year
simpleStatementToFOL (SimStateFourNH id subject verbStatus object receiver (DateQuanSome temporalQuantifier date)) =
    createFormulaSimpleConditionNHDQuanSomeThe verbStatus temporalQuantifier subject object receiver date (DateQuanSome temporalQuantifier date)
simpleStatementToFOL (SimStateFourNH id subject verbStatus object receiver (DateQuanThe temporalQuantifier date)) =
    createFormulaSimpleConditionNHDQuanSomeThe verbStatus temporalQuantifier subject object receiver date (DateQuanThe temporalQuantifier date)
simpleStatementToFOL (SimStateFourNH id subject verbStatus object receiver (DateQuanTempSome temporalQuantifier temporalOffset tq date)) =
    createFormulaSimpleConditionNHDQuanTempSomeThe verbStatus temporalQuantifier subject object receiver temporalOffset tq date (DateQuanTempSome temporalQuantifier temporalOffset tq date)
simpleStatementToFOL (SimStateFourNH id subject verbStatus object receiver (DateQuanTempThe temporalQuantifier temporalOffset tq date)) =
    createFormulaSimpleConditionNHDQuanTempSomeThe verbStatus temporalQuantifier subject object receiver temporalOffset tq date (DateQuanTempThe temporalQuantifier temporalOffset tq date)
simpleStatementToFOL (SimStateFourNH id subject verbStatus object receiver (DateQuanSomeWO temporalOffset tq date)) =
    createFormulaSimpleStatementNHDateCheckSomeThe verbStatus subject object receiver temporalOffset tq date (DateQuanSomeWO temporalOffset tq date)
simpleStatementToFOL (SimStateFourNH id subject verbStatus object receiver (DateQuanTheWO temporalOffset tq date)) =
    createFormulaSimpleStatementNHDateCheckSomeThe verbStatus subject object receiver temporalOffset tq date (DateQuanTheWO temporalOffset tq date)

createFormulaSimpleStatement :: Holds -> ModalVerb -> Verb -> Subject -> Object -> Receiver -> Num -> Month -> Num -> State (DateDictionary, TempQuanDictionary) FOLFormula
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
                    (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                    (Pred "date" [Var "D", dateSpeToTerm year month day])
                )
                (objectToPredicate object)
                
        -- Predicates for different cases
        yesMustDel = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustDeliver" [Var "X", Var "Y", Var "O", Var "D"])
        noMustDel = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustDeliver" [Var "X", Var "Y", Var "O", Var "D"])
        yesMayDel = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayDeliver" [Var "X", Var "Y", Var "O", Var "D"])
        noMayDel = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayDeliver" [Var "X", Var "Y", Var "O", Var "D"])
        yesMustPay = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustPay" [Var "X", Var "Y", Var "O", Var "D"])
        noMustPay = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustPay" [Var "X", Var "Y", Var "O", Var "D"])
        yesMayPay = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayPay" [Var "X", Var "Y", Var "O", Var "D"])
        noMayPay = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayPay" [Var "X", Var "Y", Var "O", Var "D"])
        yesMustCharge = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustCharge" [Var "X", Var "Y", Var "O", Var "D"])
        noMustCharge = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustCharge" [Var "X", Var "Y", Var "O", Var "D"])
        yesMayCharge = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayCharge" [Var "X", Var "Y", Var "O", Var "D"])
        noMayCharge = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayCharge" [Var "X", Var "Y", Var "O", Var "D"])
        yesMustRefund = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustRefund" [Var "X", Var "Y", Var "O", Var "D"])
        noMustRefund = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustRefund" [Var "X", Var "Y", Var "O", Var "D"])
        yesMayRefund = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayRefund" [Var "X", Var "Y", Var "O", Var "D"])
        noMayRefund = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayRefund" [Var "X", Var "Y", Var "O", Var "D"])

createFormulaSimpleStatementDAny :: Holds -> ModalVerb -> Verb -> Subject -> Object -> Receiver -> State (DateDictionary, TempQuanDictionary) FOLFormula
createFormulaSimpleStatementDAny holds modalVerb verb subject object receiver =
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
                (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                (objectToPredicate object)
                
        -- Predicates for different cases
        yesMustDel = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "mustDeliver" [Var "X", Var "Y", Var "O", Var "D"])
        noMustDel = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "mustDeliver" [Var "X", Var "Y", Var "O", Var "D"])
        yesMayDel = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "mayDeliver" [Var "X", Var "Y", Var "O", Var "D"])
        noMayDel = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "mayDeliver" [Var "X", Var "Y", Var "O", Var "D"])
        yesMustPay = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "mustPay" [Var "X", Var "Y", Var "O", Var "D"])
        noMustPay = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "mustPay" [Var "X", Var "Y", Var "O", Var "D"])
        yesMayPay = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "mayPay" [Var "X", Var "Y", Var "O", Var "D"])
        noMayPay = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "mayPay" [Var "X", Var "Y", Var "O", Var "D"])
        yesMustCharge = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "mustCharge" [Var "X", Var "Y", Var "O", Var "D"])
        noMustCharge = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "mustCharge" [Var "X", Var "Y", Var "O", Var "D"])
        yesMayCharge = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "mayCharge" [Var "X", Var "Y", Var "O", Var "D"])
        noMayCharge = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "mayCharge" [Var "X", Var "Y", Var "O", Var "D"])
        yesMustRefund = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "mustRefund" [Var "X", Var "Y", Var "O", Var "D"])
        noMustRefund = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "mustRefund" [Var "X", Var "Y", Var "O", Var "D"])
        yesMayRefund = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "mayRefund" [Var "X", Var "Y", Var "O", Var "D"])
        noMayRefund = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "mayRefund" [Var "X", Var "Y", Var "O", Var "D"])

createFormulaSimpleStatementDSomeThe :: Holds -> ModalVerb -> Verb -> Subject -> Object -> Receiver -> Subject -> Date -> State (DateDictionary, TempQuanDictionary) FOLFormula
createFormulaSimpleStatementDSomeThe holds modalVerb verb subject object receiver date dateType = do
    -- Get the actual value associated with the date
    actualValue <- lookupDate (subjectToString date)

    -- Use actualValue here
    let basePredicates = case dateType of
            DateSome _ -> And
                            (And
                                (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                (Pred "isDate" [Var actualValue])
                            )
                            (objectToPredicate object)
            DateThe _ -> And
                            (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                            (objectToPredicate object)

    -- Generate FOL formulas based on different cases
    generateFormulas basePredicates actualValue

    where
        -- Function to generate FOL formulas based on different cases
        generateFormulas basePredicates actualValue = 
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
                -- Predicates for different cases
                yesMustDel = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustDeliver" [Var "X", Var "Y", Var "O", Var actualValue])
                noMustDel = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustDeliver" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMayDel = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayDeliver" [Var "X", Var "Y", Var "O", Var actualValue])
                noMayDel = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayDeliver" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMustPay = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustPay" [Var "X", Var "Y", Var "O", Var actualValue])
                noMustPay = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustPay" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMayPay = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayPay" [Var "X", Var "Y", Var "O", Var actualValue])
                noMayPay = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayPay" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMustCharge = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustCharge" [Var "X", Var "Y", Var "O", Var actualValue])
                noMustCharge = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustCharge" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMayCharge = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayCharge" [Var "X", Var "Y", Var "O", Var actualValue])
                noMayCharge = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayCharge" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMustRefund = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustRefund" [Var "X", Var "Y", Var "O", Var actualValue])
                noMustRefund = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustRefund" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMayRefund = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayRefund" [Var "X", Var "Y", Var "O", Var actualValue])
                noMayRefund = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayRefund" [Var "X", Var "Y", Var "O", Var actualValue])

createFormulaSimpleStatementDQuanSpe :: Holds -> ModalVerb -> Verb -> TemporalQuantifier -> Subject -> Object -> Receiver -> Num -> Month -> Num -> State (DateDictionary, TempQuanDictionary) FOLFormula
createFormulaSimpleStatementDQuanSpe holds modalVerb verb temporalQuantifier subject object receiver day month year =
    case (holds, modalVerb, verb, temporalQuantifier) of
        (HoldYes, ModalObli _, VDel, TempBefore) -> do
            let formula = yesMustDelBefore
            updateTempQuanDictionary (connectTerms subject verb receiver object) (temporalQuantifierToString temporalQuantifier) (dateSpeToInt year month day)
            formula

        (HoldNo, ModalObli _, VDel, TempBefore) -> noMustDelBefore
        (HoldYes, ModalPermi, VDel, TempBefore) -> yesMayDelBefore
        (HoldNo, ModalPermi, VDel, TempBefore) -> noMayDelBefore
        (HoldYes, ModalForbi, VDel, TempBefore) -> noMayDelBefore
        (HoldNo, ModalForbi, VDel, TempBefore) -> yesMayDelBefore

        (HoldYes, ModalObli _, VPay, TempBefore) -> do
            let formula = yesMustPayBefore
            updateTempQuanDictionary (connectTerms subject verb receiver object) (temporalQuantifierToString temporalQuantifier) (dateSpeToInt year month day)
            formula

        (HoldNo, ModalObli _, VPay, TempBefore) -> noMustPayBefore
        (HoldYes, ModalPermi, VPay, TempBefore) -> yesMayPayBefore
        (HoldNo, ModalPermi, VPay, TempBefore) -> noMayPayBefore
        (HoldYes, ModalForbi, VPay, TempBefore) -> noMayPayBefore
        (HoldNo, ModalForbi, VPay, TempBefore) -> yesMayPayBefore

        (HoldYes, ModalObli _, VCharge, TempBefore) -> do
            let formula = yesMustChargeBefore
            updateTempQuanDictionary (connectTerms subject verb receiver object) (temporalQuantifierToString temporalQuantifier) (dateSpeToInt year month day)
            formula

        (HoldNo, ModalObli _, VCharge, TempBefore) -> noMustChargeBefore
        (HoldYes, ModalPermi, VCharge, TempBefore) -> yesMayChargeBefore
        (HoldNo, ModalPermi, VCharge, TempBefore) -> noMayChargeBefore
        (HoldYes, ModalForbi, VCharge, TempBefore) -> noMayChargeBefore
        (HoldNo, ModalForbi, VCharge, TempBefore) -> yesMayChargeBefore

        (HoldYes, ModalObli _, VRefund, TempBefore) -> do
            let formula = yesMustRefundBefore
            updateTempQuanDictionary (connectTerms subject verb receiver object) (temporalQuantifierToString temporalQuantifier) (dateSpeToInt year month day)
            formula

        (HoldNo, ModalObli _, VRefund, TempBefore) -> noMustRefundBefore
        (HoldYes, ModalPermi, VRefund, TempBefore) -> yesMayRefundBefore
        (HoldNo, ModalPermi, VRefund, TempBefore) -> noMayRefundBefore
        (HoldYes, ModalForbi, VRefund, TempBefore) -> noMayRefundBefore
        (HoldNo, ModalForbi, VRefund, TempBefore) -> yesMayRefundBefore

        (HoldYes, ModalObli _, VDel, TempAfter) -> do
            let formula = yesMustDelAfter
            updateTempQuanDictionary (connectTerms subject verb receiver object) (temporalQuantifierToString temporalQuantifier) (dateSpeToInt year month day)
            formula

        (HoldNo, ModalObli _, VDel, TempAfter) -> noMustDelAfter
        (HoldYes, ModalPermi, VDel, TempAfter) -> yesMayDelAfter
        (HoldNo, ModalPermi, VDel, TempAfter) -> noMayDelAfter
        (HoldYes, ModalForbi, VDel, TempAfter) -> noMayDelAfter
        (HoldNo, ModalForbi, VDel, TempAfter) -> yesMayDelAfter

        (HoldYes, ModalObli _, VPay, TempAfter) -> do
            let formula = yesMustPayAfter
            updateTempQuanDictionary (connectTerms subject verb receiver object) (temporalQuantifierToString temporalQuantifier) (dateSpeToInt year month day)
            formula

        (HoldNo, ModalObli _, VPay, TempAfter) -> noMustPayAfter 
        (HoldYes, ModalPermi, VPay, TempAfter) -> yesMayPayAfter
        (HoldNo, ModalPermi, VPay, TempAfter) -> noMayPayAfter 
        (HoldYes, ModalForbi, VPay, TempAfter) -> noMayPayAfter
        (HoldNo, ModalForbi, VPay, TempAfter) -> yesMayPayAfter

        (HoldYes, ModalObli _, VCharge, TempAfter) -> do
            let formula = yesMustChargeAfter
            updateTempQuanDictionary (connectTerms subject verb receiver object) (temporalQuantifierToString temporalQuantifier) (dateSpeToInt year month day)
            formula

        (HoldNo, ModalObli _, VCharge, TempAfter) -> noMustChargeAfter
        (HoldYes, ModalPermi, VCharge, TempAfter) -> yesMayChargeAfter
        (HoldNo, ModalPermi, VCharge, TempAfter) -> noMayChargeAfter
        (HoldYes, ModalForbi, VCharge, TempAfter) -> noMayChargeAfter
        (HoldNo, ModalForbi, VCharge, TempAfter) -> yesMayChargeAfter

        (HoldYes, ModalObli _, VRefund, TempAfter) -> do
            let formula = yesMustRefundAfter
            updateTempQuanDictionary (connectTerms subject verb receiver object) (temporalQuantifierToString temporalQuantifier) (dateSpeToInt year month day)
            formula

        (HoldNo, ModalObli _, VRefund, TempAfter) -> noMustRefundAfter
        (HoldYes, ModalPermi, VRefund, TempAfter) -> yesMayRefundAfter
        (HoldNo, ModalPermi, VRefund, TempAfter) -> noMayRefundAfter
        (HoldYes, ModalForbi, VRefund, TempAfter) -> noMayRefundAfter
        (HoldNo, ModalForbi, VRefund, TempAfter) -> yesMayRefundAfter

    where
        -- Common base predicates
        basePredicates = 
            And
                (And
                    (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                    (Pred "date" [Var "D", dateSpeToTerm year month day])
                )
                (objectToPredicate object)
                
        -- Predicates for different cases
        yesMustDelBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustDeliverBefore" [Var "X", Var "Y", Var "O", Var "D"])
        noMustDelBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustDeliverBefore" [Var "X", Var "Y", Var "O", Var "D"])
        yesMayDelBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayDeliverBefore" [Var "X", Var "Y", Var "O", Var "D"])
        noMayDelBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayDeliverBefore" [Var "X", Var "Y", Var "O", Var "D"])
        yesMustPayBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustPayBefore" [Var "X", Var "Y", Var "O", Var "D"])
        noMustPayBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustPayBefore" [Var "X", Var "Y", Var "O", Var "D"])
        yesMayPayBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayPayBefore" [Var "X", Var "Y", Var "O", Var "D"])
        noMayPayBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayPayBefore" [Var "X", Var "Y", Var "O", Var "D"])
        yesMustChargeBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustChargeBefore" [Var "X", Var "Y", Var "O", Var "D"])
        noMustChargeBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustChargeBefore" [Var "X", Var "Y", Var "O", Var "D"])
        yesMayChargeBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayChargeBefore" [Var "X", Var "Y", Var "O", Var "D"])
        noMayChargeBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayChargeBefore" [Var "X", Var "Y", Var "O", Var "D"])
        yesMustRefundBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustRefundBefore" [Var "X", Var "Y", Var "O", Var "D"])
        noMustRefundBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustRefundBefore" [Var "X", Var "Y", Var "O", Var "D"])
        yesMayRefundBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayRefundBefore" [Var "X", Var "Y", Var "O", Var "D"])
        noMayRefundBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayRefundBefore" [Var "X", Var "Y", Var "O", Var "D"])
        yesMustDelAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustDeliverAfter" [Var "X", Var "Y", Var "O", Var "D"])
        noMustDelAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustDeliverAfter" [Var "X", Var "Y", Var "O", Var "D"])
        yesMayDelAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayDeliverAfter" [Var "X", Var "Y", Var "O", Var "D"])
        noMayDelAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayDeliverAfter" [Var "X", Var "Y", Var "O", Var "D"])
        yesMustPayAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustPayAfter" [Var "X", Var "Y", Var "O", Var "D"])
        noMustPayAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustPayAfter" [Var "X", Var "Y", Var "O", Var "D"])
        yesMayPayAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayPayAfter" [Var "X", Var "Y", Var "O", Var "D"])
        noMayPayAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayPayAfter" [Var "X", Var "Y", Var "O", Var "D"])
        yesMustChargeAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustChargeAfter" [Var "X", Var "Y", Var "O", Var "D"])
        noMustChargeAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustChargeAfter" [Var "X", Var "Y", Var "O", Var "D"])
        yesMayChargeAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayChargeAfter" [Var "X", Var "Y", Var "O", Var "D"])
        noMayChargeAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayChargeAfter" [Var "X", Var "Y", Var "O", Var "D"])
        yesMustRefundAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustRefundAfter" [Var "X", Var "Y", Var "O", Var "D"])
        noMustRefundAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustRefundAfter" [Var "X", Var "Y", Var "O", Var "D"])
        yesMayRefundAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayRefundAfter" [Var "X", Var "Y", Var "O", Var "D"])
        noMayRefundAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayRefundAfter" [Var "X", Var "Y", Var "O", Var "D"])

createFormulaSimpleStatementDQuanSomeThe :: Holds -> ModalVerb -> Verb -> TemporalQuantifier -> Subject -> Object -> Receiver -> Subject -> Date -> State (DateDictionary, TempQuanDictionary) FOLFormula
createFormulaSimpleStatementDQuanSomeThe holds modalVerb verb temporalQuantifier subject object receiver date dateType = do
    -- Get the actual value associated with the date
    actualValue <- lookupDate (subjectToString date)

    -- Use actualValue here
    let basePredicates = case dateType of
            DateQuanSome _ _ -> And
                            (And
                                (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                (Pred "isDate" [Var actualValue])
                            )
                            (objectToPredicate object)
            DateQuanThe _ _ -> And
                            (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                            (objectToPredicate object)

    -- Generate FOL formulas based on different cases
    generateFormulas basePredicates actualValue

    where
        -- Function to generate FOL formulas based on different cases
        generateFormulas basePredicates actualValue = 
            case (holds, modalVerb, verb, temporalQuantifier) of
                (HoldYes, ModalObli _, VDel, TempBefore) -> yesMustDelBefore
                (HoldNo, ModalObli _, VDel, TempBefore) -> noMustDelBefore
                (HoldYes, ModalPermi, VDel, TempBefore) -> yesMayDelBefore
                (HoldNo, ModalPermi, VDel, TempBefore) -> noMayDelBefore
                (HoldYes, ModalForbi, VDel, TempBefore) -> noMayDelBefore
                (HoldNo, ModalForbi, VDel, TempBefore) -> yesMayDelBefore
                (HoldYes, ModalObli _, VPay, TempBefore) -> yesMustPayBefore
                (HoldNo, ModalObli _, VPay, TempBefore) -> noMustPayBefore
                (HoldYes, ModalPermi, VPay, TempBefore) -> yesMayPayBefore
                (HoldNo, ModalPermi, VPay, TempBefore) -> noMayPayBefore
                (HoldYes, ModalForbi, VPay, TempBefore) -> noMayPayBefore
                (HoldNo, ModalForbi, VPay, TempBefore) -> yesMayPayBefore
                (HoldYes, ModalObli _, VCharge, TempBefore) -> yesMustChargeBefore
                (HoldNo, ModalObli _, VCharge, TempBefore) -> noMustChargeBefore
                (HoldYes, ModalPermi, VCharge, TempBefore) -> yesMayChargeBefore
                (HoldNo, ModalPermi, VCharge, TempBefore) -> noMayChargeBefore
                (HoldYes, ModalForbi, VCharge, TempBefore) -> noMayChargeBefore
                (HoldNo, ModalForbi, VCharge, TempBefore) -> yesMayChargeBefore
                (HoldYes, ModalObli _, VRefund, TempBefore) -> yesMustRefundBefore
                (HoldNo, ModalObli _, VRefund, TempBefore) -> noMustRefundBefore
                (HoldYes, ModalPermi, VRefund, TempBefore) -> yesMayRefundBefore
                (HoldNo, ModalPermi, VRefund, TempBefore) -> noMayRefundBefore
                (HoldYes, ModalForbi, VRefund, TempBefore) -> noMayRefundBefore
                (HoldNo, ModalForbi, VRefund, TempBefore) -> yesMayRefundBefore
                (HoldYes, ModalObli _, VDel, TempAfter) -> yesMustDelAfter 
                (HoldNo, ModalObli _, VDel, TempAfter) -> noMustDelAfter
                (HoldYes, ModalPermi, VDel, TempAfter) -> yesMayDelAfter
                (HoldNo, ModalPermi, VDel, TempAfter) -> noMayDelAfter
                (HoldYes, ModalForbi, VDel, TempAfter) -> noMayDelAfter
                (HoldNo, ModalForbi, VDel, TempAfter) -> yesMayDelAfter
                (HoldYes, ModalObli _, VPay, TempAfter) -> yesMustPayAfter
                (HoldNo, ModalObli _, VPay, TempAfter) -> noMustPayAfter 
                (HoldYes, ModalPermi, VPay, TempAfter) -> yesMayPayAfter
                (HoldNo, ModalPermi, VPay, TempAfter) -> noMayPayAfter 
                (HoldYes, ModalForbi, VPay, TempAfter) -> noMayPayAfter
                (HoldNo, ModalForbi, VPay, TempAfter) -> yesMayPayAfter
                (HoldYes, ModalObli _, VCharge, TempAfter) -> yesMustChargeAfter
                (HoldNo, ModalObli _, VCharge, TempAfter) -> noMustChargeAfter
                (HoldYes, ModalPermi, VCharge, TempAfter) -> yesMayChargeAfter
                (HoldNo, ModalPermi, VCharge, TempAfter) -> noMayChargeAfter
                (HoldYes, ModalForbi, VCharge, TempAfter) -> noMayChargeAfter
                (HoldNo, ModalForbi, VCharge, TempAfter) -> yesMayChargeAfter
                (HoldYes, ModalObli _, VRefund, TempAfter) -> yesMustRefundAfter
                (HoldNo, ModalObli _, VRefund, TempAfter) -> noMustRefundAfter
                (HoldYes, ModalPermi, VRefund, TempAfter) -> yesMayRefundAfter
                (HoldNo, ModalPermi, VRefund, TempAfter) -> noMayRefundAfter
                (HoldYes, ModalForbi, VRefund, TempAfter) -> noMayRefundAfter
                (HoldNo, ModalForbi, VRefund, TempAfter) -> yesMayRefundAfter

            where
                -- Predicates for different cases
                yesMustDelBefore = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustDeliverBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                noMustDelBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustDeliverBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMayDelBefore = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayDeliverBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                noMayDelBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayDeliverBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMustPayBefore = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustPayBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                noMustPayBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustPayBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMayPayBefore = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayPayBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                noMayPayBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayPayBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMustChargeBefore = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustChargeBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                noMustChargeBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustChargeBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMayChargeBefore = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayChargeBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                noMayChargeBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayChargeBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMustRefundBefore = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustRefundBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                noMustRefundBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustRefundBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMayRefundBefore = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayRefundBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                noMayRefundBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayRefundBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMustDelAfter = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustDeliverAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                noMustDelAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustDeliverAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMayDelAfter = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayDeliverAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                noMayDelAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayDeliverAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMustPayAfter = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustPayAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                noMustPayAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustPayAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMayPayAfter = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayPayAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                noMayPayAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayPayAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMustChargeAfter = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustChargeAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                noMustChargeAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustChargeAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMayChargeAfter = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayChargeAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                noMayChargeAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayChargeAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMustRefundAfter = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustRefundAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                noMustRefundAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustRefundAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMayRefundAfter = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayRefundAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                noMayRefundAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayRefundAfter" [Var "X", Var "Y", Var "O", Var actualValue])

createFormulaSimpleStatementDQuanTempSomeThe :: Holds -> ModalVerb -> Verb -> TemporalQuantifier -> Subject -> Object -> Receiver -> TemporalOffset -> TemporalQuantifier -> Subject -> Date -> State (DateDictionary, TempQuanDictionary) FOLFormula
createFormulaSimpleStatementDQuanTempSomeThe holds modalVerb verb temporalQuantifier subject object receiver temporalOffset tq date dateType = do
    -- Get the actual value associated with the date
    actualValue <- lookupDate (subjectToString date)

    -- Use actualValue here
    let basePredicates = case (dateType, tq) of
            (DateQuanTempSome _ _ _ _, TempBefore) -> And
                            (And
                                (And
                                    (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                    (Pred "isDate" [Var actualValue])
                                )
                                (Pred "dateBefore" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)
            (DateQuanTempSome _ _ _ _, TempAfter) -> And
                            (And
                                (And
                                    (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                    (Pred "isDate" [Var actualValue])
                                )
                                (Pred "dateAfter" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)
            (DateQuanTempThe _ _ _ _, TempBefore) -> And
                            (And
                                (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                (Pred "dateBefore" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)
            (DateQuanTempThe _ _ _ _, TempAfter) -> And
                            (And
                                (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                (Pred "dateAfter" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)

    -- Generate FOL formulas based on different cases
    generateFormulas basePredicates actualValue

    where
        -- Function to generate FOL formulas based on different cases
        generateFormulas basePredicates actualValue = 
            case (holds, modalVerb, verb, temporalQuantifier) of
                (HoldYes, ModalObli _, VDel, TempBefore) -> do
                    let formula = yesMustDelBefore
                    updateTempQuanDictionary ((connectTerms subject verb receiver object) ++ "SomeThe") (temporalQuantifierToString temporalQuantifier) (temporalOffsetToInt temporalOffset)
                    formula

                (HoldNo, ModalObli _, VDel, TempBefore) -> noMustDelBefore
                (HoldYes, ModalPermi, VDel, TempBefore) -> yesMayDelBefore
                (HoldNo, ModalPermi, VDel, TempBefore) -> noMayDelBefore
                (HoldYes, ModalForbi, VDel, TempBefore) -> noMayDelBefore
                (HoldNo, ModalForbi, VDel, TempBefore) -> yesMayDelBefore

                (HoldYes, ModalObli _, VPay, TempBefore) -> do
                    let formula = yesMustPayBefore
                    updateTempQuanDictionary ((connectTerms subject verb receiver object) ++ "SomeThe") (temporalQuantifierToString temporalQuantifier) (temporalOffsetToInt temporalOffset)
                    formula

                (HoldNo, ModalObli _, VPay, TempBefore) -> noMustPayBefore
                (HoldYes, ModalPermi, VPay, TempBefore) -> yesMayPayBefore
                (HoldNo, ModalPermi, VPay, TempBefore) -> noMayPayBefore
                (HoldYes, ModalForbi, VPay, TempBefore) -> noMayPayBefore
                (HoldNo, ModalForbi, VPay, TempBefore) -> yesMayPayBefore

                (HoldYes, ModalObli _, VCharge, TempBefore) -> do
                    let formula = yesMustChargeBefore
                    updateTempQuanDictionary ((connectTerms subject verb receiver object) ++ "SomeThe") (temporalQuantifierToString temporalQuantifier) (temporalOffsetToInt temporalOffset)
                    formula

                (HoldNo, ModalObli _, VCharge, TempBefore) -> noMustChargeBefore
                (HoldYes, ModalPermi, VCharge, TempBefore) -> yesMayChargeBefore
                (HoldNo, ModalPermi, VCharge, TempBefore) -> noMayChargeBefore
                (HoldYes, ModalForbi, VCharge, TempBefore) -> noMayChargeBefore
                (HoldNo, ModalForbi, VCharge, TempBefore) -> yesMayChargeBefore

                (HoldYes, ModalObli _, VRefund, TempBefore) -> do
                    let formula = yesMustRefundBefore
                    updateTempQuanDictionary ((connectTerms subject verb receiver object) ++ "SomeThe") (temporalQuantifierToString temporalQuantifier) (temporalOffsetToInt temporalOffset)
                    formula

                (HoldNo, ModalObli _, VRefund, TempBefore) -> noMustRefundBefore
                (HoldYes, ModalPermi, VRefund, TempBefore) -> yesMayRefundBefore
                (HoldNo, ModalPermi, VRefund, TempBefore) -> noMayRefundBefore
                (HoldYes, ModalForbi, VRefund, TempBefore) -> noMayRefundBefore
                (HoldNo, ModalForbi, VRefund, TempBefore) -> yesMayRefundBefore

                (HoldYes, ModalObli _, VDel, TempAfter) -> do 
                    let formula = yesMustDelAfter
                    updateTempQuanDictionary ((connectTerms subject verb receiver object) ++ "SomeThe") (temporalQuantifierToString temporalQuantifier) (temporalOffsetToInt temporalOffset)
                    formula

                (HoldNo, ModalObli _, VDel, TempAfter) -> noMustDelAfter
                (HoldYes, ModalPermi, VDel, TempAfter) -> yesMayDelAfter
                (HoldNo, ModalPermi, VDel, TempAfter) -> noMayDelAfter
                (HoldYes, ModalForbi, VDel, TempAfter) -> noMayDelAfter
                (HoldNo, ModalForbi, VDel, TempAfter) -> yesMayDelAfter

                (HoldYes, ModalObli _, VPay, TempAfter) -> do
                    let formula = yesMustPayAfter
                    updateTempQuanDictionary ((connectTerms subject verb receiver object) ++ "SomeThe") (temporalQuantifierToString temporalQuantifier) (temporalOffsetToInt temporalOffset)
                    formula

                (HoldNo, ModalObli _, VPay, TempAfter) -> noMustPayAfter 
                (HoldYes, ModalPermi, VPay, TempAfter) -> yesMayPayAfter
                (HoldNo, ModalPermi, VPay, TempAfter) -> noMayPayAfter 
                (HoldYes, ModalForbi, VPay, TempAfter) -> noMayPayAfter
                (HoldNo, ModalForbi, VPay, TempAfter) -> yesMayPayAfter

                (HoldYes, ModalObli _, VCharge, TempAfter) -> do
                    let formula = yesMustChargeAfter
                    updateTempQuanDictionary ((connectTerms subject verb receiver object) ++ "SomeThe") (temporalQuantifierToString temporalQuantifier) (temporalOffsetToInt temporalOffset)
                    formula

                (HoldNo, ModalObli _, VCharge, TempAfter) -> noMustChargeAfter
                (HoldYes, ModalPermi, VCharge, TempAfter) -> yesMayChargeAfter
                (HoldNo, ModalPermi, VCharge, TempAfter) -> noMayChargeAfter
                (HoldYes, ModalForbi, VCharge, TempAfter) -> noMayChargeAfter
                (HoldNo, ModalForbi, VCharge, TempAfter) -> yesMayChargeAfter

                (HoldYes, ModalObli _, VRefund, TempAfter) -> do
                    let formula = yesMustRefundAfter
                    updateTempQuanDictionary ((connectTerms subject verb receiver object) ++ "SomeThe") (temporalQuantifierToString temporalQuantifier) (temporalOffsetToInt temporalOffset)
                    formula

                (HoldNo, ModalObli _, VRefund, TempAfter) -> noMustRefundAfter
                (HoldYes, ModalPermi, VRefund, TempAfter) -> yesMayRefundAfter
                (HoldNo, ModalPermi, VRefund, TempAfter) -> noMayRefundAfter
                (HoldYes, ModalForbi, VRefund, TempAfter) -> noMayRefundAfter
                (HoldNo, ModalForbi, VRefund, TempAfter) -> yesMayRefundAfter

            where
                -- Predicates for different cases
                yesMustDelBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustDeliverBefore" [Var "X", Var "Y", Var "O", Var "D"])
                noMustDelBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustDeliverBefore" [Var "X", Var "Y", Var "O", Var "D"])
                yesMayDelBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayDeliverBefore" [Var "X", Var "Y", Var "O", Var "D"])
                noMayDelBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayDeliverBefore" [Var "X", Var "Y", Var "O", Var "D"])
                yesMustPayBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustPayBefore" [Var "X", Var "Y", Var "O", Var "D"])
                noMustPayBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustPayBefore" [Var "X", Var "Y", Var "O", Var "D"])
                yesMayPayBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayPayBefore" [Var "X", Var "Y", Var "O", Var "D"])
                noMayPayBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayPayBefore" [Var "X", Var "Y", Var "O", Var "D"])
                yesMustChargeBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustChargeBefore" [Var "X", Var "Y", Var "O", Var "D"])
                noMustChargeBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustChargeBefore" [Var "X", Var "Y", Var "O", Var "D"])
                yesMayChargeBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayChargeBefore" [Var "X", Var "Y", Var "O", Var "D"])
                noMayChargeBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayChargeBefore" [Var "X", Var "Y", Var "O", Var "D"])
                yesMustRefundBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustRefundBefore" [Var "X", Var "Y", Var "O", Var "D"])
                noMustRefundBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustRefundBefore" [Var "X", Var "Y", Var "O", Var "D"])
                yesMayRefundBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayRefundBefore" [Var "X", Var "Y", Var "O", Var "D"])
                noMayRefundBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayRefundBefore" [Var "X", Var "Y", Var "O", Var "D"])
                yesMustDelAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustDeliverAfter" [Var "X", Var "Y", Var "O", Var "D"])
                noMustDelAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustDeliverAfter" [Var "X", Var "Y", Var "O", Var "D"])
                yesMayDelAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayDeliverAfter" [Var "X", Var "Y", Var "O", Var "D"])
                noMayDelAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayDeliverAfter" [Var "X", Var "Y", Var "O", Var "D"])
                yesMustPayAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustPayAfter" [Var "X", Var "Y", Var "O", Var "D"])
                noMustPayAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustPayAfter" [Var "X", Var "Y", Var "O", Var "D"])
                yesMayPayAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayPayAfter" [Var "X", Var "Y", Var "O", Var "D"])
                noMayPayAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayPayAfter" [Var "X", Var "Y", Var "O", Var "D"])
                yesMustChargeAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustChargeAfter" [Var "X", Var "Y", Var "O", Var "D"])
                noMustChargeAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustChargeAfter" [Var "X", Var "Y", Var "O", Var "D"])
                yesMayChargeAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayChargeAfter" [Var "X", Var "Y", Var "O", Var "D"])
                noMayChargeAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayChargeAfter" [Var "X", Var "Y", Var "O", Var "D"])
                yesMustRefundAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustRefundAfter" [Var "X", Var "Y", Var "O", Var "D"])
                noMustRefundAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustRefundAfter" [Var "X", Var "Y", Var "O", Var "D"])
                yesMayRefundAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayRefundAfter" [Var "X", Var "Y", Var "O", Var "D"])
                noMayRefundAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayRefundAfter" [Var "X", Var "Y", Var "O", Var "D"])

createFormulaSimpleStatementDQuanSomeTheWO :: Holds -> ModalVerb -> Verb -> Subject -> Object -> Receiver -> TemporalOffset -> TemporalQuantifier -> Subject -> Date -> State (DateDictionary, TempQuanDictionary) FOLFormula
createFormulaSimpleStatementDQuanSomeTheWO holds modalVerb verb subject object receiver temporalOffset tq date dateType = do
    -- Get the actual value associated with the date
    actualValue <- lookupDate (subjectToString date)

    -- Use actualValue here
    let basePredicates = case (dateType, tq) of
            (DateQuanSomeWO _ _ _, TempBefore) -> And
                            (And
                                (And
                                    (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                    (Pred "isDate" [Var actualValue])
                                )
                                (Pred "dateBefore" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)
            (DateQuanSomeWO _ _ _, TempAfter) -> And
                            (And
                                (And
                                    (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                    (Pred "isDate" [Var actualValue])
                                )
                                (Pred "dateAfter" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)
            (DateQuanTheWO _ _ _, TempBefore) -> And
                            (And
                                (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                (Pred "dateBefore" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)
            (DateQuanTheWO _ _ _, TempAfter) -> And
                            (And
                                (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                (Pred "dateAfter" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)

    -- Generate FOL formulas based on different cases
    generateFormulas basePredicates actualValue

    where
        -- Function to generate FOL formulas based on different cases
        generateFormulas basePredicates actualValue = 
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
                -- Predicates for different cases
                yesMustDel = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustDeliver" [Var "X", Var "Y", Var "O", Var "D"])
                noMustDel = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustDeliver" [Var "X", Var "Y", Var "O", Var "D"])
                yesMayDel = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayDeliver" [Var "X", Var "Y", Var "O", Var "D"])
                noMayDel = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayDeliver" [Var "X", Var "Y", Var "O", Var "D"])
                yesMustPay = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustPay" [Var "X", Var "Y", Var "O", Var "D"])
                noMustPay = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustPay" [Var "X", Var "Y", Var "O", Var "D"])
                yesMayPay = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayPay" [Var "X", Var "Y", Var "O", Var "D"])
                noMayPay = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayPay" [Var "X", Var "Y", Var "O", Var "D"])
                yesMustCharge = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustCharge" [Var "X", Var "Y", Var "O", Var "D"])
                noMustCharge = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustCharge" [Var "X", Var "Y", Var "O", Var "D"])
                yesMayCharge = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayCharge" [Var "X", Var "Y", Var "O", Var "D"])
                noMayCharge = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayCharge" [Var "X", Var "Y", Var "O", Var "D"])
                yesMustRefund = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustRefund" [Var "X", Var "Y", Var "O", Var "D"])
                noMustRefund = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustRefund" [Var "X", Var "Y", Var "O", Var "D"])
                yesMayRefund = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayRefund" [Var "X", Var "Y", Var "O", Var "D"])
                noMayRefund = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayRefund" [Var "X", Var "Y", Var "O", Var "D"])

createFormulaSimpleStatementNH :: ModalVerb -> Verb -> Subject -> Object -> Receiver -> Num -> Month -> Num -> State (DateDictionary, TempQuanDictionary) FOLFormula
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
                    (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                    (Pred "date" [Var "D", dateSpeToTerm year month day])
                )
                (objectToPredicate object)
                
        -- Predicates for different cases
        yesMustDel = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustDeliver" [Var "X", Var "Y", Var "O", Var "D"])
        yesMayDel = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayDeliver" [Var "X", Var "Y", Var "O", Var "D"])
        noMayDel = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayDeliver" [Var "X", Var "Y", Var "O", Var "D"])
        yesMustPay = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustPay" [Var "X", Var "Y", Var "O", Var "D"])
        yesMayPay = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayPay" [Var "X", Var "Y", Var "O", Var "D"])
        noMayPay = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayPay" [Var "X", Var "Y", Var "O", Var "D"])
        yesMustCharge = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustCharge" [Var "X", Var "Y", Var "O", Var "D"])
        yesMayCharge = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayCharge" [Var "X", Var "Y", Var "O", Var "D"])
        noMayCharge = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayCharge" [Var "X", Var "Y", Var "O", Var "D"])
        yesMustRefund = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustRefund" [Var "X", Var "Y", Var "O", Var "D"])
        yesMayRefund = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayRefund" [Var "X", Var "Y", Var "O", Var "D"])
        noMayRefund = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayRefund" [Var "X", Var "Y", Var "O", Var "D"])

createFormulaSimpleStatementNHDAny :: ModalVerb -> Verb -> Subject -> Object -> Receiver -> State (DateDictionary, TempQuanDictionary) FOLFormula
createFormulaSimpleStatementNHDAny modalVerb verb subject object receiver =
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
                (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                (objectToPredicate object)
                
        -- Predicates for different cases
        yesMustDel = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "mustDeliver" [Var "X", Var "Y", Var "O", Var "D"])
        yesMayDel = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "mayDeliver" [Var "X", Var "Y", Var "O", Var "D"])
        noMayDel = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "mayDeliver" [Var "X", Var "Y", Var "O", Var "D"])
        yesMustPay = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "mustPay" [Var "X", Var "Y", Var "O", Var "D"])
        yesMayPay = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "mayPay" [Var "X", Var "Y", Var "O", Var "D"])
        noMayPay = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "mayPay" [Var "X", Var "Y", Var "O", Var "D"])
        yesMustCharge = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "mustCharge" [Var "X", Var "Y", Var "O", Var "D"])
        yesMayCharge = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "mayCharge" [Var "X", Var "Y", Var "O", Var "D"])
        noMayCharge = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "mayCharge" [Var "X", Var "Y", Var "O", Var "D"])
        yesMustRefund = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "mustRefund" [Var "X", Var "Y", Var "O", Var "D"])
        yesMayRefund = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "mayRefund" [Var "X", Var "Y", Var "O", Var "D"])
        noMayRefund = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "mayRefund" [Var "X", Var "Y", Var "O", Var "D"])

createFormulaSimpleStatementNHDSomeThe :: ModalVerb -> Verb -> Subject -> Object -> Receiver -> Subject -> Date -> State (DateDictionary, TempQuanDictionary) FOLFormula
createFormulaSimpleStatementNHDSomeThe modalVerb verb subject object receiver date dateType = do
    -- Get the actual value associated with the date
    actualValue <- lookupDate (subjectToString date)

    -- Use actualValue here
    let basePredicates = case dateType of
            DateSome _ -> And
                            (And
                                (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                (Pred "isDate" [Var actualValue])
                            )
                            (objectToPredicate object)
            DateThe _ -> And
                            (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                            (objectToPredicate object)

    -- Generate FOL formulas based on different cases
    generateFormulas basePredicates actualValue

    where
        -- Function to generate FOL formulas based on different cases
        generateFormulas basePredicates actualValue = 
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
                -- Predicates for different cases
                yesMustDel = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustDeliver" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMayDel = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayDeliver" [Var "X", Var "Y", Var "O", Var actualValue])
                noMayDel = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayDeliver" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMustPay = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustPay" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMayPay = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayPay" [Var "X", Var "Y", Var "O", Var actualValue])
                noMayPay = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayPay" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMustCharge = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustCharge" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMayCharge = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayCharge" [Var "X", Var "Y", Var "O", Var actualValue])
                noMayCharge = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayCharge" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMustRefund = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustRefund" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMayRefund = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayRefund" [Var "X", Var "Y", Var "O", Var actualValue])
                noMayRefund = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayRefund" [Var "X", Var "Y", Var "O", Var actualValue])

createFormulaSimpleStatementNHDQuanSpe :: ModalVerb -> Verb -> TemporalQuantifier -> Subject -> Object -> Receiver -> Num -> Month -> Num -> State (DateDictionary, TempQuanDictionary) FOLFormula
createFormulaSimpleStatementNHDQuanSpe modalVerb verb temporalQuantifier subject object receiver day month year =
    case (modalVerb, verb, temporalQuantifier) of
        (ModalObli _, VDel, TempBefore) -> do
            let formula = yesMustDelBefore
            updateTempQuanDictionary (connectTerms subject verb receiver object) (temporalQuantifierToString temporalQuantifier) (dateSpeToInt year month day)
            formula

        (ModalPermi, VDel, TempBefore) -> yesMayDelBefore
        (ModalForbi, VDel, TempBefore) -> noMayDelBefore

        (ModalObli _, VPay, TempBefore) -> do
            let formula = yesMustPayBefore
            updateTempQuanDictionary (connectTerms subject verb receiver object) (temporalQuantifierToString temporalQuantifier) (dateSpeToInt year month day)
            formula

        (ModalPermi, VPay, TempBefore) -> yesMayPayBefore
        (ModalForbi, VPay, TempBefore) -> noMayPayBefore

        (ModalObli _, VCharge, TempBefore) -> do
            let formula = yesMustChargeBefore
            updateTempQuanDictionary (connectTerms subject verb receiver object) (temporalQuantifierToString temporalQuantifier) (dateSpeToInt year month day)
            formula

        (ModalPermi, VCharge, TempBefore) -> yesMayChargeBefore
        (ModalForbi, VCharge, TempBefore) -> noMayChargeBefore

        (ModalObli _, VRefund, TempBefore) -> do
            let formula = yesMustRefundBefore
            updateTempQuanDictionary (connectTerms subject verb receiver object) (temporalQuantifierToString temporalQuantifier) (dateSpeToInt year month day)
            formula

        (ModalPermi, VRefund, TempBefore) -> yesMayRefundBefore
        (ModalForbi, VRefund, TempBefore) -> noMayRefundBefore

        (ModalObli _, VDel, TempAfter) -> do
            let formula = yesMustDelAfter
            updateTempQuanDictionary (connectTerms subject verb receiver object) (temporalQuantifierToString temporalQuantifier) (dateSpeToInt year month day)
            formula 

        (ModalPermi, VDel, TempAfter) -> yesMayDelAfter
        (ModalForbi, VDel, TempAfter) -> noMayDelAfter

        (ModalObli _, VPay, TempAfter) -> do
            let formula = yesMustPayAfter
            updateTempQuanDictionary (connectTerms subject verb receiver object) (temporalQuantifierToString temporalQuantifier) (dateSpeToInt year month day)
            formula 

        (ModalPermi, VPay, TempAfter) -> yesMayPayAfter
        (ModalForbi, VPay, TempAfter) -> noMayPayAfter

        (ModalObli _, VCharge, TempAfter) -> do
            let formula = yesMustChargeAfter
            updateTempQuanDictionary (connectTerms subject verb receiver object) (temporalQuantifierToString temporalQuantifier) (dateSpeToInt year month day)
            formula 

        (ModalPermi, VCharge, TempAfter) -> yesMayChargeAfter
        (ModalForbi, VCharge, TempAfter) -> noMayChargeAfter

        (ModalObli _, VRefund, TempAfter) -> do
            let formula = yesMustRefundAfter
            updateTempQuanDictionary (connectTerms subject verb receiver object) (temporalQuantifierToString temporalQuantifier) (dateSpeToInt year month day)
            formula 

        (ModalPermi, VRefund, TempAfter) -> yesMayRefundAfter
        (ModalForbi, VRefund, TempAfter) -> noMayRefundAfter

    where
        -- Common base predicates
        basePredicates = 
            And
                (And
                    (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                    (Pred "date" [Var "D", dateSpeToTerm year month day])
                )
                (objectToPredicate object)
                
        -- Predicates for different cases
        yesMustDelBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustDeliverBefore" [Var "X", Var "Y", Var "O", Var "D"])
        yesMayDelBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayDeliverBefore" [Var "X", Var "Y", Var "O", Var "D"])
        noMayDelBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayDeliverBefore" [Var "X", Var "Y", Var "O", Var "D"])
        yesMustPayBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustPayBefore" [Var "X", Var "Y", Var "O", Var "D"])
        yesMayPayBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayPayBefore" [Var "X", Var "Y", Var "O", Var "D"])
        noMayPayBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayPayBefore" [Var "X", Var "Y", Var "O", Var "D"])
        yesMustChargeBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustChargeBefore" [Var "X", Var "Y", Var "O", Var "D"])
        yesMayChargeBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayChargeBefore" [Var "X", Var "Y", Var "O", Var "D"])
        noMayChargeBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayChargeBefore" [Var "X", Var "Y", Var "O", Var "D"])
        yesMustRefundBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustRefundBefore" [Var "X", Var "Y", Var "O", Var "D"])
        yesMayRefundBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayRefundBefore" [Var "X", Var "Y", Var "O", Var "D"])
        noMayRefundBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayRefundBefore" [Var "X", Var "Y", Var "O", Var "D"])
        yesMustDelAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustDeliverAfter" [Var "X", Var "Y", Var "O", Var "D"])
        yesMayDelAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayDeliverAfter" [Var "X", Var "Y", Var "O", Var "D"])
        noMayDelAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayDeliverAfter" [Var "X", Var "Y", Var "O", Var "D"])
        yesMustPayAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustPayAfter" [Var "X", Var "Y", Var "O", Var "D"])
        yesMayPayAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayPayAfter" [Var "X", Var "Y", Var "O", Var "D"])
        noMayPayAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayPayAfter" [Var "X", Var "Y", Var "O", Var "D"])
        yesMustChargeAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustChargeAfter" [Var "X", Var "Y", Var "O", Var "D"])
        yesMayChargeAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayChargeAfter" [Var "X", Var "Y", Var "O", Var "D"])
        noMayChargeAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayChargeAfter" [Var "X", Var "Y", Var "O", Var "D"])
        yesMustRefundAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustRefundAfter" [Var "X", Var "Y", Var "O", Var "D"])
        yesMayRefundAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayRefundAfter" [Var "X", Var "Y", Var "O", Var "D"])
        noMayRefundAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayRefundAfter" [Var "X", Var "Y", Var "O", Var "D"])

createFormulaSimpleStatementNHDQuanSomeThe :: ModalVerb -> Verb -> TemporalQuantifier -> Subject -> Object -> Receiver -> Subject -> Date -> State (DateDictionary, TempQuanDictionary) FOLFormula
createFormulaSimpleStatementNHDQuanSomeThe modalVerb verb temporalQuantifier subject object receiver date dateType = do
    -- Get the actual value associated with the date
    actualValue <- lookupDate (subjectToString date)

    -- Use actualValue here
    let basePredicates = case dateType of
            DateQuanSome _ _ -> And
                            (And
                                (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                (Pred "isDate" [Var actualValue])
                            )
                            (objectToPredicate object)
            DateQuanThe _ _ -> And
                            (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                            (objectToPredicate object)

    -- Generate FOL formulas based on different cases
    generateFormulas basePredicates actualValue 

    where
        -- Function to generate FOL formulas based on different cases
        generateFormulas basePredicates actualValue = 
            case (modalVerb, verb, temporalQuantifier) of
                (ModalObli _, VDel, TempBefore) -> yesMustDelBefore
                (ModalPermi, VDel, TempBefore) -> yesMayDelBefore
                (ModalForbi, VDel, TempBefore) -> noMayDelBefore
                (ModalObli _, VPay, TempBefore) -> yesMustPayBefore
                (ModalPermi, VPay, TempBefore) -> yesMayPayBefore
                (ModalForbi, VPay, TempBefore) -> noMayPayBefore
                (ModalObli _, VCharge, TempBefore) -> yesMustChargeBefore
                (ModalPermi, VCharge, TempBefore) -> yesMayChargeBefore
                (ModalForbi, VCharge, TempBefore) -> noMayChargeBefore
                (ModalObli _, VRefund, TempBefore) -> yesMustRefundBefore
                (ModalPermi, VRefund, TempBefore) -> yesMayRefundBefore
                (ModalForbi, VRefund, TempBefore) -> noMayRefundBefore
                (ModalObli _, VDel, TempAfter) -> yesMustDelAfter
                (ModalPermi, VDel, TempAfter) -> yesMayDelAfter
                (ModalForbi, VDel, TempAfter) -> noMayDelAfter
                (ModalObli _, VPay, TempAfter) -> yesMustPayAfter
                (ModalPermi, VPay, TempAfter) -> yesMayPayAfter
                (ModalForbi, VPay, TempAfter) -> noMayPayAfter
                (ModalObli _, VCharge, TempAfter) -> yesMustChargeAfter
                (ModalPermi, VCharge, TempAfter) -> yesMayChargeAfter
                (ModalForbi, VCharge, TempAfter) -> noMayChargeAfter
                (ModalObli _, VRefund, TempAfter) -> yesMustRefundAfter
                (ModalPermi, VRefund, TempAfter) -> yesMayRefundAfter
                (ModalForbi, VRefund, TempAfter) -> noMayRefundAfter

            where
                -- Predicates for different cases
                yesMustDelBefore = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustDeliverBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMayDelBefore = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayDeliverBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                noMayDelBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayDeliverBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMustPayBefore = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustPayBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMayPayBefore = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayPayBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                noMayPayBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayPayBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMustChargeBefore = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustChargeBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMayChargeBefore = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayChargeBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                noMayChargeBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayChargeBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMustRefundBefore = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustRefundBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMayRefundBefore = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayRefundBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                noMayRefundBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayRefundBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMustDelAfter = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustDeliverAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMayDelAfter = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayDeliverAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                noMayDelAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayDeliverAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMustPayAfter = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustPayAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMayPayAfter = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayPayAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                noMayPayAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayPayAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMustChargeAfter = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustChargeAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMayChargeAfter = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayChargeAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                noMayChargeAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayChargeAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMustRefundAfter = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mustRefundAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                yesMayRefundAfter = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayRefundAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                noMayRefundAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "mayRefundAfter" [Var "X", Var "Y", Var "O", Var actualValue])

createFormulaSimpleStatementNHDQuanTempSomeThe :: ModalVerb -> Verb -> TemporalQuantifier -> Subject -> Object -> Receiver -> TemporalOffset -> TemporalQuantifier -> Subject -> Date -> State (DateDictionary, TempQuanDictionary) FOLFormula
createFormulaSimpleStatementNHDQuanTempSomeThe modalVerb verb temporalQuantifier subject object receiver temporalOffset tq date dateType = do
    -- Get the actual value associated with the date
    actualValue <- lookupDate (subjectToString date)

    -- Use actualValue here
    let basePredicates = case (dateType, tq) of
            (DateQuanTempSome _ _ _ _, TempBefore) -> And
                            (And
                                (And
                                    (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                    (Pred "isDate" [Var actualValue])
                                )
                                (Pred "dateBefore" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)
            (DateQuanTempSome _ _ _ _, TempAfter) -> And
                            (And
                                (And
                                    (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                    (Pred "isDate" [Var actualValue])
                                )
                                (Pred "dateAfter" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)
            (DateQuanTempThe _ _ _ _, TempBefore) -> And
                            (And
                                (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                (Pred "dateBefore" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)
            (DateQuanTempThe _ _ _ _, TempAfter) -> And
                            (And
                                (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                (Pred "dateAfter" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)

    -- Generate FOL formulas based on different cases
    generateFormulas basePredicates actualValue

    where
        -- Function to generate FOL formulas based on different cases
        generateFormulas basePredicates actualValue = 
            case (modalVerb, verb, temporalQuantifier) of
                (ModalObli _, VDel, TempBefore) -> do
                    let formula = yesMustDelBefore
                    updateTempQuanDictionary ((connectTerms subject verb receiver object) ++ "SomeThe") (temporalQuantifierToString temporalQuantifier) (temporalOffsetToInt temporalOffset)
                    formula

                (ModalPermi, VDel, TempBefore) -> yesMayDelBefore
                (ModalForbi, VDel, TempBefore) -> noMayDelBefore

                (ModalObli _, VPay, TempBefore) -> do
                    let formula = yesMustPayBefore
                    updateTempQuanDictionary ((connectTerms subject verb receiver object) ++ "SomeThe") (temporalQuantifierToString temporalQuantifier) (temporalOffsetToInt temporalOffset)
                    formula

                (ModalPermi, VPay, TempBefore) -> yesMayPayBefore
                (ModalForbi, VPay, TempBefore) -> noMayPayBefore
                
                (ModalObli _, VCharge, TempBefore) -> do
                    let formula = yesMustChargeBefore
                    updateTempQuanDictionary ((connectTerms subject verb receiver object) ++ "SomeThe") (temporalQuantifierToString temporalQuantifier) (temporalOffsetToInt temporalOffset)
                    formula

                (ModalPermi, VCharge, TempBefore) -> yesMayChargeBefore
                (ModalForbi, VCharge, TempBefore) -> noMayChargeBefore

                (ModalObli _, VRefund, TempBefore) -> do
                    let formula = yesMustRefundBefore
                    updateTempQuanDictionary ((connectTerms subject verb receiver object) ++ "SomeThe") (temporalQuantifierToString temporalQuantifier) (temporalOffsetToInt temporalOffset)
                    formula

                (ModalPermi, VRefund, TempBefore) -> yesMayRefundBefore
                (ModalForbi, VRefund, TempBefore) -> noMayRefundBefore

                (ModalObli _, VDel, TempAfter) -> do 
                    let formula = yesMustDelAfter
                    updateTempQuanDictionary ((connectTerms subject verb receiver object) ++ "SomeThe") (temporalQuantifierToString temporalQuantifier) (temporalOffsetToInt temporalOffset)
                    formula

                (ModalPermi, VDel, TempAfter) -> yesMayDelAfter
                (ModalForbi, VDel, TempAfter) -> noMayDelAfter

                (ModalObli _, VPay, TempAfter) -> do 
                    let formula = yesMustPayAfter
                    updateTempQuanDictionary ((connectTerms subject verb receiver object) ++ "SomeThe") (temporalQuantifierToString temporalQuantifier) (temporalOffsetToInt temporalOffset)
                    formula

                (ModalPermi, VPay, TempAfter) -> yesMayPayAfter
                (ModalForbi, VPay, TempAfter) -> noMayPayAfter

                (ModalObli _, VCharge, TempAfter) -> do
                    let formula = yesMustChargeAfter
                    updateTempQuanDictionary ((connectTerms subject verb receiver object) ++ "SomeThe") (temporalQuantifierToString temporalQuantifier) (temporalOffsetToInt temporalOffset)
                    formula

                (ModalPermi, VCharge, TempAfter) -> yesMayChargeAfter
                (ModalForbi, VCharge, TempAfter) -> noMayChargeAfter

                (ModalObli _, VRefund, TempAfter) -> do
                    let formula = yesMustRefundAfter
                    updateTempQuanDictionary ((connectTerms subject verb receiver object) ++ "SomeThe") (temporalQuantifierToString temporalQuantifier) (temporalOffsetToInt temporalOffset)
                    formula

                (ModalPermi, VRefund, TempAfter) -> yesMayRefundAfter
                (ModalForbi, VRefund, TempAfter) -> noMayRefundAfter

            where
                -- Predicates for different cases
                yesMustDelBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustDeliverBefore" [Var "X", Var "Y", Var "O", Var "D"])
                yesMayDelBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayDeliverBefore" [Var "X", Var "Y", Var "O", Var "D"])
                noMayDelBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayDeliverBefore" [Var "X", Var "Y", Var "O", Var "D"])
                yesMustPayBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustPayBefore" [Var "X", Var "Y", Var "O", Var "D"])
                yesMayPayBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayPayBefore" [Var "X", Var "Y", Var "O", Var "D"])
                noMayPayBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayPayBefore" [Var "X", Var "Y", Var "O", Var "D"])
                yesMustChargeBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustChargeBefore" [Var "X", Var "Y", Var "O", Var "D"])
                yesMayChargeBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayChargeBefore" [Var "X", Var "Y", Var "O", Var "D"])
                noMayChargeBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayChargeBefore" [Var "X", Var "Y", Var "O", Var "D"])
                yesMustRefundBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustRefundBefore" [Var "X", Var "Y", Var "O", Var "D"])
                yesMayRefundBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayRefundBefore" [Var "X", Var "Y", Var "O", Var "D"])
                noMayRefundBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayRefundBefore" [Var "X", Var "Y", Var "O", Var "D"])
                yesMustDelAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustDeliverAfter" [Var "X", Var "Y", Var "O", Var "D"])
                yesMayDelAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayDeliverAfter" [Var "X", Var "Y", Var "O", Var "D"])
                noMayDelAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayDeliverAfter" [Var "X", Var "Y", Var "O", Var "D"])
                yesMustPayAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustPayAfter" [Var "X", Var "Y", Var "O", Var "D"])
                yesMayPayAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayPayAfter" [Var "X", Var "Y", Var "O", Var "D"])
                noMayPayAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayPayAfter" [Var "X", Var "Y", Var "O", Var "D"])
                yesMustChargeAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustChargeAfter" [Var "X", Var "Y", Var "O", Var "D"])
                yesMayChargeAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayChargeAfter" [Var "X", Var "Y", Var "O", Var "D"])
                noMayChargeAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayChargeAfter" [Var "X", Var "Y", Var "O", Var "D"])
                yesMustRefundAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustRefundAfter" [Var "X", Var "Y", Var "O", Var "D"])
                yesMayRefundAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayRefundAfter" [Var "X", Var "Y", Var "O", Var "D"])
                noMayRefundAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayRefundAfter" [Var "X", Var "Y", Var "O", Var "D"])

createFormulaSimpleStatementNHDQuanSomeTheWO :: ModalVerb -> Verb -> Subject -> Object -> Receiver -> TemporalOffset -> TemporalQuantifier -> Subject -> Date -> State (DateDictionary, TempQuanDictionary) FOLFormula
createFormulaSimpleStatementNHDQuanSomeTheWO modalVerb verb subject object receiver temporalOffset tq date dateType = do
    -- Get the actual value associated with the date
    actualValue <- lookupDate (subjectToString date)

    -- Use actualValue here
    let basePredicates = case (dateType, tq) of
            (DateQuanSomeWO _ _ _, TempBefore) -> And
                            (And
                                (And
                                    (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                    (Pred "isDate" [Var actualValue])
                                )
                                (Pred "dateBefore" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)
            (DateQuanSomeWO _ _ _, TempAfter) -> And
                            (And
                                (And
                                    (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                    (Pred "isDate" [Var actualValue])
                                )
                                (Pred "dateAfter" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)
            (DateQuanTheWO _ _ _, TempBefore) -> And
                            (And
                                (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                (Pred "dateBefore" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)
            (DateQuanTheWO _ _ _, TempAfter) -> And
                            (And
                                (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                (Pred "dateAfter" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)

    -- Generate FOL formulas based on different cases
    generateFormulas basePredicates actualValue

    where
        -- Function to generate FOL formulas based on different cases
        generateFormulas basePredicates actualValue = 
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
                -- Predicates for different cases
                yesMustDel = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustDeliver" [Var "X", Var "Y", Var "O", Var "D"])
                yesMayDel = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayDeliver" [Var "X", Var "Y", Var "O", Var "D"])
                noMayDel = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayDeliver" [Var "X", Var "Y", Var "O", Var "D"])
                yesMustPay = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustPay" [Var "X", Var "Y", Var "O", Var "D"])
                yesMayPay = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayPay" [Var "X", Var "Y", Var "O", Var "D"])
                noMayPay = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayPay" [Var "X", Var "Y", Var "O", Var "D"])
                yesMustCharge = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustCharge" [Var "X", Var "Y", Var "O", Var "D"])
                yesMayCharge = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayCharge" [Var "X", Var "Y", Var "O", Var "D"])
                noMayCharge = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayCharge" [Var "X", Var "Y", Var "O", Var "D"])
                yesMustRefund = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mustRefund" [Var "X", Var "Y", Var "O", Var "D"])
                yesMayRefund = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayRefund" [Var "X", Var "Y", Var "O", Var "D"])
                noMayRefund = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "mayRefund" [Var "X", Var "Y", Var "O", Var "D"])

-- Convert different kinds of objects to predicates
objectToPredicate :: Object -> FOLFormula
objectToPredicate (ObjNu (NumPound _ (NumInt num))) = Pred "objectPound" [Var "O", Var (show num)]
objectToPredicate (ObjNu (NumDol _ (NumInt num))) = Pred "objectDollar" [Var "O", Var (show num)]
objectToPredicate (ObjNu (NumEur _ (NumInt num))) = Pred "objectEuro" [Var "O", Var (show num)]
objectToPredicate (ObjNu (NumAmount subject)) = Pred "objectAmount" [Var "O", subjectToTerm subject]
objectToPredicate (ObjNonNu (NonNumCurr subject)) = Pred "objectSomeCurrency" [Var "O", subjectToTerm subject]
objectToPredicate (ObjNonNu (NonNumRep subject)) = Pred "objectReport" [Var "O", subjectToTerm subject]
objectToPredicate (ObjNonNu (NonNumNamed subject)) = Pred "objectNamedObject" [Var "O", subjectToTerm subject]
objectToPredicate (ObjNonNu (NonNumOther subject)) = Pred "objectOtherObject" [Var "O", subjectToTerm subject]

objectToString :: Object -> String
objectToString (ObjNu (NumPound _ (NumInt num))) = "ObjectPound" ++ show num
objectToString (ObjNu (NumDol _ (NumInt num))) = "ObjectDollar" ++ show num
objectToString (ObjNu (NumEur _ (NumInt num))) = "ObjectEuro" ++ show num
objectToString (ObjNu (NumAmount subject)) = "ObjectAmount" ++ subjectToString subject
objectToString (ObjNonNu (NonNumCurr subject)) = "ObjectSomeCurrency" ++ subjectToString subject
objectToString (ObjNonNu (NonNumRep subject)) = "ObjectReport" ++ subjectToString subject
objectToString (ObjNonNu (NonNumNamed subject)) = "ObjectNamedObject" ++ subjectToString subject
objectToString (ObjNonNu (NonNumOther subject)) = "ObjectOtherObject" ++ subjectToString subject

simpleConditionToFOL :: SimpleCondition -> State (DateDictionary, TempQuanDictionary) FOLFormula
simpleConditionToFOL (SimConOne id holds subject verbStatus object receiver (DateSpe (DateSpeOnThe day month year))) =
    createFormulaSimpleCondition holds verbStatus subject object receiver day month year
simpleConditionToFOL (SimConOne id holds subject verbStatus object receiver (DateSpe (DateSpeOn day month year))) =
    createFormulaSimpleCondition holds verbStatus subject object receiver day month year
simpleConditionToFOL (SimConOne id holds subject verbStatus object receiver DateAny) =
    createFormulaSimpleConditionDAny holds verbStatus subject object receiver
simpleConditionToFOL (SimConOne id holds subject verbStatus object receiver (DateSome date)) =
    createFormulaSimpleConditionDSomeThe holds verbStatus subject object receiver date (DateSome date)
simpleConditionToFOL (SimConOne id holds subject verbStatus object receiver (DateThe date)) =
    createFormulaSimpleConditionDSomeThe holds verbStatus subject object receiver date (DateThe date)
simpleConditionToFOL (SimConOne id holds subject verbStatus object receiver (DateQuanSpecific temporalQuantifier day month year)) =
    createFormulaSimpleConditionDQuanSpe holds verbStatus temporalQuantifier subject object receiver day month year
simpleConditionToFOL (SimConOne id holds subject verbStatus object receiver (DateQuanSome temporalQuantifier date)) =
    createFormulaSimpleConditionDQuanSomeThe holds verbStatus temporalQuantifier subject object receiver date (DateQuanSome temporalQuantifier date)
simpleConditionToFOL (SimConOne id holds subject verbStatus object receiver (DateQuanThe temporalQuantifier date)) =
    createFormulaSimpleConditionDQuanSomeThe holds verbStatus temporalQuantifier subject object receiver date (DateQuanThe temporalQuantifier date)
simpleConditionToFOL (SimConOne id holds subject verbStatus object receiver (DateQuanTempSome temporalQuantifier temporalOffset tq date)) =
    createFormulaSimpleConditionDQuanTempSomeThe holds verbStatus temporalQuantifier subject object receiver temporalOffset tq date (DateQuanTempSome temporalQuantifier temporalOffset tq date)
simpleConditionToFOL (SimConOne id holds subject verbStatus object receiver (DateQuanTempThe temporalQuantifier temporalOffset tq date)) =
    createFormulaSimpleConditionDQuanTempSomeThe holds verbStatus temporalQuantifier subject object receiver temporalOffset tq date (DateQuanTempThe temporalQuantifier temporalOffset tq date)
simpleConditionToFOL (SimConOne id holds subject verbStatus object receiver (DateQuanSomeWO temporalOffset tq date)) =
    createFormulaSimpleConditionDQuanSomeTheWO holds verbStatus subject object receiver temporalOffset tq date (DateQuanSomeWO temporalOffset tq date)
simpleConditionToFOL (SimConOne id holds subject verbStatus object receiver (DateQuanTheWO temporalOffset tq date)) =
    createFormulaSimpleConditionDQuanSomeTheWO holds verbStatus subject object receiver temporalOffset tq date (DateQuanTheWO temporalOffset tq date)

simpleConditionToFOL (SimConTwo id holds subject (DateSpe (DateSpeOnThe day month year)) verbStatus object receiver) = 
    createFormulaSimpleCondition holds verbStatus subject object receiver day month year
simpleConditionToFOL (SimConTwo id holds subject (DateSpe (DateSpeOn day month year)) verbStatus object receiver) = 
    createFormulaSimpleCondition holds verbStatus subject object receiver day month year
simpleConditionToFOL (SimConTwo id holds subject DateAny verbStatus object receiver) = 
    createFormulaSimpleConditionDAny holds verbStatus subject object receiver
simpleConditionToFOL (SimConTwo id holds subject (DateSome date) verbStatus object receiver) = 
    createFormulaSimpleConditionDSomeThe holds verbStatus subject object receiver date (DateSome date)
simpleConditionToFOL (SimConTwo id holds subject (DateThe date) verbStatus object receiver) = 
    createFormulaSimpleConditionDSomeThe holds verbStatus subject object receiver date (DateThe date)
simpleConditionToFOL (SimConTwo id holds subject (DateQuanSpecific temporalQuantifier day month year) verbStatus object receiver) = 
    createFormulaSimpleConditionDQuanSpe holds verbStatus temporalQuantifier subject object receiver day month year
simpleConditionToFOL (SimConTwo id holds subject (DateQuanSome temporalQuantifier date) verbStatus object receiver) =
    createFormulaSimpleConditionDQuanSomeThe holds verbStatus temporalQuantifier subject object receiver date (DateQuanSome temporalQuantifier date)
simpleConditionToFOL (SimConTwo id holds subject (DateQuanThe temporalQuantifier date) verbStatus object receiver) =
    createFormulaSimpleConditionDQuanSomeThe holds verbStatus temporalQuantifier subject object receiver date (DateQuanThe temporalQuantifier date)
simpleConditionToFOL (SimConTwo id holds subject (DateQuanTempSome temporalQuantifier temporalOffset tq date) verbStatus object receiver) =
    createFormulaSimpleConditionDQuanTempSomeThe holds verbStatus temporalQuantifier subject object receiver temporalOffset tq date (DateQuanTempSome temporalQuantifier temporalOffset tq date)
simpleConditionToFOL (SimConTwo id holds subject (DateQuanTempThe temporalQuantifier temporalOffset tq date) verbStatus object receiver) =
    createFormulaSimpleConditionDQuanTempSomeThe holds verbStatus temporalQuantifier subject object receiver temporalOffset tq date (DateQuanTempThe temporalQuantifier temporalOffset tq date)
simpleConditionToFOL (SimConTwo id holds subject (DateQuanSomeWO temporalOffset tq date) verbStatus object receiver ) =
    createFormulaSimpleConditionDQuanSomeTheWO holds verbStatus subject object receiver temporalOffset tq date (DateQuanSomeWO temporalOffset tq date)
simpleConditionToFOL (SimConTwo id holds subject (DateQuanTheWO temporalOffset tq date) verbStatus object receiver) =
    createFormulaSimpleConditionDQuanSomeTheWO holds verbStatus subject object receiver temporalOffset tq date (DateQuanTheWO temporalOffset tq date)

simpleConditionToFOL (SimConThree id holds (DateSpe (DateSpeOnThe day month year)) subject verbStatus object receiver) = 
    createFormulaSimpleCondition holds verbStatus subject object receiver day month year 
simpleConditionToFOL (SimConThree id holds (DateSpe (DateSpeOn day month year)) subject verbStatus object receiver) = 
    createFormulaSimpleCondition holds verbStatus subject object receiver day month year 
simpleConditionToFOL (SimConThree id holds DateAny subject verbStatus object receiver) = 
    createFormulaSimpleConditionDAny holds verbStatus subject object receiver
simpleConditionToFOL (SimConThree id holds (DateSome date) subject verbStatus object receiver) = 
    createFormulaSimpleConditionDSomeThe holds verbStatus subject object receiver date (DateSome date)
simpleConditionToFOL (SimConThree id holds (DateThe date) subject verbStatus object receiver) = 
    createFormulaSimpleConditionDSomeThe holds verbStatus subject object receiver date (DateThe date)
simpleConditionToFOL (SimConThree id holds (DateQuanSpecific temporalQuantifier day month year) subject verbStatus object receiver) = 
    createFormulaSimpleConditionDQuanSpe holds verbStatus temporalQuantifier subject object receiver day month year 
simpleConditionToFOL (SimConThree id holds (DateQuanSome temporalQuantifier date) subject verbStatus object receiver) =
    createFormulaSimpleConditionDQuanSomeThe holds verbStatus temporalQuantifier subject object receiver date (DateQuanSome temporalQuantifier date)
simpleConditionToFOL (SimConThree id holds (DateQuanThe temporalQuantifier date) subject verbStatus object receiver) =
    createFormulaSimpleConditionDQuanSomeThe holds verbStatus temporalQuantifier subject object receiver date (DateQuanThe temporalQuantifier date)
simpleConditionToFOL (SimConThree id holds (DateQuanTempSome temporalQuantifier temporalOffset tq date) subject verbStatus object receiver) =
    createFormulaSimpleConditionDQuanTempSomeThe holds verbStatus temporalQuantifier subject object receiver temporalOffset tq date (DateQuanTempSome temporalQuantifier temporalOffset tq date)
simpleConditionToFOL (SimConThree id holds (DateQuanTempThe temporalQuantifier temporalOffset tq date) subject verbStatus object receiver) =
    createFormulaSimpleConditionDQuanTempSomeThe holds verbStatus temporalQuantifier subject object receiver temporalOffset tq date (DateQuanTempThe temporalQuantifier temporalOffset tq date)
simpleConditionToFOL (SimConThree id holds (DateQuanSomeWO temporalOffset tq date) subject verbStatus object receiver ) =
    createFormulaSimpleConditionDQuanSomeTheWO holds verbStatus subject object receiver temporalOffset tq date (DateQuanSomeWO temporalOffset tq date)
simpleConditionToFOL (SimConThree id holds (DateQuanTheWO temporalOffset tq date) subject verbStatus object receiver) =
    createFormulaSimpleConditionDQuanSomeTheWO holds verbStatus subject object receiver temporalOffset tq date (DateQuanTheWO temporalOffset tq date)

simpleConditionToFOL (SimConFour id holds subject modalVerb verb object receiver (DateSpe (DateSpeOnThe day month year))) =
    createFormulaSimpleStatement holds modalVerb verb subject object receiver day month year
simpleConditionToFOL (SimConFour id holds subject modalVerb verb object receiver (DateSpe (DateSpeOn day month year))) =
    createFormulaSimpleStatement holds modalVerb verb subject object receiver day month year
simpleConditionToFOL (SimConFour id holds subject modalVerb verb object receiver DateAny) =
    createFormulaSimpleStatementDAny holds modalVerb verb subject object receiver
simpleConditionToFOL (SimConFour id holds subject modalVerb verb object receiver (DateSome date)) =
    createFormulaSimpleStatementDSomeThe holds modalVerb verb subject object receiver date (DateSome date)
simpleConditionToFOL (SimConFour id holds subject modalVerb verb object receiver (DateThe date)) =
    createFormulaSimpleStatementDSomeThe holds modalVerb verb subject object receiver date (DateThe date)
simpleConditionToFOL (SimConFour id holds subject modalVerb verb object receiver (DateQuanSpecific temporalQuantifier day month year)) =
    createFormulaSimpleStatementDQuanSpe holds modalVerb verb temporalQuantifier subject object receiver day month year
simpleConditionToFOL (SimConFour id holds subject modalVerb verb object receiver (DateQuanSome temporalQuantifier date)) =
    createFormulaSimpleStatementDQuanSomeThe holds modalVerb verb temporalQuantifier subject object receiver date (DateQuanSome temporalQuantifier date)
simpleConditionToFOL (SimConFour id holds subject modalVerb verb object receiver (DateQuanThe temporalQuantifier date)) =
    createFormulaSimpleStatementDQuanSomeThe holds modalVerb verb temporalQuantifier subject object receiver date (DateQuanThe temporalQuantifier date)
simpleConditionToFOL (SimConFour id holds subject modalVerb verb object receiver (DateQuanTempSome temporalQuantifier temporalOffset tq date)) =
    createFormulaSimpleStatementDQuanTempSomeThe holds modalVerb verb temporalQuantifier subject object receiver temporalOffset tq date (DateQuanTempSome temporalQuantifier temporalOffset tq date)
simpleConditionToFOL (SimConFour id holds subject modalVerb verb object receiver (DateQuanTempThe temporalQuantifier temporalOffset tq date)) =
    createFormulaSimpleStatementDQuanTempSomeThe holds modalVerb verb temporalQuantifier subject object receiver temporalOffset tq date (DateQuanTempThe temporalQuantifier temporalOffset tq date)
simpleConditionToFOL (SimConFour id holds subject modalVerb verb object receiver (DateQuanSomeWO temporalOffset tq date)) =
    createFormulaSimpleStatementDQuanSomeTheWO holds modalVerb verb subject object receiver temporalOffset tq date (DateQuanSomeWO temporalOffset tq date)
simpleConditionToFOL (SimConFour id holds subject modalVerb verb object receiver (DateQuanTheWO temporalOffset tq date)) =
    createFormulaSimpleStatementDQuanSomeTheWO holds modalVerb verb subject object receiver temporalOffset tq date (DateQuanTheWO temporalOffset tq date)

simpleConditionToFOL (SimConFive id HoldYes booleanExpression) = boolExToFOL booleanExpression
simpleConditionToFOL (SimConFive id HoldNo booleanExpression) = Not <$> (boolExToFOL booleanExpression)

simpleConditionToFOL (SimConOneNH id subject verbStatus object receiver (DateSpe (DateSpeOnThe day month year))) =
    createFormulaSimpleConditionNH verbStatus subject object receiver day month year
simpleConditionToFOL (SimConOneNH id subject verbStatus object receiver (DateSpe (DateSpeOn day month year))) =
    createFormulaSimpleConditionNH verbStatus subject object receiver day month year
simpleConditionToFOL (SimConOneNH id subject verbStatus object receiver DateAny) =
    createFormulaSimpleConditionNHDAny verbStatus subject object receiver 
simpleConditionToFOL (SimConOneNH id subject verbStatus object receiver (DateSome date)) =
    createFormulaSimpleConditionNHDSomeThe verbStatus subject object receiver date (DateSome date)
simpleConditionToFOL (SimConOneNH id subject verbStatus object receiver (DateThe date)) =
    createFormulaSimpleConditionNHDSomeThe verbStatus subject object receiver date (DateThe date)
simpleConditionToFOL (SimConOneNH id subject verbStatus object receiver (DateQuanSpecific temporalQuantifier day month year)) =
    createFormulaSimpleConditionNHDQuanSpe verbStatus temporalQuantifier subject object receiver day month year
simpleConditionToFOL (SimConOneNH id subject verbStatus object receiver (DateQuanSome temporalQuantifier date)) =
    createFormulaSimpleConditionNHDQuanSomeThe verbStatus temporalQuantifier subject object receiver date (DateQuanSome temporalQuantifier date)
simpleConditionToFOL (SimConOneNH id subject verbStatus object receiver (DateQuanThe temporalQuantifier date)) =
    createFormulaSimpleConditionNHDQuanSomeThe verbStatus temporalQuantifier subject object receiver date (DateQuanThe temporalQuantifier date)
simpleConditionToFOL (SimConOneNH id subject verbStatus object receiver (DateQuanTempSome temporalQuantifier temporalOffset tq date)) =
    createFormulaSimpleConditionNHDQuanTempSomeThe verbStatus temporalQuantifier subject object receiver temporalOffset tq date (DateQuanTempSome temporalQuantifier temporalOffset tq date)
simpleConditionToFOL (SimConOneNH id subject verbStatus object receiver (DateQuanTempThe temporalQuantifier temporalOffset tq date)) =
    createFormulaSimpleConditionNHDQuanTempSomeThe verbStatus temporalQuantifier subject object receiver temporalOffset tq date (DateQuanTempThe temporalQuantifier temporalOffset tq date)
simpleConditionToFOL (SimConOneNH id subject verbStatus object receiver (DateQuanSomeWO temporalOffset tq date)) =
    createFormulaSimpleConditionNHDQuanSomeTheWO verbStatus subject object receiver temporalOffset tq date (DateQuanSomeWO temporalOffset tq date)
simpleConditionToFOL (SimConOneNH id subject verbStatus object receiver (DateQuanTheWO temporalOffset tq date)) =
    createFormulaSimpleConditionNHDQuanSomeTheWO verbStatus subject object receiver temporalOffset tq date (DateQuanTheWO temporalOffset tq date)

simpleConditionToFOL (SimConTwoNH id subject (DateSpe (DateSpeOnThe day month year)) verbStatus object receiver) = 
    createFormulaSimpleConditionNH verbStatus subject object receiver day month year
simpleConditionToFOL (SimConTwoNH id subject (DateSpe (DateSpeOn day month year)) verbStatus object receiver) = 
    createFormulaSimpleConditionNH verbStatus subject object receiver day month year
simpleConditionToFOL (SimConTwoNH id subject DateAny verbStatus object receiver) = 
    createFormulaSimpleConditionNHDAny verbStatus subject object receiver 
simpleConditionToFOL (SimConTwoNH id subject (DateSome date) verbStatus object receiver) = 
    createFormulaSimpleConditionNHDSomeThe verbStatus subject object receiver date (DateSome date)
simpleConditionToFOL (SimConTwoNH id subject (DateThe date) verbStatus object receiver) = 
    createFormulaSimpleConditionNHDSomeThe verbStatus subject object receiver date (DateThe date)
simpleConditionToFOL (SimConTwoNH id subject (DateQuanSpecific temporalQuantifier day month year) verbStatus object receiver) = 
    createFormulaSimpleConditionNHDQuanSpe verbStatus temporalQuantifier subject object receiver day month year
simpleConditionToFOL (SimConTwoNH id subject (DateQuanSome temporalQuantifier date) verbStatus object receiver) =
    createFormulaSimpleConditionNHDQuanSomeThe verbStatus temporalQuantifier subject object receiver date (DateQuanSome temporalQuantifier date)
simpleConditionToFOL (SimConTwoNH id subject (DateQuanThe temporalQuantifier date) verbStatus object receiver) =
    createFormulaSimpleConditionNHDQuanSomeThe verbStatus temporalQuantifier subject object receiver date (DateQuanThe temporalQuantifier date)
simpleConditionToFOL (SimConTwoNH id subject (DateQuanTempSome temporalQuantifier temporalOffset tq date) verbStatus object receiver) =
    createFormulaSimpleConditionNHDQuanTempSomeThe verbStatus temporalQuantifier subject object receiver temporalOffset tq date (DateQuanTempSome temporalQuantifier temporalOffset tq date)
simpleConditionToFOL (SimConTwoNH id subject (DateQuanTempThe temporalQuantifier temporalOffset tq date) verbStatus object receiver) =
    createFormulaSimpleConditionNHDQuanTempSomeThe verbStatus temporalQuantifier subject object receiver temporalOffset tq date (DateQuanTempThe temporalQuantifier temporalOffset tq date)
simpleConditionToFOL (SimConTwoNH id subject (DateQuanSomeWO temporalOffset tq date) verbStatus object receiver ) =
    createFormulaSimpleConditionNHDQuanSomeTheWO verbStatus subject object receiver temporalOffset tq date (DateQuanSomeWO temporalOffset tq date)
simpleConditionToFOL (SimConTwoNH id subject (DateQuanTheWO temporalOffset tq date) verbStatus object receiver) =
    createFormulaSimpleConditionNHDQuanSomeTheWO verbStatus subject object receiver temporalOffset tq date (DateQuanTheWO temporalOffset tq date)

simpleConditionToFOL (SimConThreeNH id (DateSpe (DateSpeOnThe day month year)) subject verbStatus object receiver) = 
    createFormulaSimpleConditionNH verbStatus subject object receiver day month year 
simpleConditionToFOL (SimConThreeNH id (DateSpe (DateSpeOn day month year)) subject verbStatus object receiver) = 
    createFormulaSimpleConditionNH verbStatus subject object receiver day month year 
simpleConditionToFOL (SimConThreeNH id DateAny subject verbStatus object receiver) = 
    createFormulaSimpleConditionNHDAny verbStatus subject object receiver
simpleConditionToFOL (SimConThreeNH id (DateSome date) subject verbStatus object receiver) = 
    createFormulaSimpleConditionNHDSomeThe verbStatus subject object receiver date (DateSome date)
simpleConditionToFOL (SimConThreeNH id (DateThe date) subject verbStatus object receiver) = 
    createFormulaSimpleConditionNHDSomeThe verbStatus subject object receiver date (DateThe date)
simpleConditionToFOL (SimConThreeNH id (DateQuanSpecific temporalQuantifier day month year) subject verbStatus object receiver) = 
    createFormulaSimpleConditionNHDQuanSpe verbStatus temporalQuantifier subject object receiver day month year
simpleConditionToFOL (SimConThreeNH id (DateQuanSome temporalQuantifier date) subject verbStatus object receiver) =
    createFormulaSimpleConditionNHDQuanSomeThe verbStatus temporalQuantifier subject object receiver date (DateQuanSome temporalQuantifier date)
simpleConditionToFOL (SimConThreeNH id (DateQuanThe temporalQuantifier date) subject verbStatus object receiver) =
    createFormulaSimpleConditionNHDQuanSomeThe verbStatus temporalQuantifier subject object receiver date (DateQuanThe temporalQuantifier date)
simpleConditionToFOL (SimConThreeNH id (DateQuanTempSome temporalQuantifier temporalOffset tq date) subject verbStatus object receiver) =
    createFormulaSimpleConditionNHDQuanTempSomeThe verbStatus temporalQuantifier subject object receiver temporalOffset tq date (DateQuanTempSome temporalQuantifier temporalOffset tq date)
simpleConditionToFOL (SimConThreeNH id (DateQuanTempThe temporalQuantifier temporalOffset tq date) subject verbStatus object receiver) =
    createFormulaSimpleConditionNHDQuanTempSomeThe verbStatus temporalQuantifier subject object receiver temporalOffset tq date (DateQuanTempThe temporalQuantifier temporalOffset tq date)
simpleConditionToFOL (SimConThreeNH id (DateQuanSomeWO temporalOffset tq date) subject verbStatus object receiver ) =
    createFormulaSimpleConditionNHDQuanSomeTheWO verbStatus subject object receiver temporalOffset tq date (DateQuanSomeWO temporalOffset tq date)
simpleConditionToFOL (SimConThreeNH id (DateQuanTheWO temporalOffset tq date) subject verbStatus object receiver) =
    createFormulaSimpleConditionNHDQuanSomeTheWO verbStatus subject object receiver temporalOffset tq date (DateQuanTheWO temporalOffset tq date)

simpleConditionToFOL (SimConFourNH id subject modalVerb verb object receiver (DateSpe (DateSpeOnThe day month year))) =
    createFormulaSimpleStatementNH modalVerb verb subject object receiver day month year
simpleConditionToFOL (SimConFourNH id subject modalVerb verb object receiver (DateSpe (DateSpeOn day month year))) =
    createFormulaSimpleStatementNH modalVerb verb subject object receiver day month year
simpleConditionToFOL (SimConFourNH id subject modalVerb verb object receiver DateAny) =
    createFormulaSimpleStatementNHDAny modalVerb verb subject object receiver
simpleConditionToFOL (SimConFourNH id subject modalVerb verb object receiver (DateSome date)) =
    createFormulaSimpleStatementNHDSomeThe modalVerb verb subject object receiver date (DateSome date)
simpleConditionToFOL (SimConFourNH id subject modalVerb verb object receiver (DateThe date)) =
    createFormulaSimpleStatementNHDSomeThe modalVerb verb subject object receiver date (DateThe date)
simpleConditionToFOL (SimConFourNH id subject modalVerb verb object receiver (DateQuanSpecific temporalQuantifier day month year)) =
    createFormulaSimpleStatementNHDQuanSpe modalVerb verb temporalQuantifier subject object receiver day month year
simpleConditionToFOL (SimConFourNH id subject modalVerb verb object receiver (DateQuanSome temporalQuantifier date)) =
    createFormulaSimpleStatementNHDQuanSomeThe modalVerb verb temporalQuantifier subject object receiver date (DateQuanSome temporalQuantifier date)
simpleConditionToFOL (SimConFourNH id subject modalVerb verb object receiver (DateQuanThe temporalQuantifier date)) =
    createFormulaSimpleStatementNHDQuanSomeThe modalVerb verb temporalQuantifier subject object receiver date (DateQuanThe temporalQuantifier date)
simpleConditionToFOL (SimConFourNH id subject modalVerb verb object receiver (DateQuanTempSome temporalQuantifier temporalOffset tq date)) =
    createFormulaSimpleStatementNHDQuanTempSomeThe modalVerb verb temporalQuantifier subject object receiver temporalOffset tq date (DateQuanTempSome temporalQuantifier temporalOffset tq date)
simpleConditionToFOL (SimConFourNH id subject modalVerb verb object receiver (DateQuanTempThe temporalQuantifier temporalOffset tq date)) =
    createFormulaSimpleStatementNHDQuanTempSomeThe modalVerb verb temporalQuantifier subject object receiver temporalOffset tq date (DateQuanTempThe temporalQuantifier temporalOffset tq date)
simpleConditionToFOL (SimConFourNH id subject modalVerb verb object receiver (DateQuanSomeWO temporalOffset tq date)) =
    createFormulaSimpleStatementNHDQuanSomeTheWO modalVerb verb subject object receiver temporalOffset tq date (DateQuanSomeWO temporalOffset tq date)
simpleConditionToFOL (SimConFourNH id subject modalVerb verb object receiver (DateQuanTheWO temporalOffset tq date)) =
    createFormulaSimpleStatementNHDQuanSomeTheWO modalVerb verb subject object receiver temporalOffset tq date (DateQuanTheWO temporalOffset tq date)

simpleConditiontoFOL (SimConFiveNH id booleanExpression) = boolExToFOL booleanExpression

createFormulaSimpleStatementDateCheck :: Holds -> VerbStatus -> Subject -> Object -> Receiver -> Num -> Month -> Num -> State (DateDictionary, TempQuanDictionary) FOLFormula
createFormulaSimpleStatementDateCheck holds verbStatus subject object receiver day month year =
    let
        -- Common base predicates
        basePredicates =
            And
                (And
                    (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                    (Pred "date" [Var "D", dateSpeToTerm year month day])
                )
                (objectToPredicate object)
                
        -- Predicates for different cases
        yesDelivered = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "delivered" [Var "X", Var "Y", Var "O", Var "D"])
        noDelivered = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "delivered" [Var "X", Var "Y", Var "O", Var "D"])
        yesPaid = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "paid" [Var "X", Var "Y", Var "O", Var "D"])
        noPaid = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "paid" [Var "X", Var "Y", Var "O", Var "D"])
        yesCharged = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "charged" [Var "X", Var "Y", Var "O", Var "D"])
        noCharged = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "charged" [Var "X", Var "Y", Var "O", Var "D"])
        yesRefunded = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "refunded" [Var "X", Var "Y", Var "O", Var "D"])
        noRefunded = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "refunded" [Var "X", Var "Y", Var "O", Var "D"])
    in
        case (holds, verbStatus) of
            (HoldYes, VSDel) -> do
                formula <- yesDelivered
                checkValue <- checkTempQuanDictionary (connectTerms subject (verbStatusToVerb verbStatus) receiver object) (dateSpeToInt year month day)
                if checkValue
                    then return formula
                    else return $ Falser
            (HoldNo, VSDel) -> do
                formula <- noDelivered
                return formula

            (HoldYes, VSPay) -> do
                formula <- yesPaid
                checkValue <- checkTempQuanDictionary (connectTerms subject (verbStatusToVerb verbStatus) receiver object) (dateSpeToInt year month day)
                if checkValue
                    then return formula
                    else return $ Falser
            (HoldNo, VSPay) -> do
                formula <- noPaid
                return formula

            (HoldYes, VSCharge) -> do
                formula <- yesCharged
                checkValue <- checkTempQuanDictionary (connectTerms subject (verbStatusToVerb verbStatus) receiver object) (dateSpeToInt year month day)
                if checkValue
                    then return formula
                    else return $ Falser
            (HoldNo, VSCharge) -> do
                formula <- noCharged
                return formula

            (HoldYes, VSRefund) -> do
                formula <- yesRefunded
                checkValue <- checkTempQuanDictionary (connectTerms subject (verbStatusToVerb verbStatus) receiver object) (dateSpeToInt year month day)
                if checkValue
                    then return formula
                    else return $ Falser
            (HoldNo, VSRefund) -> do
                formula <- noRefunded
                return formula

createFormulaSimpleStatementNHDateCheck :: VerbStatus -> Subject -> Object -> Receiver -> Num -> Month -> Num -> State (DateDictionary, TempQuanDictionary) FOLFormula
createFormulaSimpleStatementNHDateCheck verbStatus subject object receiver day month year =
    let
        -- Common base predicates
        basePredicates =
            And
                (And
                    (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                    (Pred "date" [Var "D", dateSpeToTerm year month day])
                )
                (objectToPredicate object)
                
        -- Predicates for different cases
        yesDelivered = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "delivered" [Var "X", Var "Y", Var "O", Var "D"])
        yesPaid = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "paid" [Var "X", Var "Y", Var "O", Var "D"])
        yesCharged = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "charged" [Var "X", Var "Y", Var "O", Var "D"])
        yesRefunded = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "refunded" [Var "X", Var "Y", Var "O", Var "D"])
    in
        case (verbStatus) of
            (VSDel) -> do
                formula <- yesDelivered
                checkValue <- checkTempQuanDictionary (connectTerms subject (verbStatusToVerb verbStatus) receiver object) (dateSpeToInt year month day)
                if checkValue
                    then return formula
                    else return $ Falser

            (VSPay) -> do
                formula <- yesPaid
                checkValue <- checkTempQuanDictionary (connectTerms subject (verbStatusToVerb verbStatus) receiver object) (dateSpeToInt year month day)
                if checkValue
                    then return formula
                    else return $ Falser

            (VSCharge) -> do
                formula <- yesCharged
                checkValue <- checkTempQuanDictionary (connectTerms subject (verbStatusToVerb verbStatus) receiver object) (dateSpeToInt year month day)
                if checkValue
                    then return formula
                    else return $ Falser

            (VSRefund) -> do
                formula <- yesRefunded
                checkValue <- checkTempQuanDictionary (connectTerms subject (verbStatusToVerb verbStatus) receiver object) (dateSpeToInt year month day)
                if checkValue
                    then return formula
                    else return $ Falser

createFormulaSimpleStatementDateCheckSomeThe :: Holds -> VerbStatus -> Subject -> Object -> Receiver -> TemporalOffset -> TemporalQuantifier -> Subject -> Date -> State (DateDictionary, TempQuanDictionary) FOLFormula
createFormulaSimpleStatementDateCheckSomeThe holds verbStatus subject object receiver temporalOffset tq date dateType = do
    -- Get the actual value associated with the date
    actualValue <- lookupDate (subjectToString date)

    -- Use actualValue here
    let basePredicates = case (dateType, tq) of
            (DateQuanSomeWO _ _ _, TempBefore) -> And
                            (And
                                (And
                                    (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                    (Pred "isDate" [Var actualValue])
                                )
                                (Pred "dateBefore" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)
            (DateQuanSomeWO _ _ _, TempAfter) -> And
                            (And
                                (And
                                    (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                    (Pred "isDate" [Var actualValue])
                                )
                                (Pred "dateAfter" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)
            (DateQuanTheWO _ _ _, TempBefore) -> And
                            (And
                                (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                (Pred "dateBefore" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)
            (DateQuanTheWO _ _ _, TempAfter) -> And
                            (And
                                (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                (Pred "dateAfter" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)

    -- Generate FOL formulas based on different cases
    generateFormulas basePredicates actualValue

    where
        -- Function to generate FOL formulas based on different cases
        generateFormulas basePredicates actualValue = 
            case (holds, verbStatus) of
                (HoldYes, VSDel) -> do
                    formula <- yesDelivered
                    checkValue <- checkTempQuanDictionary ((connectTerms subject (verbStatusToVerb verbStatus) receiver object) ++ "SomeThe") (temporalOffsetToInt temporalOffset)
                    if checkValue
                        then return formula
                        else return $ Falser
                (HoldNo, VSDel) -> do
                    formula <- noDelivered
                    return formula

                (HoldYes, VSPay) -> do
                    formula <- yesPaid
                    checkValue <- checkTempQuanDictionary ((connectTerms subject (verbStatusToVerb verbStatus) receiver object) ++ "SomeThe") (temporalOffsetToInt temporalOffset)
                    if checkValue
                        then return formula
                        else return $ Falser
                (HoldNo, VSPay) -> do
                    formula <- noPaid
                    return formula

                (HoldYes, VSCharge) -> do
                    formula <- yesCharged
                    checkValue <- checkTempQuanDictionary ((connectTerms subject (verbStatusToVerb verbStatus) receiver object) ++ "SomeThe") (temporalOffsetToInt temporalOffset)
                    if checkValue
                        then return formula
                        else return $ Falser
                (HoldNo, VSCharge) -> do
                    formula <- noCharged
                    return formula

                (HoldYes, VSRefund) -> do
                    formula <- yesRefunded
                    checkValue <- checkTempQuanDictionary ((connectTerms subject (verbStatusToVerb verbStatus) receiver object) ++ "SomeThe") (temporalOffsetToInt temporalOffset)
                    if checkValue
                        then return formula
                        else return $ Falser
                (HoldNo, VSRefund) -> do
                    formula <- noRefunded
                    return formula

            where
                -- Predicates for different cases
                yesDelivered = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "delivered" [Var "X", Var "Y", Var "O", Var "D"])
                noDelivered = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "delivered" [Var "X", Var "Y", Var "O", Var "D"])
                yesPaid = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "paid" [Var "X", Var "Y", Var "O", Var "D"])
                noPaid = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "paid" [Var "X", Var "Y", Var "O", Var "D"])
                yesCharged = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "charged" [Var "X", Var "Y", Var "O", Var "D"])
                noCharged = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "charged" [Var "X", Var "Y", Var "O", Var "D"])
                yesRefunded = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "refunded" [Var "X", Var "Y", Var "O", Var "D"])
                noRefunded = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "refunded" [Var "X", Var "Y", Var "O", Var "D"])

createFormulaSimpleStatementNHDateCheckSomeThe :: VerbStatus -> Subject -> Object -> Receiver -> TemporalOffset -> TemporalQuantifier -> Subject -> Date -> State (DateDictionary, TempQuanDictionary) FOLFormula
createFormulaSimpleStatementNHDateCheckSomeThe verbStatus subject object receiver temporalOffset tq date dateType = do
    -- Get the actual value associated with the date
    actualValue <- lookupDate (subjectToString date)

    -- Use actualValue here
    let basePredicates = case (dateType, tq) of
            (DateQuanSomeWO _ _ _, TempBefore) -> And
                            (And
                                (And
                                    (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                    (Pred "isDate" [Var actualValue])
                                )
                                (Pred "dateBefore" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)
            (DateQuanSomeWO _ _ _, TempAfter) -> And
                            (And
                                (And
                                    (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                    (Pred "isDate" [Var actualValue])
                                )
                                (Pred "dateAfter" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)
            (DateQuanTheWO _ _ _, TempBefore) -> And
                            (And
                                (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                (Pred "dateBefore" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)
            (DateQuanTheWO _ _ _, TempAfter) -> And
                            (And
                                (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                (Pred "dateAfter" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)

    -- Generate FOL formulas based on different cases
    generateFormulas basePredicates actualValue

    where
        -- Function to generate FOL formulas based on different cases
        generateFormulas basePredicates actualValue = 
            case (verbStatus) of
                (VSDel) -> do
                    formula <- yesDelivered
                    checkValue <- checkTempQuanDictionary ((connectTerms subject (verbStatusToVerb verbStatus) receiver object) ++ "SomeThe") (temporalOffsetToInt temporalOffset)
                    if checkValue
                        then return formula
                        else return $ Falser

                (VSPay) -> do
                    formula <- yesPaid
                    checkValue <- checkTempQuanDictionary ((connectTerms subject (verbStatusToVerb verbStatus) receiver object) ++ "SomeThe") (temporalOffsetToInt temporalOffset)
                    if checkValue
                        then return formula
                        else return $ Falser

                (VSCharge) -> do
                    formula <- yesCharged
                    checkValue <- checkTempQuanDictionary ((connectTerms subject (verbStatusToVerb verbStatus) receiver object) ++ "SomeThe") (temporalOffsetToInt temporalOffset)
                    if checkValue
                        then return formula
                        else return $ Falser

                (VSRefund) -> do
                    formula <- yesRefunded
                    checkValue <- checkTempQuanDictionary ((connectTerms subject (verbStatusToVerb verbStatus) receiver object) ++ "SomeThe") (temporalOffsetToInt temporalOffset)
                    if checkValue
                        then return formula
                        else return $ Falser

            where
                -- Predicates for different cases
                yesDelivered = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "delivered" [Var "X", Var "Y", Var "O", Var "D"])
                yesPaid = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "paid" [Var "X", Var "Y", Var "O", Var "D"])
                yesCharged = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "charged" [Var "X", Var "Y", Var "O", Var "D"])
                yesRefunded = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "refunded" [Var "X", Var "Y", Var "O", Var "D"])
                
createFormulaSimpleCondition :: Holds -> VerbStatus -> Subject -> Object -> Receiver -> Num -> Month -> Num -> State (DateDictionary, TempQuanDictionary) FOLFormula
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
                    (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                    (Pred "date" [Var "D", dateSpeToTerm year month day])
                )
                (objectToPredicate object)
                
        -- Predicates for different cases
        yesDelivered = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "delivered" [Var "X", Var "Y", Var "O", Var "D"])
        noDelivered = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "delivered" [Var "X", Var "Y", Var "O", Var "D"])
        yesPaid = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "paid" [Var "X", Var "Y", Var "O", Var "D"])
        noPaid = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "paid" [Var "X", Var "Y", Var "O", Var "D"])
        yesCharged = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "charged" [Var "X", Var "Y", Var "O", Var "D"])
        noCharged = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "charged" [Var "X", Var "Y", Var "O", Var "D"])
        yesRefunded = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "refunded" [Var "X", Var "Y", Var "O", Var "D"])
        noRefunded = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "refunded" [Var "X", Var "Y", Var "O", Var "D"])

createFormulaSimpleConditionDAny :: Holds -> VerbStatus -> Subject -> Object -> Receiver -> State (DateDictionary, TempQuanDictionary) FOLFormula
createFormulaSimpleConditionDAny holds verbStatus subject object receiver =
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
                (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                (objectToPredicate object)
                
        -- Predicates for different cases
        yesDelivered = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "delivered" [Var "X", Var "Y", Var "O", Var "D"])
        noDelivered = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "delivered" [Var "X", Var "Y", Var "O", Var "D"])
        yesPaid = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "paid" [Var "X", Var "Y", Var "O", Var "D"])
        noPaid = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "paid" [Var "X", Var "Y", Var "O", Var "D"])
        yesCharged = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "charged" [Var "X", Var "Y", Var "O", Var "D"])
        noCharged = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "charged" [Var "X", Var "Y", Var "O", Var "D"])
        yesRefunded = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "refunded" [Var "X", Var "Y", Var "O", Var "D"])
        noRefunded = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "refunded" [Var "X", Var "Y", Var "O", Var "D"])

createFormulaSimpleConditionDSomeThe :: Holds -> VerbStatus -> Subject -> Object -> Receiver -> Subject -> Date -> State (DateDictionary, TempQuanDictionary) FOLFormula
createFormulaSimpleConditionDSomeThe holds verbStatus subject object receiver date dateType = do
    -- Get the actual value associated with the date
    actualValue <- lookupDate (subjectToString date)

    -- Use actualValue here
    let basePredicates = case dateType of
            DateSome _ -> And
                            (And
                                (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                (Pred "isDate" [Var actualValue])
                            )
                            (objectToPredicate object)
            DateThe _ -> And
                            (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                            (objectToPredicate object)

    -- Generate FOL formulas based on different cases
    generateFormulas basePredicates actualValue

    where
        -- Function to generate FOL formulas based on different cases
        generateFormulas basePredicates actualValue = 
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
                -- Predicates for different cases
                yesDelivered = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "delivered" [Var "X", Var "Y", Var "O", Var actualValue])
                noDelivered = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "delivered" [Var "X", Var "Y", Var "O", Var actualValue])
                yesPaid = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "paid" [Var "X", Var "Y", Var "O", Var actualValue])
                noPaid = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "paid" [Var "X", Var "Y", Var "O", Var actualValue])
                yesCharged = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "charged" [Var "X", Var "Y", Var "O", Var actualValue])
                noCharged = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "charged" [Var "X", Var "Y", Var "O", Var actualValue])
                yesRefunded = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "refunded" [Var "X", Var "Y", Var "O", Var actualValue])
                noRefunded = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "refunded" [Var "X", Var "Y", Var "O", Var actualValue])

createFormulaSimpleConditionDQuanSpe :: Holds -> VerbStatus -> TemporalQuantifier -> Subject -> Object -> Receiver -> Num -> Month -> Num -> State (DateDictionary, TempQuanDictionary) FOLFormula
createFormulaSimpleConditionDQuanSpe holds verbStatus temporalQuantifier subject object receiver day month year =
    case (holds, verbStatus, temporalQuantifier) of
        (HoldYes, VSDel, TempBefore) -> yesDeliveredBefore
        (HoldNo, VSDel, TempBefore) -> noDeliveredBefore
        (HoldYes, VSPay, TempBefore) -> yesPaidBefore
        (HoldNo, VSPay, TempBefore) -> noPaidBefore
        (HoldYes, VSCharge, TempBefore) -> yesChargedBefore
        (HoldNo, VSCharge, TempBefore) -> noChargedBefore
        (HoldYes, VSRefund, TempBefore) -> yesRefundedBefore
        (HoldNo, VSRefund, TempBefore) -> noRefundedBefore
        (HoldYes, VSDel, TempAfter) -> yesDeliveredAfter
        (HoldNo, VSDel, TempAfter) -> noDeliveredAfter
        (HoldYes, VSPay, TempAfter) -> yesPaidAfter
        (HoldNo, VSPay, TempAfter) -> noPaidAfter
        (HoldYes, VSCharge, TempAfter) -> yesChargedAfter
        (HoldNo, VSCharge, TempAfter) -> noChargedAfter
        (HoldYes, VSRefund, TempAfter) -> yesRefundedAfter
        (HoldNo, VSRefund, TempAfter) -> noRefundedAfter

    where
        -- Common base predicates
        basePredicates =
            And
                (And
                    (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                    (Pred "date" [Var "D", dateSpeToTerm year month day])
                )
                (objectToPredicate object)
                
        -- Predicates for different cases
        yesDeliveredBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "deliveredBefore" [Var "X", Var "Y", Var "O", Var "D"])
        noDeliveredBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "deliveredBefore" [Var "X", Var "Y", Var "O", Var "D"])
        yesPaidBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "paidBefore" [Var "X", Var "Y", Var "O", Var "D"])
        noPaidBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "paidBefore" [Var "X", Var "Y", Var "O", Var "D"])
        yesChargedBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "chargedBefore" [Var "X", Var "Y", Var "O", Var "D"])
        noChargedBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "chargedBefore" [Var "X", Var "Y", Var "O", Var "D"])
        yesRefundedBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "refundedBefore" [Var "X", Var "Y", Var "O", Var "D"])
        noRefundedBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "refundedBefore" [Var "X", Var "Y", Var "O", Var "D"])
        yesDeliveredAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "deliveredAfter" [Var "X", Var "Y", Var "O", Var "D"])
        noDeliveredAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "deliveredAfter" [Var "X", Var "Y", Var "O", Var "D"])
        yesPaidAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "paidAfter" [Var "X", Var "Y", Var "O", Var "D"])
        noPaidAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "paidAfter" [Var "X", Var "Y", Var "O", Var "D"])
        yesChargedAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "chargedAfter" [Var "X", Var "Y", Var "O", Var "D"])
        noChargedAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "chargedAfter" [Var "X", Var "Y", Var "O", Var "D"])
        yesRefundedAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "refundedAfter" [Var "X", Var "Y", Var "O", Var "D"])
        noRefundedAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "refundedAfter" [Var "X", Var "Y", Var "O", Var "D"])

createFormulaSimpleConditionDQuanSomeThe :: Holds -> VerbStatus -> TemporalQuantifier -> Subject -> Object -> Receiver -> Subject -> Date -> State (DateDictionary, TempQuanDictionary) FOLFormula
createFormulaSimpleConditionDQuanSomeThe holds verbStatus temporalQuantifier subject object receiver date dateType = do
    -- Get the actual value associated with the date
    actualValue <- lookupDate (subjectToString date)

    -- Use actualValue here
    let basePredicates = case dateType of
            DateQuanSome _ _ -> And
                            (And
                                (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                (Pred "isDate" [Var actualValue])
                            )
                            (objectToPredicate object)
            DateQuanThe _ _ -> And
                            (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                            (objectToPredicate object)

    -- Generate FOL formulas based on different cases
    generateFormulas basePredicates actualValue

    where
        -- Function to generate FOL formulas based on different cases
        generateFormulas basePredicates actualValue =
            case (holds, verbStatus, temporalQuantifier) of
                (HoldYes, VSDel, TempBefore) -> yesDeliveredBefore
                (HoldNo, VSDel, TempBefore) -> noDeliveredBefore
                (HoldYes, VSPay, TempBefore) -> yesPaidBefore
                (HoldNo, VSPay, TempBefore) -> noPaidBefore
                (HoldYes, VSCharge, TempBefore) -> yesChargedBefore
                (HoldNo, VSCharge, TempBefore) -> noChargedBefore
                (HoldYes, VSRefund, TempBefore) -> yesRefundedBefore
                (HoldNo, VSRefund, TempBefore) -> noRefundedBefore
                (HoldYes, VSDel, TempAfter) -> yesDeliveredAfter
                (HoldNo, VSDel, TempAfter) -> noDeliveredAfter
                (HoldYes, VSPay, TempAfter) -> yesPaidAfter
                (HoldNo, VSPay, TempAfter) -> noPaidAfter
                (HoldYes, VSCharge, TempAfter) -> yesChargedAfter
                (HoldNo, VSCharge, TempAfter) -> noChargedAfter
                (HoldYes, VSRefund, TempAfter) -> yesRefundedAfter
                (HoldNo, VSRefund, TempAfter) -> noRefundedAfter

            where
                -- Predicates for different cases
                yesDeliveredBefore = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "deliveredBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                noDeliveredBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "deliveredBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                yesPaidBefore = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "paidBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                noPaidBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "paidBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                yesChargedBefore = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "chargedBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                noChargedBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "chargedBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                yesRefundedBefore = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "refundedBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                noRefundedBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "refundedBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                yesDeliveredAfter = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "deliveredAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                noDeliveredAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "deliveredAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                yesPaidAfter = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "paidAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                noPaidAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "paidAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                yesChargedAfter = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "chargedAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                noChargedAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "chargedAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                yesRefundedAfter = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "refundedAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                noRefundedAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "refundedAfter" [Var "X", Var "Y", Var "O", Var actualValue])

createFormulaSimpleConditionDQuanTempSomeThe :: Holds -> VerbStatus -> TemporalQuantifier -> Subject -> Object -> Receiver -> TemporalOffset -> TemporalQuantifier -> Subject -> Date -> State (DateDictionary, TempQuanDictionary) FOLFormula
createFormulaSimpleConditionDQuanTempSomeThe holds verbStatus temporalQuantifier subject object receiver temporalOffset tq date dateType = do
    -- Get the actual value associated with the date
    actualValue <- lookupDate (subjectToString date)

    -- Use actualValue here
    let basePredicates = case (dateType, tq) of
            (DateQuanTempSome _ _ _ _, TempBefore) -> And
                            (And
                                (And
                                    (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                    (Pred "isDate" [Var actualValue])
                                )
                                (Pred "dateBefore" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)
            (DateQuanTempSome _ _ _ _, TempAfter) -> And
                            (And
                                (And
                                    (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                    (Pred "isDate" [Var actualValue])
                                )
                                (Pred "dateAfter" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)
            (DateQuanTempThe _ _ _ _, TempBefore) -> And
                            (And
                                (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                (Pred "dateBefore" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)
            (DateQuanTempThe _ _ _ _, TempAfter) -> And
                            (And
                                (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                (Pred "dateAfter" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)

    -- Generate FOL formulas based on different cases
    generateFormulas basePredicates actualValue

    where
        -- Function to generate FOL formulas based on different cases
        generateFormulas basePredicates actualValue = 
            case (holds, verbStatus, temporalQuantifier) of
                (HoldYes, VSDel, TempBefore) -> yesDeliveredBefore
                (HoldNo, VSDel, TempBefore) -> noDeliveredBefore
                (HoldYes, VSPay, TempBefore) -> yesPaidBefore
                (HoldNo, VSPay, TempBefore) -> noPaidBefore
                (HoldYes, VSCharge, TempBefore) -> yesChargedBefore
                (HoldNo, VSCharge, TempBefore) -> noChargedBefore
                (HoldYes, VSRefund, TempBefore) -> yesRefundedBefore
                (HoldNo, VSRefund, TempBefore) -> noRefundedBefore
                (HoldYes, VSDel, TempAfter) -> yesDeliveredAfter
                (HoldNo, VSDel, TempAfter) -> noDeliveredAfter
                (HoldYes, VSPay, TempAfter) -> yesPaidAfter
                (HoldNo, VSPay, TempAfter) -> noPaidAfter
                (HoldYes, VSCharge, TempAfter) -> yesChargedAfter
                (HoldNo, VSCharge, TempAfter) -> noChargedAfter
                (HoldYes, VSRefund, TempAfter) -> yesRefundedAfter
                (HoldNo, VSRefund, TempAfter) -> noRefundedAfter

            where
                -- Predicates for different cases
                yesDeliveredBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "deliveredBefore" [Var "X", Var "Y", Var "O", Var "D"])
                noDeliveredBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "deliveredBefore" [Var "X", Var "Y", Var "O", Var "D"])
                yesPaidBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "paidBefore" [Var "X", Var "Y", Var "O", Var "D"])
                noPaidBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "paidBefore" [Var "X", Var "Y", Var "O", Var "D"])
                yesChargedBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "chargedBefore" [Var "X", Var "Y", Var "O", Var "D"])
                noChargedBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "chargedBefore" [Var "X", Var "Y", Var "O", Var "D"])
                yesRefundedBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "refundedBefore" [Var "X", Var "Y", Var "O", Var "D"])
                noRefundedBefore = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "refundedBefore" [Var "X", Var "Y", Var "O", Var "D"])
                yesDeliveredAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "deliveredAfter" [Var "X", Var "Y", Var "O", Var "D"])
                noDeliveredAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "deliveredAfter" [Var "X", Var "Y", Var "O", Var "D"])
                yesPaidAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "paidAfter" [Var "X", Var "Y", Var "O", Var "D"])
                noPaidAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "paidAfter" [Var "X", Var "Y", Var "O", Var "D"])
                yesChargedAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "chargedAfter" [Var "X", Var "Y", Var "O", Var "D"])
                noChargedAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "chargedAfter" [Var "X", Var "Y", Var "O", Var "D"])
                yesRefundedAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "refundedAfter" [Var "X", Var "Y", Var "O", Var "D"])
                noRefundedAfter = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "refundedAfter" [Var "X", Var "Y", Var "O", Var "D"])

createFormulaSimpleConditionDQuanSomeTheWO :: Holds -> VerbStatus -> Subject -> Object -> Receiver -> TemporalOffset -> TemporalQuantifier -> Subject -> Date -> State (DateDictionary, TempQuanDictionary) FOLFormula
createFormulaSimpleConditionDQuanSomeTheWO holds verbStatus subject object receiver temporalOffset tq date dateType = do
    -- Get the actual value associated with the date
    actualValue <- lookupDate (subjectToString date)

    -- Use actualValue here
    let basePredicates = case (dateType, tq) of
            (DateQuanSomeWO _ _ _, TempBefore) -> And
                            (And
                                (And
                                    (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                    (Pred "isDate" [Var actualValue])
                                )
                                (Pred "dateBefore" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)
            (DateQuanSomeWO _ _ _, TempAfter) -> And
                            (And
                                (And
                                    (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                    (Pred "isDate" [Var actualValue])
                                )
                                (Pred "dateAfter" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)
            (DateQuanTheWO _ _ _, TempBefore) -> And
                            (And
                                (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                (Pred "dateBefore" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)
            (DateQuanTheWO _ _ _, TempAfter) -> And
                            (And
                                (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                (Pred "dateAfter" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)

    -- Generate FOL formulas based on different cases
    generateFormulas basePredicates actualValue

    where
        -- Function to generate FOL formulas based on different cases
        generateFormulas basePredicates actualValue = 
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
                -- Predicates for different cases
                yesDelivered = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "delivered" [Var "X", Var "Y", Var "O", Var "D"])
                noDelivered = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "delivered" [Var "X", Var "Y", Var "O", Var "D"])
                yesPaid = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "paid" [Var "X", Var "Y", Var "O", Var "D"])
                noPaid = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "paid" [Var "X", Var "Y", Var "O", Var "D"])
                yesCharged = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "charged" [Var "X", Var "Y", Var "O", Var "D"])
                noCharged = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "charged" [Var "X", Var "Y", Var "O", Var "D"])
                yesRefunded = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "refunded" [Var "X", Var "Y", Var "O", Var "D"])
                noRefunded = return $ Not $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "refunded" [Var "X", Var "Y", Var "O", Var "D"])

createFormulaSimpleConditionNH :: VerbStatus -> Subject -> Object -> Receiver -> Num -> Month -> Num -> State (DateDictionary, TempQuanDictionary) FOLFormula
createFormulaSimpleConditionNH verbStatus subject object receiver day month year =
    case verbStatus of
        VSDel -> yesDelivered
        VSPay -> yesPaid
        VSCharge -> yesCharged
        VSRefund -> yesRefunded

    where
        -- Common base predicates
        basePredicates =
                And
                    (And
                        (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                        (Pred "date" [Var "D", dateSpeToTerm year month day])
                    )
                    (objectToPredicate object)
                
        -- Predicates for different cases
        yesDelivered = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "delivered" [Var "X", Var "Y", Var "O", Var "D"])
        yesPaid = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "paid" [Var "X", Var "Y", Var "O", Var "D"])
        yesCharged = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "charged" [Var "X", Var "Y", Var "O", Var "D"])
        yesRefunded = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "refunded" [Var "X", Var "Y", Var "O", Var "D"])

createFormulaSimpleConditionNHDAny :: VerbStatus -> Subject -> Object -> Receiver -> State (DateDictionary, TempQuanDictionary) FOLFormula
createFormulaSimpleConditionNHDAny verbStatus subject object receiver =
    case verbStatus of
        VSDel -> yesDelivered
        VSPay -> yesPaid
        VSCharge -> yesCharged
        VSRefund -> yesRefunded

    where
        -- Common base predicates
        basePredicates =
                    And
                        (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                        (objectToPredicate object)
                
        -- Predicates for different cases
        yesDelivered = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "delivered" [Var "X", Var "Y", Var "O", Var "D"])
        yesPaid = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "paid" [Var "X", Var "Y", Var "O", Var "D"])
        yesCharged = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "charged" [Var "X", Var "Y", Var "O", Var "D"])
        yesRefunded = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ Exists [Var "D"] $ Brackets $ And basePredicates (Pred "refunded" [Var "X", Var "Y", Var "O", Var "D"])

createFormulaSimpleConditionNHDSomeThe :: VerbStatus -> Subject -> Object -> Receiver -> Subject -> Date -> State (DateDictionary, TempQuanDictionary) FOLFormula
createFormulaSimpleConditionNHDSomeThe verbStatus subject object receiver date dateType = do
    -- Get the actual value associated with the date
    actualValue <- lookupDate (subjectToString date)

    -- Use actualValue here
    let basePredicates = case dateType of
            DateSome _ -> And
                            (And
                                (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                (Pred "isDate" [Var actualValue])
                            )
                            (objectToPredicate object)
            DateThe _ -> And
                            (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                            (objectToPredicate object)


    -- Generate FOL formulas based on different cases
    generateFormulas basePredicates actualValue

    where
        -- Function to generate FOL formulas based on different cases
        generateFormulas basePredicates actualValue = 
            case verbStatus of
                VSDel -> yesDelivered
                VSPay -> yesPaid
                VSCharge -> yesCharged
                VSRefund -> yesRefunded

            where
                -- Predicates for different cases
                yesDelivered = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "delivered" [Var "X", Var "Y", Var "O", Var actualValue])
                yesPaid = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "paid" [Var "X", Var "Y", Var "O", Var actualValue])
                yesCharged = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "charged" [Var "X", Var "Y", Var "O", Var actualValue])
                yesRefunded = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "refunded" [Var "X", Var "Y", Var "O", Var actualValue])

createFormulaSimpleConditionNHDQuanSpe :: VerbStatus -> TemporalQuantifier -> Subject -> Object -> Receiver -> Num -> Month -> Num -> State (DateDictionary, TempQuanDictionary) FOLFormula
createFormulaSimpleConditionNHDQuanSpe verbStatus temporalQuantifier subject object receiver day month year =
    case (verbStatus, temporalQuantifier) of
        (VSDel, TempBefore) -> yesDeliveredBefore
        (VSPay, TempBefore) -> yesPaidBefore
        (VSCharge, TempBefore) -> yesChargedBefore
        (VSRefund, TempBefore) -> yesRefundedBefore
        (VSDel, TempAfter) -> yesDeliveredAfter
        (VSPay, TempAfter) -> yesPaidAfter
        (VSCharge, TempAfter) -> yesChargedAfter
        (VSRefund, TempAfter) -> yesRefundedAfter

    where
        -- Common base predicates
        basePredicates =
            And
                (And
                    (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                    (Pred "date" [Var "D", dateSpeToTerm year month day])
                )
                (objectToPredicate object)
                
        -- Predicates for different cases
        yesDeliveredBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "deliveredBefore" [Var "X", Var "Y", Var "O", Var "D"])
        yesPaidBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "paidBefore" [Var "X", Var "Y", Var "O", Var "D"])
        yesChargedBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "chargedBefore" [Var "X", Var "Y", Var "O", Var "D"])
        yesRefundedBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "refundedBefore" [Var "X", Var "Y", Var "O", Var "D"])
        yesDeliveredAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "deliveredAfter" [Var "X", Var "Y", Var "O", Var "D"])
        yesPaidAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "paidAfter" [Var "X", Var "Y", Var "O", Var "D"])
        yesChargedAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "chargedAfter" [Var "X", Var "Y", Var "O", Var "D"])
        yesRefundedAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "refundedAfter" [Var "X", Var "Y", Var "O", Var "D"])

createFormulaSimpleConditionNHDQuanSomeThe :: VerbStatus -> TemporalQuantifier -> Subject -> Object -> Receiver -> Subject -> Date -> State (DateDictionary, TempQuanDictionary) FOLFormula
createFormulaSimpleConditionNHDQuanSomeThe verbStatus temporalQuantifier subject object receiver date dateType = do
    -- Get the actual value associated with the date
    actualValue <- lookupDate (subjectToString date)

    -- Use actualValue here
    let basePredicates = case dateType of
            DateQuanSome _ _ -> And
                            (And
                                (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                (Pred "isDate" [Var actualValue])
                            )
                            (objectToPredicate object)
            DateQuanThe _ _ -> And
                            (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                            (objectToPredicate object)

    -- Generate FOL formulas based on different cases
    generateFormulas basePredicates actualValue 

    where
        -- Function to generate FOL formulas based on different cases
        generateFormulas basePredicates actualValue = 
            case (verbStatus, temporalQuantifier) of
                (VSDel, TempBefore) -> yesDeliveredBefore
                (VSPay, TempBefore) -> yesPaidBefore
                (VSCharge, TempBefore) -> yesChargedBefore
                (VSRefund, TempBefore) -> yesRefundedBefore
                (VSDel, TempAfter) -> yesDeliveredAfter
                (VSPay, TempAfter) -> yesPaidAfter
                (VSCharge, TempAfter) -> yesChargedAfter
                (VSRefund, TempAfter) -> yesRefundedAfter

            where
                -- Predicates for different cases
                yesDeliveredBefore = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "deliveredBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                yesPaidBefore = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "paidBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                yesChargedBefore = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "chargedBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                yesRefundedBefore = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "refundedBefore" [Var "X", Var "Y", Var "O", Var actualValue])
                yesDeliveredAfter = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "deliveredAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                yesPaidAfter = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "paidAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                yesChargedAfter = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "chargedAfter" [Var "X", Var "Y", Var "O", Var actualValue])
                yesRefundedAfter = return $ ForAll [Var "X", Var "Y", Var "O"] $ Brackets $ And basePredicates (Pred "refundedAfter" [Var "X", Var "Y", Var "O", Var actualValue])

createFormulaSimpleConditionNHDQuanTempSomeThe :: VerbStatus -> TemporalQuantifier -> Subject -> Object -> Receiver -> TemporalOffset -> TemporalQuantifier -> Subject -> Date -> State (DateDictionary, TempQuanDictionary) FOLFormula
createFormulaSimpleConditionNHDQuanTempSomeThe verbStatus temporalQuantifier subject object receiver temporalOffset tq date dateType = do
    -- Get the actual value associated with the date
    actualValue <- lookupDate (subjectToString date)

    -- Use actualValue here
    let basePredicates = case (dateType, tq) of
            (DateQuanTempSome _ _ _ _, TempBefore) -> And
                            (And
                                (And
                                    (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                    (Pred "isDate" [Var actualValue])
                                )
                                (Pred "dateBefore" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)
            (DateQuanTempSome _ _ _ _, TempAfter) -> And
                            (And
                                (And
                                    (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                    (Pred "isDate" [Var actualValue])
                                )
                                (Pred "dateAfter" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)
            (DateQuanTempThe _ _ _ _, TempBefore) -> And
                            (And
                                (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                (Pred "dateBefore" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)
            (DateQuanTempThe _ _ _ _, TempAfter) -> And
                            (And
                                (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                (Pred "dateAfter" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)

    -- Generate FOL formulas based on different cases
    generateFormulas basePredicates actualValue

    where
        -- Function to generate FOL formulas based on different cases
        generateFormulas basePredicates actualValue = 
            case (verbStatus, temporalQuantifier) of
                (VSDel, TempBefore) -> yesDeliveredBefore
                (VSPay, TempBefore) -> yesPaidBefore
                (VSCharge, TempBefore) -> yesChargedBefore
                (VSRefund, TempBefore) -> yesRefundedBefore
                (VSDel, TempAfter) -> yesDeliveredAfter
                (VSPay, TempAfter) -> yesPaidAfter
                (VSCharge, TempAfter) -> yesChargedAfter
                (VSRefund, TempAfter) -> yesRefundedAfter

            where
                -- Predicates for different cases
                yesDeliveredBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "deliveredBefore" [Var "X", Var "Y", Var "O", Var "D"])
                yesPaidBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "paidBefore" [Var "X", Var "Y", Var "O", Var "D"])
                yesChargedBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "chargedBefore" [Var "X", Var "Y", Var "O", Var "D"])
                yesRefundedBefore = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "refundedBefore" [Var "X", Var "Y", Var "O", Var "D"])
                yesDeliveredAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "deliveredAfter" [Var "X", Var "Y", Var "O", Var "D"])
                yesPaidAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "paidAfter" [Var "X", Var "Y", Var "O", Var "D"])
                yesChargedAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "chargedAfter" [Var "X", Var "Y", Var "O", Var "D"])
                yesRefundedAfter = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "refundedAfter" [Var "X", Var "Y", Var "O", Var "D"])

createFormulaSimpleConditionNHDQuanSomeTheWO :: VerbStatus -> Subject -> Object -> Receiver -> TemporalOffset -> TemporalQuantifier -> Subject -> Date -> State (DateDictionary, TempQuanDictionary) FOLFormula
createFormulaSimpleConditionNHDQuanSomeTheWO verbStatus subject object receiver temporalOffset tq date dateType = do
    -- Get the actual value associated with the date
    actualValue <- lookupDate (subjectToString date)

    -- Use actualValue here
    let basePredicates = case (dateType, tq) of
            (DateQuanSomeWO _ _ _, TempBefore) -> And
                            (And
                                (And
                                    (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                    (Pred "isDate" [Var actualValue])
                                )
                                (Pred "dateBefore" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)
            (DateQuanSomeWO _ _ _, TempAfter) -> And
                            (And
                                (And
                                    (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                    (Pred "isDate" [Var actualValue])
                                )
                                (Pred "dateAfter" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)
            (DateQuanTheWO _ _ _, TempBefore) -> And
                            (And
                                (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                (Pred "dateBefore" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)
            (DateQuanTheWO _ _ _, TempAfter) -> And
                            (And
                                (And (Pred "name" [Var "X", subjectToTerm subject]) (Pred "name" [Var "Y", receiverToTerm receiver]))
                                (Pred "dateAfter" [Var "D", Var actualValue, temporalOffsetToTerm temporalOffset])
                            )
                            (objectToPredicate object)

    -- Generate FOL formulas based on different cases
    generateFormulas basePredicates actualValue

    where
        -- Function to generate FOL formulas based on different cases
        generateFormulas basePredicates actualValue = 
            case (verbStatus) of
                (VSDel) -> yesDelivered
                (VSPay) -> yesPaid
                (VSCharge) -> yesCharged
                (VSRefund) -> yesRefunded

            where
                -- Predicates for different cases
                yesDelivered = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "delivered" [Var "X", Var "Y", Var "O", Var "D"])
                yesPaid = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "paid" [Var "X", Var "Y", Var "O", Var "D"])
                yesCharged = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "charged" [Var "X", Var "Y", Var "O", Var "D"])
                yesRefunded = return $ ForAll [Var "X", Var "Y", Var "O", Var "D"] $ Brackets $ And basePredicates (Pred "refunded" [Var "X", Var "Y", Var "O", Var "D"])
                
boolExToFOL :: BooleanExpression -> State (DateDictionary, TempQuanDictionary) FOLFormula
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
        basePredicates = (And (Pred "name" [Var "X", subjectToTerm subject1]) (Pred "name" [Var "Y", subjectToTerm subject2]))
                          
        -- Predicates for different cases
        deliveredLess = return $ ForAll [Var "X", Var "Y"] $ Brackets $ And basePredicates (Pred "deliveredLess" [Var "X", Var "Y"])
        deliveredEqual = return $ ForAll [Var "X", Var "Y"] $ Brackets $ And basePredicates (Pred "deliveredEqual" [Var "X", Var "Y"])
        deliveredMore = return $ ForAll [Var "X", Var "Y"] $ Brackets $ And basePredicates (Pred "deliveredMore" [Var "X", Var "Y"])
        paidLess = return $ ForAll [Var "X", Var "Y"] $ Brackets $ And basePredicates (Pred "paidLess" [Var "X", Var "Y"])
        paidEqual = return $ ForAll [Var "X", Var "Y"] $ Brackets $ And basePredicates (Pred "paidEqual" [Var "X", Var "Y"])
        paidMore = return $ ForAll [Var "X", Var "Y"] $ Brackets $ And basePredicates (Pred "paidMore" [Var "X", Var "Y"])
        chargedLess = return $ ForAll [Var "X", Var "Y"] $ Brackets $ And basePredicates (Pred "chargedLess" [Var "X", Var "Y"])
        chargedEqual = return $ ForAll [Var "X", Var "Y"] $ Brackets $ And basePredicates (Pred "chargedEqual" [Var "X", Var "Y"])
        chargedMore = return $ ForAll [Var "X", Var "Y"] $ Brackets $ And basePredicates (Pred "chargedMore" [Var "X", Var "Y"])
        refundedLess = return $ ForAll [Var "X", Var "Y"] $ Brackets $ And basePredicates (Pred "refundedLess" [Var "X", Var "Y"])
        refundedEqual = return $ ForAll [Var "X", Var "Y"] $ Brackets $ And basePredicates (Pred "refundedEqual" [Var "X", Var "Y"])
        refundedMore = return $ ForAll [Var "X", Var "Y"] $ Brackets $ And basePredicates (Pred "refundedMore" [Var "X", Var "Y"])

-- Function to run contractToFOLWithCheck with two empty dictionaries
runFOLConversion :: Contract -> FOLFormula
runFOLConversion contract = evalState (contractToFOLWithCheck contract) (Map.empty, Map.empty)

-- Function to run contractToFOLWithCheck with two empty dictionaries
runFOLConversion' :: Contract -> (FOLFormula, DateDictionary, TempQuanDictionary)
runFOLConversion' contract = (result, newDateDict, newTempQuanDict)
  where
    -- Run the conversion and get the result and the updated state
    (result, (newDateDict, newTempQuanDict)) = runState (contractToFOLWithCheck contract) (Map.empty, Map.empty)

-- Updated function to convert the contract to FOL formula with date dictionary check
contractToFOLWithCheck :: Contract -> State (DateDictionary, TempQuanDictionary) FOLFormula
contractToFOLWithCheck contract = do
    folFormula <- contractToFOL contract
    addDateDictionaryCheck folFormula

-- Function to add date dictionary check to the FOL formula
addDateDictionaryCheck :: FOLFormula -> State (DateDictionary, TempQuanDictionary) FOLFormula
addDateDictionaryCheck folFormula = do
    dateDict <- gets fst
    let dateVars = Map.elems (Map.map Var dateDict)
    return $ addDateDictionaryCheck' folFormula dateVars

-- Helper function to apply ForAll quantification
addDateDictionaryCheck' :: FOLFormula -> [Term] -> FOLFormula
addDateDictionaryCheck' folFormula dateVars =
    case dateVars of
        [] -> folFormula
        _ -> ForAll dateVars $ Brackets $ folFormula

-- Function to add temp quan dictionary check to the FOL formula
-- addDateDictionaryCheck :: FOLFormula -> State (DateDictionary, TempQuanDictionary) FOLFormula
-- addDateDictionaryCheck folFormula = do
--     dateDict <- gets snd
--     let dateVars = Map.elems (Map.mapWithKey (\key (_, intValue) -> Var key) dateDict)
--     return $ addDateDictionaryCheck' folFormula dateVars