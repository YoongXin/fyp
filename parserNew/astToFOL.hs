module AstToFOL where

import Prelude
  ( ($), (.), (<$>)
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
  , Maybe(..)
  , (+)
  )

import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad      ( when )
import Data.Time
import Control.Monad.State

import AbsCoLa   
import LexCoLa   ( Token, mkPosToken )
import ParCoLa   ( pContract, myLexer )
import PrintCoLa ( Print, printTree )
import SkelCoLa  ()
import qualified Data.Map as Map

type DateDictionary = Map.Map String String

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

  deriving (Eq, Show, Read)

-- Function to convert a specific date to an integer
dateToInt :: Integer -> Int -> Int -> Integer
dateToInt year month day = toModifiedJulianDay (fromGregorian year month day)

-- Function to convert the contract to FOL formula
contractToFOL :: Contract -> State DateDictionary FOLFormula
contractToFOL (ConEmpty) = return $ Brackets $ Pred "empty" [Var "empty"]
contractToFOL (ConComp component) = componentToFOL component 
contractToFOL (ConAnd component contract) = do
    c1 <- Brackets <$> componentToFOL component
    c2 <- Brackets <$> contractToFOL contract
    return $ And c1 c2

componentToFOL :: Component -> State DateDictionary FOLFormula
componentToFOL (ComDef definition) = definitionToFOL definition
componentToFOL (ComConDef conditionalDefinition) = conditionalDefinitionToFOL conditionalDefinition
componentToFOL (ComState statement) = statementToFOL statement
componentToFOL (ComConState conditionalStatement) = conditionalStatementToFOL conditionalStatement

definitionToFOL :: Definition -> State DateDictionary FOLFormula
definitionToFOL (DefSim simpleDefinition) = simpleDefinitionToFOL simpleDefinition
definitionToFOL (DefAnd simpleDefinition definition) = liftM2 And (simpleDefinitionToFOL simpleDefinition) (definitionToFOL definition)

conditionalDefinitionToFOL :: ConditionalDefinition -> State DateDictionary FOLFormula
conditionalDefinitionToFOL (ConDefIf definition condition) = liftM2 Implies (conditionToFOL condition) (definitionToFOL definition)
conditionalDefinitionToFOL (ConDefIfThen condition definition) = liftM2 Implies (conditionToFOL condition) (definitionToFOL definition)

statementToFOL :: Statement -> State DateDictionary FOLFormula
statementToFOL (StateSim simpleStatement) = simpleStatementToFOL simpleStatement
statementToFOL (StateOr simpleStatement statement) = liftM2 Or (simpleStatementToFOL simpleStatement) (statementToFOL statement)
statementToFOL (StateAnd simpleStatement statement) = liftM2 And (simpleStatementToFOL simpleStatement) (statementToFOL statement)

conditionalStatementToFOL :: ConditionalStatement -> State DateDictionary FOLFormula
conditionalStatementToFOL (ConStateIf statement condition) = liftM2 Implies (conditionToFOL condition) (statementToFOL statement)
conditionalStatementToFOL (ConStateIfThen condition statement) = liftM2 Implies (conditionToFOL condition) (statementToFOL statement)

simpleDefinitionToFOL :: SimpleDefinition -> State DateDictionary FOLFormula
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
numericalObjectToTerm (NumPound _ (NumInt n)) = Var ("Pound" ++ show n)
numericalObjectToTerm (NumDol _ (NumInt n)) = Var ("Dollar" ++ show n)
numericalObjectToTerm (NumEur _ (NumInt n)) = Var ("Euro" ++ show n)
numericalObjectToTerm (NumAmount subject) = subjectToTerm subject

conditionToFOL :: Condition -> State DateDictionary FOLFormula
conditionToFOL (CondiSim simpleCondition) = simpleConditionToFOL simpleCondition
conditionToFOL (CondiOr simpleCondition condition) = liftM2 Or (simpleConditionToFOL simpleCondition) (conditionToFOL condition)
conditionToFOL (CondiAnd simpleCondition condition) = liftM2 And (simpleConditionToFOL simpleCondition) (conditionToFOL condition)

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

dateSpeToTerm :: Num -> Month -> Num -> Term
dateSpeToTerm (NumInt year) month (NumInt day) = Var $ show $ dateToInt year (monthToInt month) (fromInteger day)

lookupDate :: String -> State DateDictionary String
lookupDate date = do
    -- Get the current state of the date dictionary
    dateDictionary <- get

    -- Check if the date is already in the dictionary
    case Map.lookup date dateDictionary of
        Just value -> return value
        Nothing -> do
            -- If it doesn't exist, add a new entry to the dictionary
            let newValue = "d" ++ show (Map.size dateDictionary + 1)
            let newDateDict = Map.insert date newValue dateDictionary
            put newDateDict
            return newValue

simpleStatementToFOL :: SimpleStatement -> State DateDictionary FOLFormula
simpleStatementToFOL (SimStateOne id holds subject modalVerb verb object receiver (DateSpe day month year)) =
    createFormulaSimpleStatement holds modalVerb verb subject object receiver day month year
simpleStatementToFOL (SimStateOne id holds subject modalVerb verb object receiver DateAny) =
    createFormulaSimpleStatementDAny holds modalVerb verb subject object receiver 
simpleStatementToFOL (SimStateOne id holds subject modalVerb verb object receiver (DateSome date)) =
    createFormulaSimpleStatementDSome holds modalVerb verb subject object receiver date

simpleStatementToFOL (SimStateTwo id holds subject (DateSpe day month year) modalVerb verb object receiver) =
    createFormulaSimpleStatement holds modalVerb verb subject object receiver day month year
simpleStatementToFOL (SimStateTwo id holds subject DateAny modalVerb verb object receiver) =
    createFormulaSimpleStatementDAny holds modalVerb verb subject object receiver 
simpleStatementToFOL (SimStateTwo id holds subject (DateSome date) modalVerb verb object receiver) =
    createFormulaSimpleStatementDSome holds modalVerb verb subject object receiver date

simpleStatementToFOL (SimStateThree id holds (DateSpe day month year) subject modalVerb verb object receiver) =
    createFormulaSimpleStatement holds modalVerb verb subject object receiver day month year
simpleStatementToFOL (SimStateThree id holds DateAny subject modalVerb verb object receiver) =
    createFormulaSimpleStatementDAny holds modalVerb verb subject object receiver
simpleStatementToFOL (SimStateThree id holds (DateSome date) subject modalVerb verb object receiver) =
    createFormulaSimpleStatementDSome holds modalVerb verb subject object receiver date

simpleStatementToFOL (SimStateOneNH id subject modalVerb verb object receiver (DateSpe day month year)) =
    createFormulaSimpleStatementNH modalVerb verb subject object receiver day month year
simpleStatementToFOL (SimStateOneNH id subject modalVerb verb object receiver DateAny) =
    createFormulaSimpleStatementNHDAny modalVerb verb subject object receiver
simpleStatementToFOL (SimStateOneNH id subject modalVerb verb object receiver (DateSome date)) =
    createFormulaSimpleStatementNHDSome modalVerb verb subject object receiver date

simpleStatementToFOL (SimStateTwoNH id subject (DateSpe day month year) modalVerb verb object receiver) =
    createFormulaSimpleStatementNH modalVerb verb subject object receiver day month year
simpleStatementToFOL (SimStateTwoNH id subject DateAny modalVerb verb object receiver) =
    createFormulaSimpleStatementNHDAny modalVerb verb subject object receiver
simpleStatementToFOL (SimStateTwoNH id subject (DateSome date) modalVerb verb object receiver) =
    createFormulaSimpleStatementNHDSome modalVerb verb subject object receiver date

simpleStatementToFOL (SimStateThreeNH id (DateSpe day month year) subject modalVerb verb object receiver) =
    createFormulaSimpleStatementNH modalVerb verb subject object receiver day month year
simpleStatementToFOL (SimStateThreeNH id DateAny subject modalVerb verb object receiver) =
    createFormulaSimpleStatementNHDAny modalVerb verb subject object receiver
simpleStatementToFOL (SimStateThreeNH id (DateSome date) subject modalVerb verb object receiver) =
    createFormulaSimpleStatementNHDSome modalVerb verb subject object receiver date

createFormulaSimpleStatement :: Holds -> ModalVerb -> Verb -> Subject -> Object -> Receiver -> Num -> Month -> Num -> State DateDictionary FOLFormula
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
        yesMustDel = return $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "MustDeliver" [Var "x", Var "y", Var "o", Var "d"])
        noMustDel = return $ Not $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "MustDeliver" [Var "x", Var "y", Var "o", Var "d"])
        yesMayDel = return $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "MayDeliver" [Var "x", Var "y", Var "o", Var "d"])
        noMayDel = return $ Not $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "MayDeliver" [Var "x", Var "y", Var "o", Var "d"])
        yesMustPay = return $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "MustPay" [Var "x", Var "y", Var "o", Var "d"])
        noMustPay = return $ Not $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "MustPay" [Var "x", Var "y", Var "o", Var "d"])
        yesMayPay = return $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "MayPay" [Var "x", Var "y", Var "o", Var "d"])
        noMayPay = return $ Not $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "MayPay" [Var "x", Var "y", Var "o", Var "d"])
        yesMustCharge = return $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "MustCharge" [Var "x", Var "y", Var "o", Var "d"])
        noMustCharge = return $ Not $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "MustCharge" [Var "x", Var "y", Var "o", Var "d"])
        yesMayCharge = return $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "MayCharge" [Var "x", Var "y", Var "o", Var "d"])
        noMayCharge = return $ Not $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "MayCharge" [Var "x", Var "y", Var "o", Var "d"])
        yesMustRefund = return $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "MustRefund" [Var "x", Var "y", Var "o", Var "d"])
        noMustRefund = return $ Not $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "MustRefund" [Var "x", Var "y", Var "o", Var "d"])
        yesMayRefund = return $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "MayRefund" [Var "x", Var "y", Var "o", Var "d"])
        noMayRefund = return $ Not $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "MayRefund" [Var "x", Var "y", Var "o", Var "d"])

createFormulaSimpleStatementDAny :: Holds -> ModalVerb -> Verb -> Subject -> Object -> Receiver -> State DateDictionary FOLFormula
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
                (And (Pred "Name" [Var "x", subjectToTerm subject]) (Pred "Name" [Var "y", receiverToTerm receiver]))
                (objectToPredicate object)
                
        -- Predicates for different cases
        yesMustDel = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "MustDeliver" [Var "x", Var "y", Var "o", Var "d"])
        noMustDel = return $ Not $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "MustDeliver" [Var "x", Var "y", Var "o", Var "d"])
        yesMayDel = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "MayDeliver" [Var "x", Var "y", Var "o", Var "d"])
        noMayDel = return $ Not $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "MayDeliver" [Var "x", Var "y", Var "o", Var "d"])
        yesMustPay = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "MustPay" [Var "x", Var "y", Var "o", Var "d"])
        noMustPay = return $ Not $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "MustPay" [Var "x", Var "y", Var "o", Var "d"])
        yesMayPay = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "MayPay" [Var "x", Var "y", Var "o", Var "d"])
        noMayPay = return $ Not $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "MayPay" [Var "x", Var "y", Var "o", Var "d"])
        yesMustCharge = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "MustCharge" [Var "x", Var "y", Var "o", Var "d"])
        noMustCharge = return $ Not $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "MustCharge" [Var "x", Var "y", Var "o", Var "d"])
        yesMayCharge = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "MayCharge" [Var "x", Var "y", Var "o", Var "d"])
        noMayCharge = return $ Not $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "MayCharge" [Var "x", Var "y", Var "o", Var "d"])
        yesMustRefund = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "MustRefund" [Var "x", Var "y", Var "o", Var "d"])
        noMustRefund = return $ Not $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "MustRefund" [Var "x", Var "y", Var "o", Var "d"])
        yesMayRefund = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "MayRefund" [Var "x", Var "y", Var "o", Var "d"])
        noMayRefund = return $ Not $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "MayRefund" [Var "x", Var "y", Var "o", Var "d"])

createFormulaSimpleStatementDSome :: Holds -> ModalVerb -> Verb -> Subject -> Object -> Receiver -> Subject -> State DateDictionary FOLFormula
createFormulaSimpleStatementDSome holds modalVerb verb subject object receiver date = do
    -- Get the actual value associated with the date
    actualValue <- lookupDate (subjectToString date)

    -- Use actualValue here
    let basePredicates = 
            And
                (And
                    (And (Pred "Name" [Var "x", subjectToTerm subject]) (Pred "Name" [Var "y", receiverToTerm receiver]))
                    (Pred "IsDate" [Var actualValue])
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
                yesMustDel = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "MustDeliver" [Var "x", Var "y", Var "o", Var actualValue])
                noMustDel = return $ Not $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "MustDeliver" [Var "x", Var "y", Var "o", Var actualValue])
                yesMayDel = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "MayDeliver" [Var "x", Var "y", Var "o", Var actualValue])
                noMayDel = return $ Not $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "MayDeliver" [Var "x", Var "y", Var "o", Var actualValue])
                yesMustPay = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "MustPay" [Var "x", Var "y", Var "o", Var actualValue])
                noMustPay = return $ Not $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "MustPay" [Var "x", Var "y", Var "o", Var actualValue])
                yesMayPay = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "MayPay" [Var "x", Var "y", Var "o", Var actualValue])
                noMayPay = return $ Not $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "MayPay" [Var "x", Var "y", Var "o", Var actualValue])
                yesMustCharge = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "MustCharge" [Var "x", Var "y", Var "o", Var actualValue])
                noMustCharge = return $ Not $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "MustCharge" [Var "x", Var "y", Var "o", Var actualValue])
                yesMayCharge = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "MayCharge" [Var "x", Var "y", Var "o", Var actualValue])
                noMayCharge = return $ Not $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "MayCharge" [Var "x", Var "y", Var "o", Var actualValue])
                yesMustRefund = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "MustRefund" [Var "x", Var "y", Var "o", Var actualValue])
                noMustRefund = return $ Not $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "MustRefund" [Var "x", Var "y", Var "o", Var actualValue])
                yesMayRefund = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "MayRefund" [Var "x", Var "y", Var "o", Var actualValue])
                noMayRefund = return $ Not $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "MayRefund" [Var "x", Var "y", Var "o", Var actualValue])

createFormulaSimpleStatementNH :: ModalVerb -> Verb -> Subject -> Object -> Receiver -> Num -> Month -> Num -> State DateDictionary FOLFormula
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
        yesMustDel = return $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "MustDeliver" [Var "x", Var "y", Var "o", Var "d"])
        yesMayDel = return $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "MayDeliver" [Var "x", Var "y", Var "o", Var "d"])
        noMayDel = return $ Not $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "MayDeliver" [Var "x", Var "y", Var "o", Var "d"])
        yesMustPay = return $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "MustPay" [Var "x", Var "y", Var "o", Var "d"])
        yesMayPay = return $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "MayPay" [Var "x", Var "y", Var "o", Var "d"])
        noMayPay = return $ Not $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "MayPay" [Var "x", Var "y", Var "o", Var "d"])
        yesMustCharge = return $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "MustCharge" [Var "x", Var "y", Var "o", Var "d"])
        yesMayCharge = return $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "MayCharge" [Var "x", Var "y", Var "o", Var "d"])
        noMayCharge = return $ Not $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "MayCharge" [Var "x", Var "y", Var "o", Var "d"])
        yesMustRefund = return $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "MustRefund" [Var "x", Var "y", Var "o", Var "d"])
        yesMayRefund = return $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "MayRefund" [Var "x", Var "y", Var "o", Var "d"])
        noMayRefund = return $ Not $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "MayRefund" [Var "x", Var "y", Var "o", Var "d"])

createFormulaSimpleStatementNHDAny :: ModalVerb -> Verb -> Subject -> Object -> Receiver -> State DateDictionary FOLFormula
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
                (And (Pred "Name" [Var "x", subjectToTerm subject]) (Pred "Name" [Var "y", receiverToTerm receiver]))
                (objectToPredicate object)
                
        -- Predicates for different cases
        yesMustDel = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "MustDeliver" [Var "x", Var "y", Var "o", Var "d"])
        yesMayDel = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "MayDeliver" [Var "x", Var "y", Var "o", Var "d"])
        noMayDel = return $ Not $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "MayDeliver" [Var "x", Var "y", Var "o", Var "d"])
        yesMustPay = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "MustPay" [Var "x", Var "y", Var "o", Var "d"])
        yesMayPay = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "MayPay" [Var "x", Var "y", Var "o", Var "d"])
        noMayPay = return $ Not $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "MayPay" [Var "x", Var "y", Var "o", Var "d"])
        yesMustCharge = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "MustCharge" [Var "x", Var "y", Var "o", Var "d"])
        yesMayCharge = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "MayCharge" [Var "x", Var "y", Var "o", Var "d"])
        noMayCharge = return $ Not $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "MayCharge" [Var "x", Var "y", Var "o", Var "d"])
        yesMustRefund = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "MustRefund" [Var "x", Var "y", Var "o", Var "d"])
        yesMayRefund = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "MayRefund" [Var "x", Var "y", Var "o", Var "d"])
        noMayRefund = return $ Not $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "MayRefund" [Var "x", Var "y", Var "o", Var "d"])

createFormulaSimpleStatementNHDSome :: ModalVerb -> Verb -> Subject -> Object -> Receiver -> Subject -> State DateDictionary FOLFormula
createFormulaSimpleStatementNHDSome modalVerb verb subject object receiver date = do
    -- Get the actual value associated with the date
    actualValue <- lookupDate (subjectToString date)

    -- Use actualValue here
    let basePredicates = 
            And
                (And
                    (And (Pred "Name" [Var "x", subjectToTerm subject]) (Pred "Name" [Var "y", receiverToTerm receiver]))
                    (Pred "IsDate" [Var actualValue])
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
                yesMustDel = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "MustDeliver" [Var "x", Var "y", Var "o", Var actualValue])
                yesMayDel = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "MayDeliver" [Var "x", Var "y", Var "o", Var actualValue])
                noMayDel = return $ Not $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "MayDeliver" [Var "x", Var "y", Var "o", Var actualValue])
                yesMustPay = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "MustPay" [Var "x", Var "y", Var "o", Var actualValue])
                yesMayPay = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "MayPay" [Var "x", Var "y", Var "o", Var actualValue])
                noMayPay = return $ Not $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "MayPay" [Var "x", Var "y", Var "o", Var actualValue])
                yesMustCharge = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "MustCharge" [Var "x", Var "y", Var "o", Var actualValue])
                yesMayCharge = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "MayCharge" [Var "x", Var "y", Var "o", Var actualValue])
                noMayCharge = return $ Not $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "MayCharge" [Var "x", Var "y", Var "o", Var actualValue])
                yesMustRefund = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "MustRefund" [Var "x", Var "y", Var "o", Var actualValue])
                yesMayRefund = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "MayRefund" [Var "x", Var "y", Var "o", Var actualValue])
                noMayRefund = return $ Not $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "MayRefund" [Var "x", Var "y", Var "o", Var actualValue])

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

simpleConditionToFOL :: SimpleCondition -> State DateDictionary FOLFormula
simpleConditionToFOL (SimConOne id holds subject verbStatus object receiver (DateSpe day month year)) =
    createFormulaSimpleCondition holds verbStatus subject object receiver day month year
simpleConditionToFOL (SimConOne id holds subject verbStatus object receiver DateAny) =
    createFormulaSimpleConditionDAny holds verbStatus subject object receiver
simpleConditionToFOL (SimConOne id holds subject verbStatus object receiver (DateSome date)) =
    createFormulaSimpleConditionDSome holds verbStatus subject object receiver date

simpleConditionToFOL (SimConTwo id holds subject (DateSpe day month year) verbStatus object receiver) = 
    createFormulaSimpleCondition holds verbStatus subject object receiver day month year
simpleConditionToFOL (SimConTwo id holds subject DateAny verbStatus object receiver) = 
    createFormulaSimpleConditionDAny holds verbStatus subject object receiver
simpleConditionToFOL (SimConTwo id holds subject (DateSome date) verbStatus object receiver) = 
    createFormulaSimpleConditionDSome holds verbStatus subject object receiver date

simpleConditionToFOL (SimConThree id holds (DateSpe day month year) subject verbStatus object receiver) = 
    createFormulaSimpleCondition holds verbStatus subject object receiver day month year 
simpleConditionToFOL (SimConThree id holds DateAny subject verbStatus object receiver) = 
    createFormulaSimpleConditionDAny holds verbStatus subject object receiver
simpleConditionToFOL (SimConThree id holds (DateSome date) subject verbStatus object receiver) = 
    createFormulaSimpleConditionDSome holds verbStatus subject object receiver date

simpleConditionToFOL (SimConFour id holds subject modalVerb verb object receiver (DateSpe day month year)) =
    createFormulaSimpleStatement holds modalVerb verb subject object receiver day month year
simpleConditionToFOL (SimConFour id holds subject modalVerb verb object receiver DateAny) =
    createFormulaSimpleStatementDAny holds modalVerb verb subject object receiver
simpleConditionToFOL (SimConFour id holds subject modalVerb verb object receiver (DateSome date)) =
    createFormulaSimpleStatementDSome holds modalVerb verb subject object receiver date

simpleConditionToFOL (SimConFive id HoldYes booleanExpression) = boolExToFOL booleanExpression
simpleConditionToFOL (SimConFive id HoldNo booleanExpression) = Not <$> (boolExToFOL booleanExpression)

simpleConditionToFOL (SimConOneNH id subject verbStatus object receiver (DateSpe day month year)) =
    createFormulaSimpleConditionNH verbStatus subject object receiver day month year
simpleConditionToFOL (SimConOneNH id subject verbStatus object receiver DateAny) =
    createFormulaSimpleConditionNHDAny verbStatus subject object receiver 
simpleConditionToFOL (SimConOneNH id subject verbStatus object receiver (DateSome date)) =
    createFormulaSimpleConditionNHDSome verbStatus subject object receiver date

simpleConditionToFOL (SimConTwoNH id subject (DateSpe day month year) verbStatus object receiver) = 
    createFormulaSimpleConditionNH verbStatus subject object receiver day month year
simpleConditionToFOL (SimConTwoNH id subject DateAny verbStatus object receiver) = 
    createFormulaSimpleConditionNHDAny verbStatus subject object receiver 
simpleConditionToFOL (SimConTwoNH id subject (DateSome date) verbStatus object receiver) = 
    createFormulaSimpleConditionNHDSome verbStatus subject object receiver date

simpleConditionToFOL (SimConThreeNH id (DateSpe day month year) subject verbStatus object receiver) = 
    createFormulaSimpleConditionNH verbStatus subject object receiver day month year 
simpleConditionToFOL (SimConThreeNH id DateAny subject verbStatus object receiver) = 
    createFormulaSimpleConditionNHDAny verbStatus subject object receiver
simpleConditionToFOL (SimConThreeNH id (DateSome date) subject verbStatus object receiver) = 
    createFormulaSimpleConditionNHDSomeThe verbStatus subject object receiver date DateSome
simpleConditionToFOL (SimConThreeNH id (DateThe date) subject verbStatus object receiver) = 
    createFormulaSimpleConditionNHDSomeThe verbStatus subject object receiver date DateThe

simpleConditionToFOL (SimConFourNH id subject modalVerb verb object receiver (DateSpe day month year)) =
    createFormulaSimpleStatementNH modalVerb verb subject object receiver day month year
simpleConditionToFOL (SimConFourNH id subject modalVerb verb object receiver DateAny) =
    createFormulaSimpleStatementNHDAny modalVerb verb subject object receiver
simpleConditionToFOL (SimConFourNH id subject modalVerb verb object receiver (DateSome date)) =
    createFormulaSimpleStatementNHDSome modalVerb verb subject object receiver date

simpleConditiontoFOL (SimConFiveNH id booleanExpression) = boolExToFOL booleanExpression

createFormulaSimpleCondition :: Holds -> VerbStatus -> Subject -> Object -> Receiver -> Num -> Month -> Num -> State DateDictionary FOLFormula
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
        yesDelivered = return $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "Delivered" [Var "x", Var "y", Var "o", Var "d"])
        noDelivered = return $ Not $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "Delivered" [Var "x", Var "y", Var "o", Var "d"])
        yesPaid = return $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "Paid" [Var "x", Var "y", Var "o", Var "d"])
        noPaid = return $ Not $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "Paid" [Var "x", Var "y", Var "o", Var "d"])
        yesCharged = return $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "Charged" [Var "x", Var "y", Var "o", Var "d"])
        noCharged = return $ Not $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "Charged" [Var "x", Var "y", Var "o", Var "d"])
        yesRefunded = return $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "Refunded" [Var "x", Var "y", Var "o", Var "d"])
        noRefunded = return $ Not $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "Refunded" [Var "x", Var "y", Var "o", Var "d"])

createFormulaSimpleConditionDAny :: Holds -> VerbStatus -> Subject -> Object -> Receiver -> State DateDictionary FOLFormula
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
                (And (Pred "Name" [Var "x", subjectToTerm subject]) (Pred "Name" [Var "y", receiverToTerm receiver]))
                (objectToPredicate object)
                
        -- Predicates for different cases
        yesDelivered = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "Delivered" [Var "x", Var "y", Var "o", Var "d"])
        noDelivered = return $ Not $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "Delivered" [Var "x", Var "y", Var "o", Var "d"])
        yesPaid = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "Paid" [Var "x", Var "y", Var "o", Var "d"])
        noPaid = return $ Not $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "Paid" [Var "x", Var "y", Var "o", Var "d"])
        yesCharged = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "Charged" [Var "x", Var "y", Var "o", Var "d"])
        noCharged = return $ Not $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "Charged" [Var "x", Var "y", Var "o", Var "d"])
        yesRefunded = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "Refunded" [Var "x", Var "y", Var "o", Var "d"])
        noRefunded = return $ Not $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "Refunded" [Var "x", Var "y", Var "o", Var "d"])

createFormulaSimpleConditionDSome :: Holds -> VerbStatus -> Subject -> Object -> Receiver -> Subject -> State DateDictionary FOLFormula
createFormulaSimpleConditionDSome holds verbStatus subject object receiver date = do
    -- Get the actual value associated with the date
    actualValue <- lookupDate (subjectToString date)

    -- Use actualValue here
    let basePredicates = 
            And
                (And
                    (And (Pred "Name" [Var "x", subjectToTerm subject]) (Pred "Name" [Var "y", receiverToTerm receiver]))
                    (Pred "IsDate" [Var actualValue])
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
                yesDelivered = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "Delivered" [Var "x", Var "y", Var "o", Var actualValue])
                noDelivered = return $ Not $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "Delivered" [Var "x", Var "y", Var "o", Var actualValue])
                yesPaid = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "Paid" [Var "x", Var "y", Var "o", Var actualValue])
                noPaid = return $ Not $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "Paid" [Var "x", Var "y", Var "o", Var actualValue])
                yesCharged = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "Charged" [Var "x", Var "y", Var "o", Var actualValue])
                noCharged = return $ Not $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "Charged" [Var "x", Var "y", Var "o", Var actualValue])
                yesRefunded = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "Refunded" [Var "x", Var "y", Var "o", Var actualValue])
                noRefunded = return $ Not $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "Refunded" [Var "x", Var "y", Var "o", Var actualValue])

createFormulaSimpleConditionNH :: VerbStatus -> Subject -> Object -> Receiver -> Num -> Month -> Num -> State DateDictionary FOLFormula
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
                        (And (Pred "Name" [Var "x", subjectToTerm subject]) (Pred "Name" [Var "y", receiverToTerm receiver]))
                        (Pred "Date" [Var "d", dateSpeToTerm year month day])
                    )
                    (objectToPredicate object)
                
        -- Predicates for different cases
        yesDelivered = return $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "Delivered" [Var "x", Var "y", Var "o", Var "d"])
        yesPaid = return $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "Paid" [Var "x", Var "y", Var "o", Var "d"])
        yesCharged = return $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "Charged" [Var "x", Var "y", Var "o", Var "d"])
        yesRefunded = return $ ForAll [Var "x", Var "y", Var "d", Var "o"] $ Brackets $ And basePredicates (Pred "Refunded" [Var "x", Var "y", Var "o", Var "d"])

createFormulaSimpleConditionNHDAny :: VerbStatus -> Subject -> Object -> Receiver -> State DateDictionary FOLFormula
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
                        (And (Pred "Name" [Var "x", subjectToTerm subject]) (Pred "Name" [Var "y", receiverToTerm receiver]))
                        (objectToPredicate object)
                
        -- Predicates for different cases
        yesDelivered = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "Delivered" [Var "x", Var "y", Var "o", Var "d"])
        yesPaid = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "Paid" [Var "x", Var "y", Var "o", Var "d"])
        yesCharged = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "Charged" [Var "x", Var "y", Var "o", Var "d"])
        yesRefunded = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ Exists [Var "d"] $ Brackets $ And basePredicates (Pred "Refunded" [Var "x", Var "y", Var "o", Var "d"])

createFormulaSimpleConditionNHDSome :: VerbStatus -> Subject -> Object -> Receiver -> Date -> State DateDictionary FOLFormula
createFormulaSimpleConditionNHDSome verbStatus subject object receiver date = do
    -- Get the actual value associated with the date
    actualValue <- lookupDate (subjectToString date)

    -- Use actualValue here
    let basePredicates = 
            And
                (And
                    (And (Pred "Name" [Var "x", subjectToTerm subject]) (Pred "Name" [Var "y", receiverToTerm receiver]))
                    (Pred "IsDate" [Var actualValue])
                )
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
                yesDelivered = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "Delivered" [Var "x", Var "y", Var "o", Var actualValue])
                yesPaid = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "Paid" [Var "x", Var "y", Var "o", Var actualValue])
                yesCharged = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "Charged" [Var "x", Var "y", Var "o", Var actualValue])
                yesRefunded = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "Refunded" [Var "x", Var "y", Var "o", Var actualValue])

createFormulaSimpleConditionNHDSomeThe :: VerbStatus -> Subject -> Object -> Receiver -> Subject -> Date -> State DateDictionary FOLFormula
createFormulaSimpleConditionNHDSomeThe verbStatus subject object receiver date dateType = do
    -- Get the actual value associated with the date
    actualValue <- lookupDate (subjectToString date)

    -- Use actualValue here
    let basePredicates = case dateType of
            DateSome -> And
                            (And
                                (And (Pred "Name" [Var "x", subjectToTerm subject]) (Pred "Name" [Var "y", receiverToTerm receiver]))
                                (Pred "IsDate" [Var actualValue])
                            )
                            (objectToPredicate object)
            DateThe -> And
                            (And (Pred "Name" [Var "x", subjectToTerm subject]) (Pred "Name" [Var "y", receiverToTerm receiver]))
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
                yesDelivered = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "Delivered" [Var "x", Var "y", Var "o", Var actualValue])
                yesPaid = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "Paid" [Var "x", Var "y", Var "o", Var actualValue])
                yesCharged = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "Charged" [Var "x", Var "y", Var "o", Var actualValue])
                yesRefunded = return $ ForAll [Var "x", Var "y", Var "o"] $ Brackets $ And basePredicates (Pred "Refunded" [Var "x", Var "y", Var "o", Var actualValue])

boolExToFOL :: BooleanExpression -> State DateDictionary FOLFormula
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
        deliveredLess = return $ ForAll [Var "x", Var "y"] $ Brackets $ And basePredicates (Pred "DeliveredLess" [Var "x", Var "y"])
        deliveredEqual = return $ ForAll [Var "x", Var "y"] $ Brackets $ And basePredicates (Pred "DeliveredEqual" [Var "x", Var "y"])
        deliveredMore = return $ ForAll [Var "x", Var "y"] $ Brackets $ And basePredicates (Pred "DeliveredMore" [Var "x", Var "y"])
        paidLess = return $ ForAll [Var "x", Var "y"] $ Brackets $ And basePredicates (Pred "PaidLess" [Var "x", Var "y"])
        paidEqual = return $ ForAll [Var "x", Var "y"] $ Brackets $ And basePredicates (Pred "PaidEqual" [Var "x", Var "y"])
        paidMore = return $ ForAll [Var "x", Var "y"] $ Brackets $ And basePredicates (Pred "PaidMore" [Var "x", Var "y"])
        chargedLess = return $ ForAll [Var "x", Var "y"] $ Brackets $ And basePredicates (Pred "ChargedLess" [Var "x", Var "y"])
        chargedEqual = return $ ForAll [Var "x", Var "y"] $ Brackets $ And basePredicates (Pred "ChargedEqual" [Var "x", Var "y"])
        chargedMore = return $ ForAll [Var "x", Var "y"] $ Brackets $ And basePredicates (Pred "ChargedMore" [Var "x", Var "y"])
        refundedLess = return $ ForAll [Var "x", Var "y"] $ Brackets $ And basePredicates (Pred "RefundedLess" [Var "x", Var "y"])
        refundedEqual = return $ ForAll [Var "x", Var "y"] $ Brackets $ And basePredicates (Pred "RefundedEqual" [Var "x", Var "y"])
        refundedMore = return $ ForAll [Var "x", Var "y"] $ Brackets $ And basePredicates (Pred "RefundedMore" [Var "x", Var "y"])

-- Run contractToFOL with an empty DateDictionary
runContract :: Contract -> FOLFormula
runContract contract = evalState (contractToFOL contract) Map.empty