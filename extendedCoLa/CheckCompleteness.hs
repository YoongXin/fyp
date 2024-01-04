module CheckCompleteness where

import Prelude
  ( ($), (++)
  , Int
  , String
  , Show, show
  , Read
  , Bool(..)
  , Maybe(..)
  )

import AbsCoLa   

import qualified Data.Map as Map
import Control.Monad (liftM2)
import Control.Monad.State

data CompletenessReport
    = IncompleteItems ([SimpleCondition], [SimpleDefinition], [SimpleCondition], [SimpleStatement], BoolExDictionary)
  deriving (Read, Show)

type BoolExDictionary = Map.Map String [String]

-- next step is to complete the printing and figure out extraction from boolexdictionary
-- also please remember to test more variety case for boolexdictionary
printCompletenessReport :: CompletenessReport -> IO()
printCompletenessReport (IncompleteItems (scs, sds, sccs, sss, beDict)) = do
    putStrLn "Incomplete Conditional Definitions (If without \"else\"):\n"
    mapM_ (putStrLn . printSimpleCondition) scs
    mapM_ (putStrLn . printSimpleDefinition) sds

    putStrLn "Incomplete Conditional Statements (If without \"else\"):\n"
    mapM_ (putStrLn . printSimpleCondition) sccs
    mapM_ (putStrLn . printSimpleStatement) sss

    putStrLn "\nBoolean Expression Dictionary:"
    printBoolExDictionary beDict

printSimpleCondition :: SimpleCondition -> String
printSimpleCondition (SimConOne id holds subject verbStatus object receiver date) =
    printSimpleCondition' id holds subject verbStatus object receiver date
printSimpleCondition (SimConTwo id holds subject date verbStatus object receiver) =
    printSimpleCondition' id holds subject verbStatus object receiver date
printSimpleCondition (SimConThree id holds date subject verbStatus object receiver) =
    printSimpleCondition' id holds subject verbStatus object receiver date
printSimpleCondition (SimConFour id holds subject modalVerb verb object receiver date) =
    printSimpleStatement' id holds subject modalVerb verb object receiver date
printSimpleCondition (SimConFive id holds (BoolEx subject1 verbStatus comparison subject2)) =
    printBooleanExpression' id holds subject1 verbStatus comparison subject2
printSimpleCondition (SimConOneNH id subject verbStatus object receiver date) =
    printSimpleConditionNH' id subject verbStatus object receiver date
printSimpleCondition (SimConTwoNH id subject date verbStatus object receiver) =
    printSimpleConditionNH' id subject verbStatus object receiver date
printSimpleCondition (SimConThreeNH id date subject verbStatus object receiver) =
    printSimpleConditionNH' id subject verbStatus object receiver date
printSimpleCondition (SimConFourNH id subject modalVerb verb object receiver date) =
    printSimpleStatementNH' id subject modalVerb verb object receiver date
printSimpleCondition (SimConFiveNH id (BoolEx subject1 verbStatus comparison subject2)) =
    printBooleanExpressionNH' id subject1 verbStatus comparison subject2

printSimpleCondition' :: ID -> Holds -> Subject -> VerbStatus -> Object -> Receiver -> Date -> String
printSimpleCondition' id holds subject verbStatus object receiver date = 

printSimpleConditionNH' :: ID -> Subject -> VerbStatus -> Object -> Receiver -> Date -> String
printSimpleConditionNH' id subject verbStatus object receiver date = 

printSimpleStatement' :: ID -> Holds -> Subject -> ModalVerb -> Verb -> Object -> Receiver -> Date -> String
printSimpleStatement' id holds subject modalVerb verb object receiver date = 

printSimpleStatementNH' :: ID -> Subject -> ModalVerb -> Verb -> Object -> Receiver -> Date ->  State StateDict NFA
printSimpleStatementNH' id subject modalVerb verb object receiver date = 

printBooleanExpression' :: ID -> Holds -> Subject -> VerbStatus -> Comparison -> Subject
printBooleanExpression' id holds subject1 verbStatus comparison subject2 = 

printBooleanExpressionNH' :: ID -> Subject -> VerbStatus -> Comparison -> Subject
printBooleanExpressionNH' id subject1 verbStatus comparison subject2 = 

printSimpleDefinition :: SimpleDefinition -> String

printSimpleStatement :: SimpleStatement -> String

idToString :: ID -> String
idToString (IDSim num) = "[" ++ numToString num ++ "]"
idToString (IDRep num1 num2) = "[" ++ numToString num1 ++ "]"

numToString :: Num -> String
numToString (NumInt integer) = show integer

holdsToString :: Holds -> String
holdsToString (HoldYes) = "it is the case that"
holdsToString (HoldNo) = "it is not the case that"

subjectToString :: Subject -> String
subjectToString (SubQuoted str) = str
subjectToString (SubUnQuoted ident) = getIdentString ident

getIdentString :: Ident -> String
getIdentString (Ident str) = str

verbStatusToString :: VerbStatus -> String
verbStatusToString VSDel = "delivered"
verbStatusToString VSPay = "paid"
verbStatusToString VSCharge = "charged"
verbStatusToString VSRefund = "refunded"

comparisonToString :: Comparison -> String
comparisonToString (CompareLess) = "LESS THAN"
comparisonToString (CompareEq _) = "EQUAL TO"
comparisonToString (CompareMore _) = "MORE THAN"

checkContractCompleteness :: Contract -> State BoolExDictionary CompletenessReport
checkContractCompleteness (ConEmpty) = return $ IncompleteItems ([], [], [], [], Map.empty)
checkContractCompleteness (ConComp component) = checkComponentCompleteness component
checkContractCompleteness (ConAnd component contract) = do
    compResult <- checkComponentCompleteness component
    contractResult <- checkContractCompleteness contract
    combineComponents compResult contractResult

combineComponents :: CompletenessReport -> CompletenessReport -> State BoolExDictionary CompletenessReport
combineComponents (IncompleteItems (sc1, sd1, scc1, ss1, be1)) (IncompleteItems (sc2, sd2, scc2, ss2, be2)) = do
    boolExDict <- get  -- Get the current state

    let newDict = Map.union be1 be2
    put newDict  -- Update the state with the new dictionary

    return $ IncompleteItems (sc1 ++ sc2, sd1 ++ sd2, scc1 ++ scc2, ss1 ++ ss2, newDict)

checkComponentCompleteness :: Component -> State BoolExDictionary CompletenessReport
checkComponentCompleteness (ComDef definition) = return $ IncompleteItems ([], [], [], [], Map.empty)
checkComponentCompleteness (ComConDef conditionalDefinition) = checkConditionalDefinitionCompleteness conditionalDefinition
checkComponentCompleteness (ComState statement) = return $ IncompleteItems ([], [], [], [], Map.empty)
checkComponentCompleteness (ComConState conditionalStatement) = checkConditionalStatementCompleteness conditionalStatement

checkConditionalDefinitionCompleteness :: ConditionalDefinition -> State BoolExDictionary CompletenessReport
checkConditionalDefinitionCompleteness (ConDefIf definition condition) = do
    boolExDict <- get  

    let (boolExResult, newState) = runState (checkBooleanExpressionCompleteness condition) boolExDict

    let conditionResult = checkConditionCompleteness condition
        definitionResult = checkDefinitionCompleteness definition
        completenessReport = IncompleteItems (conditionResult, definitionResult, [], [], boolExResult)

    put newState

    return completenessReport

checkConditionalDefinitionCompleteness (ConDefIfThen condition definition) = do
    boolExDict <- get  

    let (boolExResult, newState) = runState (checkBooleanExpressionCompleteness condition) boolExDict

    let conditionResult = checkConditionCompleteness condition
        definitionResult = checkDefinitionCompleteness definition
        completenessReport = IncompleteItems (conditionResult, definitionResult, [], [], boolExResult)

    put newState

    return completenessReport

checkConditionalDefinitionCompleteness (ConDefIfElse definition1 condition definition2) = do
    boolExDict <- get  

    let (boolExResult, newState) = runState (checkBooleanExpressionCompleteness condition) boolExDict

    let completenessReport = IncompleteItems ([], [], [], [], boolExResult)

    put newState

    return completenessReport

checkConditionalDefinitionCompleteness (ConDefIfThenElse condition definition1 definition2) = do
    boolExDict <- get  

    let (boolExResult, newState) = runState (checkBooleanExpressionCompleteness condition) boolExDict

    let completenessReport = IncompleteItems ([], [], [], [], boolExResult)

    put newState

    return completenessReport

checkConditionalStatementCompleteness :: ConditionalStatement -> State BoolExDictionary CompletenessReport
checkConditionalStatementCompleteness (ConStateIf statement condition) = do
    boolExDict <- get  

    let (boolExResult, newState) = runState (checkBooleanExpressionCompleteness condition) boolExDict

    let conditionResult = checkConditionCompleteness condition
        statementResult = checkStatementCompleteness statement
        completenessReport = IncompleteItems ([], [], conditionResult, statementResult, boolExResult)

    put newState

    return completenessReport

checkConditionalStatementCompleteness (ConStateIfThen condition statement) = do
    boolExDict <- get  

    let (boolExResult, newState) = runState (checkBooleanExpressionCompleteness condition) boolExDict

    let conditionResult = checkConditionCompleteness condition
        statementResult = checkStatementCompleteness statement
        completenessReport = IncompleteItems ([], [], conditionResult, statementResult, boolExResult)

    put newState

    return completenessReport

checkConditionalStatementCompleteness (ConStateIfElse statement1 condition statement2) = do
    boolExDict <- get  

    let (boolExResult, newState) = runState (checkBooleanExpressionCompleteness condition) boolExDict

    let completenessReport = IncompleteItems ([], [], [], [], boolExResult)

    put newState

    return completenessReport

checkConditionalStatementCompleteness (ConStateIfThenElse condition statement1 statement2) = do
    boolExDict <- get  

    let (boolExResult, newState) = runState (checkBooleanExpressionCompleteness condition) boolExDict

    let completenessReport = IncompleteItems ([], [], [], [], boolExResult)

    put newState

    return completenessReport

checkConditionCompleteness :: Condition -> [SimpleCondition]
checkConditionCompleteness (CondiSim simpleCondition) = [simpleCondition]
checkConditionCompleteness (CondiOr simpleCondition condition) = [simpleCondition] ++ checkConditionCompleteness condition
checkConditionCompleteness (CondiAnd simpleCondition condition) = [simpleCondition] ++ checkConditionCompleteness condition

checkDefinitionCompleteness :: Definition -> [SimpleDefinition]
checkDefinitionCompleteness (DefSim simpleDefinition) = [simpleDefinition] 
checkDefinitionCompleteness (DefAnd simpleDefinition definition) = [simpleDefinition] ++ checkDefinitionCompleteness definition

checkStatementCompleteness :: Statement -> [SimpleStatement]
checkStatementCompleteness (StateSim simpleStatement) = [simpleStatement]
checkStatementCompleteness (StateOr simpleStatement statement) = [simpleStatement] ++ checkStatementCompleteness statement
checkStatementCompleteness (StateAnd simpleStatement statement) = [simpleStatement] ++ checkStatementCompleteness statement

checkBooleanExpressionCompleteness :: Condition -> State BoolExDictionary BoolExDictionary
checkBooleanExpressionCompleteness (CondiSim simpleCondition) = checkSimpleConditionCompleteness simpleCondition
checkBooleanExpressionCompleteness (CondiOr simpleCondition condition) 
    = liftM2 Map.union (checkSimpleConditionCompleteness simpleCondition) (checkBooleanExpressionCompleteness condition)
checkBooleanExpressionCompleteness (CondiAnd simpleCondition condition)
    = liftM2 Map.union (checkSimpleConditionCompleteness simpleCondition) (checkBooleanExpressionCompleteness condition)

checkSimpleConditionCompleteness :: SimpleCondition -> State BoolExDictionary BoolExDictionary
checkSimpleConditionCompleteness (SimConOne _ _ _ _ _ _ _) = return $ Map.empty
checkSimpleConditionCompleteness (SimConTwo _ _ _ _ _ _ _) = return $ Map.empty
checkSimpleConditionCompleteness (SimConThree _ _ _ _ _ _ _) = return $ Map.empty
checkSimpleConditionCompleteness (SimConFour _ _ _ _ _ _ _ _) = return $ Map.empty
checkSimpleConditionCompleteness (SimConFive id holds (BoolEx subject1 verbStatus comparison subject2)) 
    = checkBoolExCompleteness id holds subject1 verbStatus comparison subject2
checkSimpleConditionCompleteness (SimConOneNH _ _ _ _ _ _) = return $ Map.empty
checkSimpleConditionCompleteness (SimConTwoNH _ _ _ _ _ _) = return $ Map.empty
checkSimpleConditionCompleteness (SimConThreeNH _ _ _ _ _ _) = return $ Map.empty
checkSimpleConditionCompleteness (SimConFourNH _ _ _ _ _ _ _) = return $ Map.empty
checkSimpleConditionCompleteness (SimConFiveNH id (BoolEx subject1 verbStatus comparison subject2))
    = checkBoolExCompleteness id HoldYes subject1 verbStatus comparison subject2

checkBoolExCompleteness :: ID -> Holds -> Subject -> VerbStatus -> Comparison -> Subject -> State BoolExDictionary BoolExDictionary
checkBoolExCompleteness id holds subject1 verbStatus comparison subject2 = 
    let idStr = idToString id
        holdStr = holdsToString holds
        subject1Str = subjectToString subject1
        verbStatusStr = verbStatusToString verbStatus
        comparisonStr = comparisonToString comparison
        subject2Str = subjectToString subject2

        sentenceStr = idStr ++ " " ++ holdStr ++ " " ++ subject1Str ++ " " ++ verbStatusStr ++ " " ++ subject2Str
        compareStr = comparisonStr
    in
    updateBoolExDict sentenceStr compareStr

updateBoolExDict :: String -> String -> State BoolExDictionary BoolExDictionary
updateBoolExDict key1 key2 = do
    boolExDict <- get  -- Get the current state

    let newDict = case Map.lookup key1 boolExDict of
                    Nothing -> Map.insert key1 [key2] boolExDict
                    Just values -> Map.insert key1 (values ++ [key2]) boolExDict

    put newDict  -- Update the state with the new dictionary

    return newDict  -- Return the updated dictionary (optional, based on your needs)

runCheckCompleteness :: Contract -> CompletenessReport
runCheckCompleteness contract = evalState (checkContractCompleteness contract) (Map.empty)