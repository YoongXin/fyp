module ContractAnalysis.CompletenessAnalysis where

import Prelude
  ( ($), (++), (<), (.), (+), (-)
  , Int, Integer, String, Show, Read, IO, Bool(..), Maybe(..)
  , words, unwords, putStrLn, init, last, length, null, map, fst, snd, zipWith, concat, toInteger, show
  )

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.List as List

import Data.List ( intercalate, nub )
import Control.Monad ( liftM2 )

import Parser.AbsCoLa
import ContractAnalysis.AstToDfa 
import Helper.ToStringFunctions

data CompletenessReport
    = IncompleteItems ([SimpleCondition], [SimpleDefinition], [SimpleCondition], [SimpleStatement], BoolExDictionary)
  deriving (Read, Show)

type BoolExDictionary = Map.Map String [(String, String)]

printCompletenessReport' :: CompletenessReport -> IO()
printCompletenessReport' (IncompleteItems (scs, sds, sccs, sss, beDict)) = do
    putStrLn "\nIncomplete Conditional Definitions (If without \"else\"):\n"
    putStrLn "Conditions:\n"
    mapM_ (putStrLn . printSimpleCondition) scs
    putStrLn "\nDefinitions:\n"
    mapM_ (putStrLn . printSimpleDefinition) sds

    putStrLn "\nIncomplete Conditional Statements (If without \"else\"):\n"
    putStrLn "Conditions:\n"
    mapM_ (putStrLn . printSimpleCondition) sccs
    putStrLn "\nStatements:\n"
    mapM_ (putStrLn . printSimpleStatement) sss

    putStrLn "\nIncomplete Boolean Expressions (Value test that doesn't consider all values):\n"
    putStrLn $ printBoolExDictionary beDict

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
    let idStr = idToString id
        holdStr = holdsToString holds
        subjectStr = subjectToString subject
        verbStatusStr = verbStatusToString verbStatus
        objectStr = objectToString object
        receiverStr = receiverToString receiver
        dateStr = dateToString date

        sc = idStr ++ " " ++ holdStr ++ " " ++ subjectStr ++ " " ++ verbStatusStr ++ " " ++ objectStr ++ " " ++ receiverStr ++ " " ++ dateStr
    in sc

printSimpleConditionNH' :: ID -> Subject -> VerbStatus -> Object -> Receiver -> Date -> String
printSimpleConditionNH' id subject verbStatus object receiver date = 
    let idStr = idToString id
        subjectStr = subjectToString subject
        verbStatusStr = verbStatusToString verbStatus
        objectStr = objectToString object
        receiverStr = receiverToString receiver
        dateStr = dateToString date

        scNH = idStr ++ " " ++ subjectStr ++ " " ++ verbStatusStr ++ " " ++ objectStr ++ " " ++ receiverStr ++ " " ++ dateStr
    in scNH

printSimpleStatement' :: ID -> Holds -> Subject -> ModalVerb -> Verb -> Object -> Receiver -> Date -> String
printSimpleStatement' id holds subject modalVerb verb object receiver date = 
    let idStr = idToString id
        holdStr = holdsToString holds
        subjectStr = subjectToString subject
        modalVerbStr = modalVerbToString modalVerb
        verbStr = verbToString verb
        objectStr = objectToString object
        receiverStr = receiverToString receiver
        dateStr = dateToString date

        ss = idStr ++ " " ++ holdStr ++ " " ++ subjectStr ++ " " ++ modalVerbStr ++ " " ++ verbStr ++ " " ++ objectStr ++ " " ++ receiverStr ++ " " ++ dateStr
    in ss

printSimpleStatementNH' :: ID -> Subject -> ModalVerb -> Verb -> Object -> Receiver -> Date -> String
printSimpleStatementNH' id subject modalVerb verb object receiver date = 
    let idStr = idToString id
        subjectStr = subjectToString subject
        modalVerbStr = modalVerbToString modalVerb
        verbStr = verbToString verb
        objectStr = objectToString object
        receiverStr = receiverToString receiver
        dateStr = dateToString date

        ssNH = idStr ++ " " ++ subjectStr ++ " " ++ modalVerbStr ++ " " ++ verbStr ++ " " ++ objectStr ++ " " ++ receiverStr ++ " " ++ dateStr
    in ssNH

printBooleanExpression' :: ID -> Holds -> Subject -> VerbStatus -> Comparison -> Subject -> String
printBooleanExpression' id holds subject1 verbStatus comparison subject2 = 
    let idStr = idToString id
        holdStr = holdsToString holds
        subject1Str = subjectToString subject1
        verbStatusStr = verbStatusToString verbStatus
        comparisonStr = comparisonToString' comparison
        subject2Str = subjectToString subject2

        be = idStr ++ " " ++ holdStr ++ " " ++ subject1Str ++ " " ++ verbStatusStr ++ " " ++ comparisonStr ++ " " ++ subject2Str
    in be

printBooleanExpressionNH' :: ID -> Subject -> VerbStatus -> Comparison -> Subject -> String
printBooleanExpressionNH' id subject1 verbStatus comparison subject2 = 
    let idStr = idToString id
        subject1Str = subjectToString subject1
        verbStatusStr = verbStatusToString verbStatus
        comparisonStr = comparisonToString' comparison
        subject2Str = subjectToString subject2

        be = idStr ++ " " ++ subject1Str ++ " " ++ verbStatusStr ++ " " ++ comparisonStr ++ " " ++ subject2Str
    in be

printSimpleDefinition :: SimpleDefinition -> String
printSimpleDefinition (SimDefIs id subject1 subject2) =
    let idStr = idToString id
        subject1Str = subjectToString subject1
        subject2Str = subjectToString subject2

        sd1 = idStr ++ " " ++ subject1Str ++ " IS " ++ subject2Str
    in sd1
printSimpleDefinition (SimDefEq id subject numericalExpression) =
    let idStr = idToString id
        subjectStr = subjectToString subject
        numExStr = numericalExpressionToString numericalExpression

        sd2 = idStr ++ " " ++ subjectStr ++ " EQUALS " ++ numExStr
    in sd2
printSimpleDefinition (SimDefDate id subject day month year) =
    let idStr = idToString id
        subjectStr = subjectToString subject
        dateStr = dateSpeToString day month year

        sd3 = idStr ++ " " ++ subjectStr ++ " IS " ++ dateStr
    in sd3

printSimpleStatement :: SimpleStatement -> String
printSimpleStatement (SimStateOne id holds subject modalVerb verb object receiver date) = 
    printSimpleStatement' id holds subject modalVerb verb object receiver date
printSimpleStatement (SimStateTwo id holds subject date modalVerb verb object receiver) =
    printSimpleStatement' id holds subject modalVerb verb object receiver date
printSimpleStatement (SimStateThree id holds date subject modalVerb verb object receiver) =
    printSimpleStatement' id holds subject modalVerb verb object receiver date
printSimpleStatement (SimStateFour id holds subject verbStatus object receiver date) =
    printSimpleCondition' id holds subject verbStatus object receiver date
printSimpleStatement (SimStateOneNH id subject modalVerb verb object receiver date) = 
    printSimpleStatementNH' id subject modalVerb verb object receiver date
printSimpleStatement (SimStateTwoNH id subject date modalVerb verb object receiver) =
    printSimpleStatementNH' id subject modalVerb verb object receiver date
printSimpleStatement (SimStateThreeNH id date subject modalVerb verb object receiver) =
    printSimpleStatementNH' id subject modalVerb verb object receiver date
printSimpleStatement (SimStateFourNH id subject verbStatus object receiver date) =
    printSimpleConditionNH' id subject verbStatus object receiver date

printBoolExDictionary :: BoolExDictionary -> String
printBoolExDictionary beDict =
    Map.foldrWithKey printKeyValuePair "" filteredDict
    where
    filteredDict = Map.filter (\values -> length (List.nub values) < 3) beDict

printKeyValuePair :: String -> [(String, String)] -> String -> String
printKeyValuePair key values acc =
    let keyWithoutLastWord = unwords (init (words key))
        lastWord = last (words key)
        comparisons = map fst values
        ids = map snd values
        linesToPrint = zipWith (\comparison id -> id ++ " " ++ keyWithoutLastWord ++ " " ++ comparison ++ " " ++ lastWord ++ "\n") comparisons ids
    in acc ++ concat linesToPrint

removeDuplicatesFromBoolExDict :: CompletenessReport -> CompletenessReport
removeDuplicatesFromBoolExDict (IncompleteItems (conds1, defs, conds2, stmts, dict)) =
    IncompleteItems (conds1, defs, conds2, stmts, Map.map nub dict)

checkContractCompleteness :: Contract -> State BoolExDictionary CompletenessReport
checkContractCompleteness (ConEmpty) = return $ IncompleteItems ([], [], [], [], Map.empty)
checkContractCompleteness (ConComp component) = checkComponentCompleteness component
checkContractCompleteness (ConAnd component contract) = do
    compResult <- checkComponentCompleteness component
    contractResult <- checkContractCompleteness contract
    combineComponents compResult contractResult

combineComponents :: CompletenessReport -> CompletenessReport -> State BoolExDictionary CompletenessReport
combineComponents (IncompleteItems (sc1, sd1, scc1, ss1, be1)) (IncompleteItems (sc2, sd2, scc2, ss2, be2)) = do
    boolExDict <- get 

    let newDict = Map.unionWith (++) be1 be2
    put newDict 

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
    = liftM2 (Map.unionWith (++)) (checkSimpleConditionCompleteness simpleCondition) (checkBooleanExpressionCompleteness condition)
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
        comparisonStr = comparisonToString' comparison
        subject2Str = subjectToString subject2

        sentenceStr = holdStr ++ " " ++ subject1Str ++ " " ++ verbStatusStr ++ " " ++ subject2Str
        compareStr = comparisonStr
    in
    updateBoolExDict sentenceStr compareStr idStr

updateBoolExDict :: String -> String -> String -> State BoolExDictionary BoolExDictionary
updateBoolExDict sentence comparison id = do
    boolExDict <- get 

    let newDict = case Map.lookup sentence boolExDict of
                    Nothing -> Map.insert sentence [(comparison, id)] boolExDict
                    Just values -> Map.insert sentence (values ++ [(comparison, id)]) boolExDict

    put newDict  

    return newDict 

generateCompletenessScoring :: Contract -> CompletenessReport -> Integer
generateCompletenessScoring contract (IncompleteItems (scs, sds, sccs, sss, beDict)) =
    let dfa = runDFAConversionFinal contract
        positiveScore = getNumberOfStates dfa
        negativeScore = length scs + length sccs + Map.size beDict
        finalScore = positiveScore - toInteger negativeScore

    in finalScore

runCheckCompleteness :: Contract -> CompletenessReport
runCheckCompleteness contract = removeDuplicatesFromBoolExDict (evalState (checkContractCompleteness contract) (Map.empty))

printCompletenessReport :: CompletenessReport -> Integer -> String
printCompletenessReport (IncompleteItems (scs, sds, sccs, sss, beDict)) completenessScore =
    "\n=======================================================\n" ++
    "Incomplete Conditional Definitions (If without \"else\"):\n" ++
    "=======================================================\n" ++
    (if null scs
        then "\nNone"
        else "\nConditions:\n" ++ intercalate "\n" (map printSimpleCondition scs) ++
             "\n\nDefinitions:\n" ++ intercalate "\n" (map printSimpleDefinition sds)) ++

    "\n\n======================================================\n" ++
    "Incomplete Conditional Statements (If without \"else\"):\n" ++
    "======================================================\n" ++
    (if null sccs
        then "\nNone"
        else "\nConditions:\n" ++ intercalate "\n" (map printSimpleCondition sccs) ++
             "\n\nStatements:\n" ++ intercalate "\n" (map printSimpleStatement sss)) ++

    "\n\n=============================================================================\n" ++
    "Incomplete Boolean Expressions (Value test that doesn't consider all values):\n" ++
    "=============================================================================\n\n" ++
    (if Map.null beDict
        then "None"
        else printBoolExDictionary beDict) ++

    "\n\n===================\n" ++
    "Completeness Score:\n" ++
    "===================\n\n" ++
    show completenessScore