module ContractAnalysis.ComplexityAnalysis where

import Prelude
  ( ($), (++), (.), (+), (^), (/), (-), (<=), (!!)
  , Int, Integer, Double, String, Show, Read, IO, Bool(..), Maybe(..)
  , map, notElem, sum, length, floor, fromIntegral, putStrLn, show
  )

import Control.Monad.State
import qualified Data.Map as Map

import Parser.AbsCoLa  
import Helper.ToStringFunctions
import ContractAnalysis.AstToDfa

data ComplexityScore = Score Integer
    deriving (Read, Show)   

type HelperDict = Map.Map String [Integer]

modifyHelperDictionary :: (HelperDict -> HelperDict) -> State HelperDict ()
modifyHelperDictionary f = modify f

addToHelperDictionary :: String -> Integer -> State HelperDict ()
addToHelperDictionary key value = do
    modifyHelperDictionary $ \dict ->
        case Map.lookup key dict of
            Just values -> Map.insert key (values ++ [value]) dict
            Nothing     -> Map.insert key [value] dict

combineScores :: ComplexityScore -> ComplexityScore -> ComplexityScore
combineScores (Score comp) (Score cont) = Score (comp + cont)

analyseContractComplexity :: Contract -> State HelperDict ComplexityScore
analyseContractComplexity (ConEmpty) = return $ Score 0 
analyseContractComplexity (ConComp component) = analyseComponentComplexity component
analyseContractComplexity (ConAnd component contract) = do
    compScore <- analyseComponentComplexity component
    contScore <- analyseContractComplexity contract
    return $ combineScores compScore contScore

analyseComponentComplexity :: Component -> State HelperDict ComplexityScore
analyseComponentComplexity (ComDef definition) = analyseDefinitionComplexity definition
analyseComponentComplexity (ComConDef conditionalDefinition) = analyseConditionalDefinitionComplexity conditionalDefinition
analyseComponentComplexity (ComState statement) = analyseStatementComplexity statement
analyseComponentComplexity (ComConState conditionalStatement) = analyseConditionalStatementComplexity conditionalStatement

analyseDefinitionComplexity :: Definition -> State HelperDict ComplexityScore
analyseDefinitionComplexity (DefSim simpleDefinition) = analyseSimpleDefinitionComplexity simpleDefinition
analyseDefinitionComplexity (DefAnd simpleDefinition definition) = do
    simDefScore <- analyseSimpleDefinitionComplexity simpleDefinition
    defScore <- analyseDefinitionComplexity definition
    return $ combineScores simDefScore defScore

analyseConditionalDefinitionComplexity :: ConditionalDefinition -> State HelperDict ComplexityScore
analyseConditionalDefinitionComplexity (ConDefIf definition condition) = do
    condScore <- analyseConditionComplexity condition
    defScore <- analyseDefinitionComplexity definition
    return $ combineScores condScore defScore
analyseConditionalDefinitionComplexity (ConDefIfThen condition definition) = do
    condScore <- analyseConditionComplexity condition
    defScore <- analyseDefinitionComplexity definition
    return $ combineScores condScore defScore
analyseConditionalDefinitionComplexity (ConDefIfElse definition1 condition definition2) = do
    condScore <- analyseConditionComplexity condition
    defScore1 <- analyseDefinitionComplexity definition1
    defScore2 <- analyseDefinitionComplexity definition2
    let totalDefScore = combineScores defScore1 defScore2
    return $ combineScores condScore totalDefScore
analyseConditionalDefinitionComplexity (ConDefIfThenElse condition definition1 definition2) = do
    condScore <- analyseConditionComplexity condition
    defScore1 <- analyseDefinitionComplexity definition1
    defScore2 <- analyseDefinitionComplexity definition2
    let totalDefScore = combineScores defScore1 defScore2
    return $ combineScores condScore totalDefScore

analyseStatementComplexity :: Statement -> State HelperDict ComplexityScore
analyseStatementComplexity (StateSim simpleStatement) = analyseSimpleStatementComplexity simpleStatement
analyseStatementComplexity (StateOr simpleStatement statement) = do
    simStateScore <- analyseSimpleStatementComplexity simpleStatement
    stateScore <- analyseStatementComplexity statement
    return $ combineScores simStateScore stateScore
analyseStatementComplexity (StateAnd simpleStatement statement) = do
    simStateScore <- analyseSimpleStatementComplexity simpleStatement
    stateScore <- analyseStatementComplexity statement
    return $ combineScores simStateScore stateScore

analyseConditionalStatementComplexity :: ConditionalStatement -> State HelperDict ComplexityScore
analyseConditionalStatementComplexity (ConStateIf statement condition) = do
    condScore <- analyseConditionComplexity condition
    stateScore <- analyseStatementComplexity statement
    return $ combineScores condScore stateScore
analyseConditionalStatementComplexity (ConStateIfThen condition statement) = do
    condScore <- analyseConditionComplexity condition
    stateScore <- analyseStatementComplexity statement
    return $ combineScores condScore stateScore
analyseConditionalStatementComplexity (ConStateIfElse statement1 condition statement2) = do
    condScore <- analyseConditionComplexity condition
    stateScore1 <- analyseStatementComplexity statement1
    stateScore2 <- analyseStatementComplexity statement2
    let totalStateScore = combineScores stateScore1 stateScore2
    return $ combineScores condScore totalStateScore
analyseConditionalStatementComplexity (ConStateIfThenElse condition statement1 statement2) = do
    condScore <- analyseConditionComplexity condition
    stateScore1 <- analyseStatementComplexity statement1
    stateScore2 <- analyseStatementComplexity statement2
    let totalStateScore = combineScores stateScore1 stateScore2
    return $ combineScores condScore totalStateScore

analyseSimpleDefinitionComplexity :: SimpleDefinition -> State HelperDict ComplexityScore
analyseSimpleDefinitionComplexity (SimDefIs _ _ _) = return $ Score 0
analyseSimpleDefinitionComplexity (SimDefEq _ _ numericalExpression) = analyseNumericalExpressionComplexity numericalExpression
analyseSimpleDefinitionComplexity (SimDefDate _ _ _ _ _) = return $ Score 0

analyseConditionComplexity :: Condition -> State HelperDict ComplexityScore
analyseConditionComplexity (CondiSim simpleCondition) = analyseSimpleConditionComplexity simpleCondition
analyseConditionComplexity (CondiOr simpleCondition condition) = do
    simConScore <- analyseSimpleConditionComplexity simpleCondition
    conScore <- analyseConditionComplexity condition
    return $ combineScores simConScore conScore
analyseConditionComplexity (CondiAnd simpleCondition condition) = do
    simConScore <- analyseSimpleConditionComplexity simpleCondition
    conScore <- analyseConditionComplexity condition
    return $ combineScores simConScore conScore

analyseSimpleStatementComplexity :: SimpleStatement -> State HelperDict ComplexityScore
analyseSimpleStatementComplexity (SimStateOne _ _ subject _ _ object receiver date) = do
    score <- getComplexity subject receiver date object
    return $ score
analyseSimpleStatementComplexity (SimStateTwo _ _ subject date _ _ object receiver) = do
    score <- getComplexity subject receiver date object
    return $ score
analyseSimpleStatementComplexity (SimStateThree _ _ date subject _ _ object receiver) = do
    score <- getComplexity subject receiver date object
    return $ score
analyseSimpleStatementComplexity (SimStateFour _ _ subject _ object receiver date) = do
    score <- getComplexity subject receiver date object
    return $ score
analyseSimpleStatementComplexity (SimStateOneNH _ subject _ _ object receiver date) = do
    score <- getComplexity subject receiver date object
    return $ score
analyseSimpleStatementComplexity (SimStateTwoNH _ subject date _ _ object receiver) = do
    score <- getComplexity subject receiver date object
    return $ score
analyseSimpleStatementComplexity (SimStateThreeNH _ date subject _ _ object receiver) = do
    score <- getComplexity subject receiver date object
    return $ score
analyseSimpleStatementComplexity (SimStateFourNH _ subject _ object receiver date) = do
    score <- getComplexity subject receiver date object
    return $ score

analyseSimpleConditionComplexity :: SimpleCondition -> State HelperDict ComplexityScore
analyseSimpleConditionComplexity (SimConOne _ _ subject _ object receiver date) = do
    score <- getComplexity subject receiver date object
    return $ score
analyseSimpleConditionComplexity (SimConTwo _ _ subject date _ object receiver) = do
    score <- getComplexity subject receiver date object
    return $ score
analyseSimpleConditionComplexity (SimConThree _ _ date subject _ object receiver) = do
    score <- getComplexity subject receiver date object
    return $ score
analyseSimpleConditionComplexity (SimConFour _ _ subject _ _ object receiver date) = do
    score <- getComplexity subject receiver date object
    return $ score
analyseSimpleConditionComplexity (SimConFive _ _ (BoolEx subject _ _ receiver)) = do
    addToHelperDictionary (subjectToString subject) 0
    addToHelperDictionary (subjectToString receiver) 0
    return $ Score 0
analyseSimpleConditionComplexity (SimConOneNH _ subject _ object receiver date) = do
    score <- getComplexity subject receiver date object
    return $ score
analyseSimpleConditionComplexity (SimConTwoNH _ subject date _ object receiver) = do
    score <- getComplexity subject receiver date object
    return $ score
analyseSimpleConditionComplexity (SimConThreeNH _ date subject _ object receiver) = do
    score <- getComplexity subject receiver date object
    return $ score
analyseSimpleConditionComplexity (SimConFourNH _ subject _ _ object receiver date) = do
    score <- getComplexity subject receiver date object
    return $ score
analyseSimpleConditionComplexity (SimConFiveNH _ (BoolEx subject _ _ receiver)) = do
    addToHelperDictionary (subjectToString subject) 0
    addToHelperDictionary (subjectToString receiver) 0
    return $ Score 0

getComplexity :: Subject -> Receiver -> Date -> Object -> State HelperDict ComplexityScore
getComplexity subject receiver date object = do 
    addToHelperDictionary (subjectToString subject) 0
    addToHelperDictionary (receiverToString receiver) 0
    case object of 
        ObjNu numericalObject -> do
            objScore <- analyseNumericalObjectComplexity numericalObject
            dateScore <- analyseDateComplexity date
            return $ combineScores objScore dateScore
        ObjNonNu _ -> analyseDateComplexity date

analyseNumericalExpressionComplexity :: NumericalExpression -> State HelperDict ComplexityScore
analyseNumericalExpressionComplexity (NumExpNum num) = return $ Score 0
analyseNumericalExpressionComplexity (NumExpObj numericalObject) = analyseNumericalObjectComplexity numericalObject
analyseNumericalExpressionComplexity (NumExpOp numExp1 op numExp2) = do
    numExScore1 <- analyseNumericalExpressionComplexity numExp1
    numExScore2 <- analyseNumericalExpressionComplexity numExp2
    let totalNumExScore = combineScores numExScore1 numExScore2
    addToHelperDictionary "Numerical Operator" 2
    return $ combineScores (Score 2) totalNumExScore

analyseNumericalObjectComplexity :: NumericalObject -> State HelperDict ComplexityScore
analyseNumericalObjectComplexity (NumPound _ (NumInt amount)) = do
    addToHelperDictionary "Pound" amount
    return $ Score 0
analyseNumericalObjectComplexity (NumDol _ (NumInt amount)) = do
    addToHelperDictionary "Dollar" amount
    return $ Score 0
analyseNumericalObjectComplexity (NumEur _ (NumInt amount)) = do
    addToHelperDictionary "Euro" amount
    return $ Score 0
analyseNumericalObjectComplexity (NumAmount _) = return $ Score 0

analyseDateComplexity :: Date -> State HelperDict ComplexityScore
analyseDateComplexity (DateSpe _) = return $ Score 0
analyseDateComplexity (DateAny) = return $ Score 0
analyseDateComplexity (DateSome _) = return $ Score 0
analyseDateComplexity (DateThe _) = return $ Score 0
analyseDateComplexity (DateQuanSpecific _ _ _ _) = do
    addToHelperDictionary "Temporal Quantifier" 1
    return $ Score 1
analyseDateComplexity (DateQuanSome _ _) = do
    addToHelperDictionary "Temporal Quantifier" 1
    return $ Score 1 
analyseDateComplexity (DateQuanThe _ _) = do
    addToHelperDictionary "Temporal Quantifier" 1
    return $ Score 1
analyseDateComplexity (DateQuanSomeWO _ _ _) = do
    addToHelperDictionary "Temporal Quantifier" 1
    addToHelperDictionary "Temporal Offset" 2
    return $ Score 3
analyseDateComplexity (DateQuanTheWO _ _ _) = do
    addToHelperDictionary "Temporal Quantifier" 1
    addToHelperDictionary "Temporal Offset" 2
    return $ Score 3
analyseDateComplexity (DateQuanTempSome _ _ _ _) = do
    addToHelperDictionary "Temporal Quantifier" 2
    addToHelperDictionary "Temporal Offset" 2
    return $ Score 4
analyseDateComplexity (DateQuanTempThe _ _ _ _ ) = do
    addToHelperDictionary "Temporal Quantifier" 2
    addToHelperDictionary "Temporal Offset" 2
    return $ Score 4

getVarianceScore :: HelperDict -> String -> Integer
getVarianceScore helperDict currency =
    case Map.lookup currency helperDict of
        Nothing -> 0
        Just amountList ->
            if length amountList <= 1
                then 0
                else calculateVarianceScore amountList

getNumberOfParties :: HelperDict -> Integer
getNumberOfParties helperDict =
    let filteredDict = Map.filterWithKey (\key _ -> key `notElem` ["Pound", "Dollar", "Euro", "Numerical Operator", "Temporal Quantifier", "Temporal Offset"]) helperDict
    in fromIntegral $ Map.size filteredDict

getScoreOfMetrics :: HelperDict -> String -> Integer
getScoreOfMetrics helperDict keyValue =
    case Map.lookup keyValue helperDict of
        Nothing -> 0  
        Just nums -> sum nums 

calculateVarianceScore :: [Integer] -> Integer
calculateVarianceScore amountList = 
    let variance = calculateVariance amountList
        average = calculateAverage amountList
        varianceScore = floor (variance / average)
    in varianceScore

calculateAverage :: [Integer] -> Double
calculateAverage nums =
  fromIntegral (sum nums) / fromIntegral (length nums)

calculateVariance :: [Integer] -> Double
calculateVariance nums =
  let avg = calculateAverage nums
      squaredDiffs = map (\x -> (fromIntegral x - avg) ^ 2) nums
  in sum squaredDiffs / fromIntegral (length nums)

getComplexityMetrics :: Contract -> [Integer]
getComplexityMetrics contract =
    let (oriScore, helperDict) = runState (analyseContractComplexity contract) Map.empty

        dfa = runDFAConversionFinal contract
        stateCount = getNumberOfStates dfa
        transitionCount = getNumberOfTransitions dfa

        numberOfParties = getNumberOfParties helperDict

        numericalOperatorScore = getScoreOfMetrics helperDict "Numerical Operator"
        temporalQuantifierScore = getScoreOfMetrics helperDict "Temporal Quantifier"
        temporalOffsetScore = getScoreOfMetrics helperDict "Temporal Offset"

        currencies = ["Pound", "Dollar", "Euro"]
        varianceScores = map (\currency -> getVarianceScore helperDict currency) currencies
        paymentVariabilityScore = sum varianceScores

    in [stateCount, transitionCount, numberOfParties, numericalOperatorScore, temporalQuantifierScore, temporalOffsetScore, paymentVariabilityScore]

printComplexityReport :: [Integer] -> String
printComplexityReport metricScores =
    "Number of States: " ++ show (metricScores !! 0) ++ "\n" ++ 
    "Number of Transitions: " ++ show (metricScores !! 1) ++ "\n" ++
    "Number of Parties Involved: " ++ show (metricScores !! 2) ++ "\n" ++
    "Number of Numerical Operators: " ++ show (metricScores !! 3) ++ "\n" ++
    "Number of Temporal Quantifiers: " ++ show (metricScores !! 4) ++ "\n" ++
    "Number of Temporal Offsets: " ++ show (metricScores !! 5) ++ "\n" ++
    "Score for Payment Variability: " ++ show (metricScores !! 6) ++ "\n\n" ++
    "Total Complexity Score: " ++ show (sum metricScores)