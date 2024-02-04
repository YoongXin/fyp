module ComplexityAnalysis where

import Prelude
  ( ($), (++), (.), (+), (^), (/), (-), (<=)
  , Int, Integer, Double
  , String
  , Show, show
  , Read
  , Bool(..)
  , Maybe(..)
  , IO, putStrLn
  , fromIntegral
  , map, notElem
  , sum, length, floor
  )

import AbsCoLa  
import ToStringFunctions
import AstToDFA

import Control.Monad.State
import qualified Data.Map as Map

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
analyseDateComplexity (DateQuanSpecific _ _ _ _) = return $ Score 1
analyseDateComplexity (DateQuanSome _ _) = return $ Score 1 
analyseDateComplexity (DateQuanThe _ _) = return $ Score 1
analyseDateComplexity (DateQuanSomeWO _ _ _) = return $ Score 3
analyseDateComplexity (DateQuanTheWO _ _ _) = return $ Score 3
analyseDateComplexity (DateQuanTempSome _ _ _ _) = return $ Score 4
analyseDateComplexity (DateQuanTempThe _ _ _ _ ) = return $ Score 4

-- runComplexityAnalysis :: Contract -> ComplexityScore
-- runComplexityAnalysis contract = 
--     let oriScore, helperDict = runState (analyseContractComplexity contract) (Map.empty)
--         partiesScore = getNumberOfParties helperDict
--         poundScore = getPoundVarianceScore helperDict
--         dollarScore = getDollarVarianceScore helperDict
--         euroScore = getEuroVarianceScore helperDict

--         otherScores = partiesScore + poundScore + dollarScore + euroScore 

--     in combineScores (oriScore) (Score otherScores)

runComplexityAnalysis :: Contract -> ComplexityScore
runComplexityAnalysis contract =
    let (oriScore, helperDict) = runState (analyseContractComplexity contract) Map.empty
        currencies = ["Pound", "Dollar", "Euro"]
        varianceScores = map (\currency -> getVarianceScore helperDict currency) currencies
        dfa = runDFAConversionFinal contract
        stateCount = getNumberOfStates dfa
        transitionCount = getNumberOfTransitions dfa

        otherScores = getNumberOfParties helperDict + sum varianceScores + stateCount + transitionCount
    in combineScores oriScore (Score otherScores)

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
    let filteredDict = Map.filterWithKey (\key _ -> key `notElem` ["Pound", "Dollar", "Euro"]) helperDict
    in fromIntegral $ Map.size filteredDict

-- getPoundVarianceScore :: HelperDict -> Integer
-- getPoundVarianceScore helperDict =
--     case Map.lookup "Pound" helperDict of
--     Nothing -> 0 
--     Just poundList ->
--         if length poundList <= 1
--         then 0  
--         else calculateVarianceScore poundList

-- getDollarVarianceScore :: HelperDict -> Integer
-- getDollarVarianceScore helperDict =
--     case Map.lookup "Dollar" helperDict of
--     Nothing -> 0 
--     Just poundList ->
--         if length poundList <= 1
--         then 0  
--         else calculateVarianceScore poundList

-- getEuroVarianceScore :: HelperDict -> Integer
-- getEuroVarianceScore helperDict =
--     case Map.lookup "Euro" helperDict of
--     Nothing -> 0 
--     Just poundList ->
--         if length poundList <= 1
--         then 0  
--         else calculateVarianceScore poundList

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