module AstToDFA where

import Prelude
  ( ($), (++), (||), (==), (+), (<$>), (<>)
  , Int, Integer, fromInteger, toInteger, fromIntegral
  , String
  , Show, show
  , Eq
  , Read
  , Ord
  , Bool(..)
  , IO, putStrLn, unlines
  , Maybe(..)
  , zip, foldr
  , null
  , head, reverse, splitAt, unwords
  , fst, snd
  , error, all, max, not, concat, break
  )

import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad.State
import Data.List (map, lookup, sort, group, concatMap, intercalate, find, stripPrefix, isInfixOf, words) 
import qualified Data.List as List
import Data.Maybe (catMaybes)
import Data.Time

import Data.Functor ((<&>))
import qualified Data.Text.Lazy as L (pack, unpack)
import qualified Data.Text.Lazy.IO as IO (putStrLn)
import Data.Graph.Inductive.Graph          
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.GraphViz                       
import Data.GraphViz.Attributes (Attribute(..))
import qualified Data.GraphViz.Attributes.Complete as Gv
import Data.GraphViz.Printing (renderDot, toDot)
import Data.Text.Internal.Lazy (Text)
import Data.Graph.Inductive (LEdge)
import qualified Data.GraphViz.Attributes.Colors as Gvc

import AbsCoLa   
import LexCoLa   ( Token, mkPosToken )
import ParCoLa   ( pContract, myLexer )
import PrintCoLa ( Print, printTree )
import SkelCoLa  ()
import ToStringFunctions

type StateDictionary = Map.Map StateAD (Set.Set EventD, Int)
type SelfLoopingEvents = [EventD]

data StateAD
    = StateAD String
  deriving (Eq, Ord, Read, Show)

data EventD
    = EventD String
  deriving (Eq, Ord, Read, Show)

data TransitionD
    = TransitionD StateAD EventD StateAD
  deriving (Eq, Ord, Read, Show)

data DFA = DFA
    { states :: Set.Set StateAD
    , events :: Set.Set EventD
    , transitions :: Set.Set TransitionD
    , startStates :: Set.Set StateAD
    , acceptingStates :: Set.Set StateAD
    } 
  deriving (Eq, Ord, Read, Show)

printStateAD :: StateAD -> String
printStateAD (StateAD s) = s

printEventD :: EventD -> String
printEventD (EventD e) = e

printTransitionD :: TransitionD -> String
printTransitionD (TransitionD from event to) = printStateAD from ++ " --(" ++ printEventD event ++ ")--> " ++ printStateAD to

printTransitionsD :: Set.Set TransitionD -> String
printTransitionsD transitionSet =
    unlines $ map printTransitionD (Set.toList transitionSet)

prettyPrintTransitionsD :: Set.Set TransitionD -> IO ()
prettyPrintTransitionsD transitions = putStrLn $ printTransitionsD transitions

printDFA :: DFA -> String
printDFA dfa =
    unlines
        [ "States:"
        , unlines (map printStateAD (Set.toList $ states dfa))
        , "Events:"
        , unlines (map printEventD (Set.toList $ events dfa))
        , "Transitions:"
        , unlines (map printTransitionD (Set.toList $ transitions dfa))
        , "Start States:"
        , unlines (map printStateAD (Set.toList $ startStates dfa))
        , "Accepting States:"
        , unlines (map printStateAD (Set.toList $ acceptingStates dfa))
        ]

prettyPrintDFA dfa = putStrLn $ printDFA dfa

modifyStateDictionary :: (StateDictionary -> StateDictionary) -> State (StateDictionary, SelfLoopingEvents) ()
modifyStateDictionary f = modify (\(dict, events) -> (f dict, events))

addToStateDictionary :: StateAD -> Set.Set EventD -> Int -> State (StateDictionary, SelfLoopingEvents) ()
addToStateDictionary newState newEvents idInt = do
    modifyStateDictionary (\dict -> Map.insert newState (newEvents, idInt) dict)

removeFromStateDictionary :: Set.Set StateAD -> State (StateDictionary, SelfLoopingEvents) ()
removeFromStateDictionary toBeRemovedStates = do
    modifyStateDictionary (\dict -> removeStatesFromDictionary toBeRemovedStates dict)

removeStatesFromDictionary :: Set.Set StateAD -> StateDictionary -> StateDictionary
removeStatesFromDictionary toBeRemovedStates dict =
    foldr Map.delete dict (Set.toList toBeRemovedStates)

generateTransition :: StateAD -> StateAD -> State (StateDictionary, SelfLoopingEvents) [TransitionD]
generateTransition state1 state2 = do
    stateDict <- gets fst

    case Map.lookup state2 stateDict of
        Just (eventsSet, _) -> do
            let transitionsList = [TransitionD state1 event state2 | event <- Set.toList eventsSet]
            return transitionsList

        Nothing ->
            return []

generateThenState :: StateAD -> Set.Set StateAD -> StateAD
generateThenState ifState thenStates = 
    let thenState = StateAD $ combineSets " OR " (Set.singleton $ ifState) `combine` combineSets " OR " thenStates

    in thenState
  
combineSets :: String -> Set.Set StateAD -> String
combineSets separator states = foldr (\(StateAD s) acc -> s ++ if null acc then "" else separator ++ acc) "" (Set.toList states)

combine :: String -> String -> String
combine x y = if null x || null y then x ++ y else x ++ " AND " ++ y

generateThenState' :: Set.Set EventD -> Set.Set EventD -> State (StateDictionary, SelfLoopingEvents) StateAD
generateThenState' ifEvents thenEvents = do
    originalStateDictionary <- gets fst
    let thenState = StateAD $ combineSets' " OR " ifEvents `combine` combineSets' " OR " thenEvents
    
    return thenState

combineSets' :: String -> Set.Set EventD -> String
combineSets' separator events = foldr (\(EventD s) acc -> s ++ if null acc then "" else separator ++ acc) "" (Set.toList events)

generateAndEvent :: Set.Set EventD -> Set.Set EventD -> EventD
generateAndEvent events1 events2 =
    EventD $ intercalate " AND " $ map (\(EventD s) -> s) (Set.toList events1 ++ Set.toList events2)

generateAndState :: Set.Set StateAD -> Set.Set StateAD -> StateAD
generateAndState states1 states2 =
    StateAD $ intercalate " AND " $ map (\(StateAD s) -> s) (Set.toList states1 ++ Set.toList states2)

generateOrState :: Set.Set StateAD -> Set.Set StateAD -> StateAD
generateOrState states1 states2 =
    StateAD $ intercalate " OR " $ map (\(StateAD s) -> s) (Set.toList states1 ++ Set.toList states2)  

removeSelfLoopingEvents :: Set.Set EventD -> [EventD] -> Set.Set EventD
removeSelfLoopingEvents eventsSet eventList = Set.difference eventsSet (Set.fromList eventList)

addSelfLoopingEvents :: EventD -> State (StateDictionary, SelfLoopingEvents) ()
addSelfLoopingEvents newItem = do
    (stateDict, eventsList) <- get
    let updatedEventsList = newItem : eventsList
    put (stateDict, updatedEventsList)

getNonBreachState :: Set.Set StateAD -> StateDictionary -> StateAD
getNonBreachState conditionStates stateDict =
    case findNonBreachState conditionStates stateDict of
        Just nonBreachState -> nonBreachState
        Nothing             -> error "No non-breach state found in the given set"

findNonBreachState :: Set.Set StateAD -> StateDictionary -> Maybe StateAD
findNonBreachState conditionStates stateDict =
    find (\state -> case Map.lookup state stateDict of
                        Just (_, 1) -> True
                        _           -> False) (Set.toList conditionStates)

getNonBreachStates :: Set.Set StateAD -> StateDictionary -> Set.Set StateAD
getNonBreachStates conditionStates stateDict =
    Set.fromList $ catMaybes $ map (\state -> checkState state stateDict) (Set.toList conditionStates)
    where
    checkState :: StateAD -> StateDictionary -> Maybe StateAD
    checkState state dict =
        case Map.lookup state dict of
            Just (_, 1) -> Just state
            _           -> Nothing

getBreachStates :: Set.Set StateAD -> StateDictionary -> Set.Set StateAD
getBreachStates conditionStates stateDict =
    Set.fromList $ catMaybes $ map (\state -> checkState state stateDict) (Set.toList conditionStates)
    where
    checkState :: StateAD -> StateDictionary -> Maybe StateAD
    checkState state dict =
        case Map.lookup state dict of
            Just (_, value) | value == 2 || value == 3 -> Just state
            otherwise -> Nothing

getBreachState :: Set.Set StateAD -> StateDictionary -> StateAD
getBreachState conditionStates stateDict =
    case findBreachState conditionStates stateDict of
        Just breachState -> breachState
        Nothing          -> error "No breach state found in the given set"

findBreachState :: Set.Set StateAD -> StateDictionary -> Maybe StateAD
findBreachState conditionStates stateDict =
    find (\state -> case Map.lookup state stateDict of
                        Just (_, value) | value == 2 || value == 3 -> True
                        otherwise -> False) (Set.toList conditionStates)

getNonBreachEvents :: Set.Set EventD -> StateDictionary -> Set.Set EventD
getNonBreachEvents allConditionEvents stateDict =
        Set.filter (\event -> checkEvent event eventMap) allConditionEvents
    where
        eventMap = createMappingOfEvents stateDict

        checkEvent :: EventD -> Map.Map EventD Int -> Bool
        checkEvent event eventMap =
            case Map.lookup event eventMap of
                Just val -> val == 1
                Nothing  -> False

getBreachEvents :: Set.Set EventD -> StateDictionary -> Set.Set EventD
getBreachEvents allConditionEvents stateDict =
        Set.filter (\event -> checkEvent event eventMap) allConditionEvents
    where
        eventMap = createMappingOfEvents stateDict

        checkEvent :: EventD -> Map.Map EventD Int -> Bool
        checkEvent event eventMap =
            case Map.lookup event eventMap of
                Just val -> val == 2 || val == 3 
                Nothing  -> False

createMappingOfEvents :: StateDictionary -> Map.Map EventD Int
createMappingOfEvents stateDict =
    Map.fromListWith max $ concatMap (\(_, (events, val)) -> [(event, val) | event <- Set.toList events]) (Map.toList stateDict)

removeNonBreachTransitions :: Set.Set TransitionD -> Set.Set EventD -> Set.Set TransitionD
removeNonBreachTransitions transitions events =
    Set.filter (\(TransitionD _ event _) -> not (event `Set.member` events)) transitions

-- exampleStateDict :: StateDictionary
-- exampleStateDict = Map.fromList
--     [ (StateAD "Hello", (Set.fromList [EventD "World", EventD "Bye"], 1))
--     , (StateAD "Chris", (Set.singleton (EventD "Clack"), 3))
--     , (StateAD "hate", (Set.fromList [EventD "mad", EventD "everyone"], 1))
--     ]

dateSpeToInt :: Num -> Month -> Num -> Integer
dateSpeToInt (NumInt year) month (NumInt day) = dateToInt year (monthToInt month) (fromInteger day)

dateToInt :: Integer -> Int -> Int -> Integer
dateToInt year month day = toModifiedJulianDay (fromGregorian year month day)

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

intToMonth :: Int -> Month
intToMonth 1 = MJan
intToMonth 2 = MFeb 
intToMonth 3 = MMar
intToMonth 4 = MApr 
intToMonth 5 = MMay 
intToMonth 6 = MJun
intToMonth 7 = MJul
intToMonth 8 = MAug 
intToMonth 9 = MSep
intToMonth 10 = MOct
intToMonth 11 = MNov 
intToMonth 12 = MDec

intToDate :: Integer -> (Int, Int, Int)
intToDate dateInt = 
    let (y, m, d) = toGregorian $ ModifiedJulianDay $ fromInteger dateInt
        year = fromInteger y
    in (year, m, d)

dateToDateSpe :: (Int, Int, Int) -> Date
dateToDateSpe (year, month, day) =
    DateSpe (DateSpeOnThe (NumInt $ toInteger day) (intToMonth month) (NumInt $ toInteger year))

generateNextDay :: Date -> Date
generateNextDay (DateSpe (DateSpeOnThe day month year)) =
    let theDayInt = dateSpeToInt year month day
        theNextDay = dateToDateSpe (intToDate (theDayInt + 1))
    in theNextDay
generateNextDay (DateSpe (DateSpeOn day month year)) =
    let theDayInt = dateSpeToInt year month day
        theNextDay = dateToDateSpe (intToDate (theDayInt + 1))
    in theNextDay

generateBreachEvent :: Subject -> Verb -> Object -> Receiver -> Date -> EventD
generateBreachEvent subject verb object receiver date = 
    case (date) of
        (DateSpe _) -> specificDate 
        (DateAny) -> anyDate
        (DateSome some) -> someDate some
        (DateThe the) -> theDate the
        (DateQuanSpecific tq day month year) -> baSpecificDate tq day month year
        (DateQuanSome tq some) -> baSomeDate tq some
        (DateQuanThe tq the) -> baTheDate tq the
        (DateQuanSomeWO to tq some) -> baoSomeDate to tq some
        (DateQuanTheWO to tq the) -> baoTheDate to tq the
        (DateQuanTempSome tq to tq1 some) -> baobaSomeDate tq to tq1 some
        (DateQuanTempThe tq to tq1 the) -> baobaTheDate tq to tq1 the
    where
        specificDate = 
            let nextDay = generateNextDay date
                dateStr = dateToString' nextDay
                eventStr = "Occurrence of " ++ dateStr
            in EventD eventStr
        anyDate = 
            let eventStr = subjectToString subject ++ " DIDN'T " ++ verbToString verb ++ " to " ++ receiverToString receiver
            in EventD eventStr
        someDate some =
            let eventStr = "Occurrence of one day after SOMEDATE " ++ subjectToString some
            in EventD eventStr
        theDate the =
            let eventStr = "Occurrence of one day after THEDATE " ++ subjectToString the
            in EventD eventStr
        baSpecificDate tq day month year =
            case (tq) of
                (TempAfter) -> afterCase
                (TempBefore) -> beforeCase
            where
                afterCase =
                    let eventStr = subjectToString subject ++ "DIDN'T" ++ verbToString verb ++ " to " ++ receiverToString receiver
                    in EventD eventStr
                beforeCase = 
                    let dateStr = dateSpeToString day month year
                        eventStr = "Occurrence of " ++ dateStr
                    in EventD eventStr
        baSomeDate tq some =
            case (tq) of
                (TempAfter) -> afterCase
                (TempBefore) -> beforeCase
            where
                afterCase =
                    let eventStr = subjectToString subject ++ "DIDN'T" ++ verbToString verb ++ " to " ++ receiverToString receiver
                    in EventD eventStr
                beforeCase = 
                    let eventStr = "Occurrence of SOMEDATE " ++ subjectToString some
                    in EventD eventStr
        baTheDate tq the =
            case (tq) of
                (TempAfter) -> afterCase
                (TempBefore) -> beforeCase
            where
                afterCase =
                    let eventStr = subjectToString subject ++ "DIDN'T" ++ verbToString verb ++ " to " ++ receiverToString receiver
                    in EventD eventStr
                beforeCase = 
                    let eventStr = "Occurrence of THEDATE " ++ subjectToString the
                    in EventD eventStr
        baoSomeDate to tq some =
            let eventStr = "Occurrence of " ++ temporalOffsetToString to ++ temporalQuantifierToString tq ++ "SOMEDATE " ++ subjectToString some
            in EventD eventStr
        baoTheDate to tq the =
            let eventStr = "Occurrence of " ++ temporalOffsetToString to ++ temporalQuantifierToString tq ++ "THEDATE" ++ subjectToString the
            in EventD eventStr   
        baobaSomeDate tq to tq1 some =
            case (tq) of
                (TempAfter) -> afterCase
                (TempBefore) -> beforeCase
            where
                afterCase =
                    let eventStr = subjectToString subject ++ "DIDN'T" ++ verbToString verb ++ " to " ++ receiverToString receiver
                    in EventD eventStr
                beforeCase = 
                    let eventStr = "Occurrence of " ++ temporalOffsetToString to ++ " " ++ temporalQuantifierToString tq1 ++ " SOMEDATE " ++ subjectToString some
                    in EventD eventStr
        baobaTheDate tq to tq1 the =
            case (tq) of
                (TempAfter) -> afterCase
                (TempBefore) -> beforeCase
            where
                afterCase =
                    let eventStr = subjectToString subject ++ "DIDN'T" ++ verbToString verb ++ " to " ++ receiverToString receiver
                    in EventD eventStr
                beforeCase = 
                    let eventStr = "Occurrence of " ++ temporalOffsetToString to ++ " " ++ temporalQuantifierToString tq1 ++ " THEDATE " ++ subjectToString the
                    in EventD eventStr

contractToDFA :: Contract -> State (StateDictionary, SelfLoopingEvents) DFA
contractToDFA (ConEmpty) = return $ DFA 
    { states = Set.singleton $ StateAD "Empty"
    , events = Set.empty
    , transitions = Set.empty
    , startStates = Set.singleton $ StateAD "Empty"
    , acceptingStates = Set.singleton $ StateAD "Empty"
    }
contractToDFA (ConComp component) = componentToDFA component
contractToDFA (ConAnd component contract) = do
    dfa1 <- componentToDFA component
    dfa2 <- contractToDFA contract
  
    let combinedStates = Set.union (states dfa1) (states dfa2)
        combinedEvents = Set.union (events dfa1) (events dfa2)
        combinedTransitions = Set.union (transitions dfa1) (transitions dfa2)
        endStates1 = acceptingStates dfa1
        endStates2 = acceptingStates dfa2

    return $ DFA
      { states = Set.union (combinedStates) (Set.singleton $ StateAD "Start")
      , events = combinedEvents
      , transitions = combinedTransitions
      , startStates = Set.singleton $ StateAD "Start"
      , acceptingStates = Set.union (endStates1) (endStates2)
      }

componentToDFA :: Component -> State (StateDictionary, SelfLoopingEvents) DFA
componentToDFA (ComDef definition) = definitionToDFA definition
componentToDFA (ComConDef conditionalDefinition) = conditionalDefinitionToDFA conditionalDefinition
componentToDFA (ComState statement) = statementToDFA statement
componentToDFA (ComConState conditionalStatement) = conditionalStatementToDFA conditionalStatement

definitionToDFA :: Definition -> State (StateDictionary, SelfLoopingEvents) DFA
definitionToDFA (DefSim simpleDefinition) = simpleDefinitionToDFA simpleDefinition
definitionToDFA (DefAnd simpleDefinition definition) = do
    dfa1 <- simpleDefinitionToDFA simpleDefinition
    dfa2 <- definitionToDFA definition

    let events1 = events dfa1
        events2 = events dfa2
        combinedEvent = generateAndEvent events1 events2

    addSelfLoopingEvents combinedEvent

    return $ DFA
      { states = Set.singleton $ StateAD "Start"
      , events = Set.singleton (combinedEvent)
      , transitions = Set.empty
      , startStates = Set.singleton $ StateAD "Start"
      , acceptingStates = Set.empty
      }

conditionalDefinitionToDFA :: ConditionalDefinition -> State (StateDictionary, SelfLoopingEvents) DFA
conditionalDefinitionToDFA (ConDefIf definition condition) = do
    dfaCon <- conditionToDFA condition
    dfaDef <- definitionToDFA definition
    oriDict <- gets fst
    oriList <- gets snd

    let combinedEvents = Set.union (events dfaCon) (events dfaDef)
        conditionStates = states dfaCon
        allConditionEvents = events dfaCon
        conditionEvents = getNonBreachEvents allConditionEvents oriDict
        definitionEvents = events dfaDef
        definitionState = evalState (generateThenState' conditionEvents definitionEvents) (oriDict, oriList)
        conditionState = getNonBreachState conditionStates oriDict

    addToStateDictionary definitionState definitionEvents 1

    newTransitions <- generateTransition conditionState definitionState

    return $ DFA
        { states = Set.unions [(conditionStates), (Set.singleton $ StateAD "Start"), (Set.singleton $ definitionState)]
        , events = combinedEvents
        , transitions = Set.union (transitions dfaCon) (Set.fromList newTransitions)
        , startStates = Set.singleton $ StateAD "Start"
        , acceptingStates = Set.singleton $ definitionState
        }

conditionalDefinitionToDFA (ConDefIfThen condition definition) = do
    dfaCon <- conditionToDFA condition
    dfaDef <- definitionToDFA definition
    oriDict <- gets fst
    oriList <- gets snd

    let combinedEvents = Set.union (events dfaCon) (events dfaDef)
        conditionStates = states dfaCon
        allConditionEvents = events dfaCon
        conditionEvents = getNonBreachEvents allConditionEvents oriDict
        definitionEvents = events dfaDef
        definitionState = evalState (generateThenState' conditionEvents definitionEvents) (oriDict, oriList)
        conditionState = getNonBreachState conditionStates oriDict

    addToStateDictionary definitionState definitionEvents 1

    newTransitions <- generateTransition conditionState definitionState

    return $ DFA
        { states = Set.unions [(conditionStates), (Set.singleton $ StateAD "Start"), (Set.singleton $ definitionState)]
        , events = combinedEvents
        , transitions = Set.union (transitions dfaCon) (Set.fromList newTransitions)
        , startStates = Set.singleton $ StateAD "Start"
        , acceptingStates = Set.singleton $ definitionState
        }

conditionalDefinitionToDFA (ConDefIfElse definition1 condition definition2) = do
    dfaCon <- conditionToDFA condition
    dfaDef1 <- definitionToDFA definition1
    dfaDef2 <- definitionToDFA definition2
    oriDict <- gets fst
    oriList <- gets snd

    let combinedEvents = Set.unions [(events dfaCon), (events dfaDef1), (events dfaDef2)]
        conditionStates = states dfaCon
        allConditionEvents = events dfaCon

        conditionState = getNonBreachState conditionStates oriDict
        conditionEvents = getNonBreachEvents allConditionEvents oriDict
        definitionEvents1 = events dfaDef1
        definitionState1 = evalState (generateThenState' conditionEvents definitionEvents1) (oriDict, oriList)
        
        nonConditionState = getBreachState conditionStates oriDict
        nonConditionEvents = getBreachEvents allConditionEvents oriDict
        definitionEvents2 = events dfaDef2
        definitionState2 = evalState (generateThenState' nonConditionEvents definitionEvents2) (oriDict, oriList)

    addToStateDictionary definitionState1 definitionEvents1 1
    addToStateDictionary definitionState2 definitionEvents2 1

    newTransitions1 <- generateTransition conditionState definitionState1
    newTransitions2 <- generateTransition nonConditionState definitionState2

    return $ DFA
        { states = Set.unions [(conditionStates), (Set.singleton $ StateAD "Start"), (Set.singleton $ definitionState1), (Set.singleton $ definitionState2)]
        , events = combinedEvents
        , transitions = Set.unions [(transitions dfaCon), (Set.fromList newTransitions1), (Set.fromList newTransitions2)]
        , startStates = Set.singleton $ StateAD "Start"
        , acceptingStates = Set.union (Set.singleton $ definitionState1) (Set.singleton $ definitionState2)
        }
        
conditionalDefinitionToDFA (ConDefIfThenElse condition definition1 definition2) = do
    dfaCon <- conditionToDFA condition
    dfaDef1 <- definitionToDFA definition1
    dfaDef2 <- definitionToDFA definition2
    oriDict <- gets fst
    oriList <- gets snd

    let combinedEvents = Set.unions [(events dfaCon), (events dfaDef1), (events dfaDef2)]
        conditionStates = states dfaCon
        allConditionEvents = events dfaCon

        conditionState = getNonBreachState conditionStates oriDict
        conditionEvents = getNonBreachEvents allConditionEvents oriDict
        definitionEvents1 = events dfaDef1
        definitionState1 = evalState (generateThenState' conditionEvents definitionEvents1) (oriDict, oriList)
        
        nonConditionState = getBreachState conditionStates oriDict
        nonConditionEvents = getBreachEvents allConditionEvents oriDict
        definitionEvents2 = events dfaDef2
        definitionState2 = evalState (generateThenState' nonConditionEvents definitionEvents2) (oriDict, oriList)

    addToStateDictionary definitionState1 definitionEvents1 1
    addToStateDictionary definitionState2 definitionEvents2 1

    newTransitions1 <- generateTransition conditionState definitionState1
    newTransitions2 <- generateTransition nonConditionState definitionState2

    return $ DFA
        { states = Set.unions [(conditionStates), (Set.singleton $ StateAD "Start"), (Set.singleton $ definitionState1), (Set.singleton $ definitionState2)]
        , events = combinedEvents
        , transitions = Set.unions [(transitions dfaCon), (Set.fromList newTransitions1), (Set.fromList newTransitions2)]
        , startStates = Set.singleton $ StateAD "Start"
        , acceptingStates = Set.union (Set.singleton $ definitionState1) (Set.singleton $ definitionState2)
        }

statementToDFA :: Statement -> State (StateDictionary, SelfLoopingEvents) DFA
statementToDFA (StateSim simpleStatement) = simpleStatementToDFA simpleStatement
statementToDFA (StateOr simpleStatement statement) = do
    dfa1 <- simpleStatementToDFA simpleStatement
    dfa2 <- statementToDFA statement

    let combinedStates = Set.union (states dfa1) (states dfa2)
        combinedTransitions = Set.union (transitions dfa1) (transitions dfa2)
        combinedAccepting = Set.union (acceptingStates dfa1) (acceptingStates dfa2)
        combinedEvents = Set.union (events dfa1) (events dfa2)

    return $ DFA
        { states = combinedStates
        , events = combinedEvents
        , transitions = combinedTransitions
        , startStates = Set.singleton $ StateAD "Start"
        , acceptingStates = combinedAccepting
        }
statementToDFA (StateAnd simpleStatement statement) = do
    dfa1 <- simpleStatementToDFA simpleStatement
    dfa2 <- statementToDFA statement

    let combinedStates = Set.union (states dfa1) (states dfa2)
        combinedEvents = Set.union (events dfa1) (events dfa2)
        combinedTransitions = Set.union (transitions dfa1) (transitions dfa2)
        combinedAccepting = Set.union (acceptingStates dfa1) (acceptingStates dfa2)

    return $ DFA
        { states = combinedStates
        , events = combinedEvents
        , transitions = combinedTransitions
        , startStates = Set.singleton $ StateAD "Start"
        , acceptingStates = combinedAccepting
        }

statementToDFA' :: Statement -> State (StateDictionary, SelfLoopingEvents) DFA
statementToDFA' (StateSim simpleStatement) = simpleStatementToDFA simpleStatement
statementToDFA' (StateOr simpleStatement statement) = do
    dfa1 <- simpleStatementToDFA simpleStatement
    dfa2 <- statementToDFA statement

    let combinedStates = Set.union (states dfa1) (states dfa2)
        combinedTransitions = Set.union (transitions dfa1) (transitions dfa2)
        combinedAccepting = Set.union (acceptingStates dfa1) (acceptingStates dfa2)
        combinedEvents = Set.union (events dfa1) (events dfa2)

    return $ DFA
        { states = combinedStates
        , events = combinedEvents
        , transitions = combinedTransitions
        , startStates = Set.singleton $ StateAD "Start"
        , acceptingStates = combinedAccepting
        }
statementToDFA' (StateAnd simpleStatement statement) = 
    case statement of 
        StateSim _ -> caseOne
        StateOr _ _ -> caseTwo
        StateAnd _ _ -> caseOne
    where
        caseOne = do
            dfa1 <- simpleStatementToDFA simpleStatement
            dfa2 <- statementToDFA statement
            oriDict <- gets fst

            let combinedStates = Set.union (states dfa1) (states dfa2)
                combinedEvents = Set.union (events dfa1) (events dfa2)
                combinedTransitions = Set.union (transitions dfa1) (transitions dfa2)
                statementStatesNB = getNonBreachStates combinedStates oriDict
                statementEventsNB = getNonBreachEvents combinedEvents oriDict
                statementStatesB = getBreachStates combinedStates oriDict
                statementEventsB = getBreachEvents combinedEvents oriDict

                combinedStateNB = generateAndState statementStatesNB Set.empty
                combinedEventNB = generateAndEvent statementEventsNB Set.empty

                cleanedTransitions = removeNonBreachTransitions combinedTransitions statementEventsNB

            addToStateDictionary combinedStateNB (Set.singleton $ combinedEventNB) 1
            removeFromStateDictionary statementStatesNB

            return $ DFA
                { states = Set.union (statementStatesB) (Set.singleton combinedStateNB)
                , events = Set.union (statementEventsB) (Set.singleton combinedEventNB)
                , transitions = cleanedTransitions
                , startStates = Set.singleton $ StateAD "Start"
                , acceptingStates = Set.empty
                }
        caseTwo = do
            dfa1 <- simpleStatementToDFA simpleStatement
            dfa2 <- statementToDFA statement
            oriDict <- gets fst

            let statementStates1NB = getNonBreachStates (states dfa1) oriDict
                statementEvents1NB = getNonBreachEvents (events dfa1) oriDict
                statementStates2NB = getNonBreachStates (states dfa2) oriDict
                statementEvents2NB = getNonBreachEvents (events dfa2) oriDict

                statementStates2NB1 = if Set.null statementStates2NB
                                        then Set.empty
                                        else Set.singleton (head (Set.toList statementStates2NB))
                statementStates2NB2 = case Set.toList statementStates2NB of
                                        [_, lastState] -> Set.singleton lastState
                                        _              -> Set.empty
                combinedStateNB1 = generateAndState statementStates1NB statementStates2NB1
                combinedStateNB2 = generateAndState statementStates1NB statementStates2NB2

                statementEvents2NB1 = if Set.null statementEvents2NB
                                        then Set.empty
                                        else Set.singleton (head (Set.toList statementEvents2NB))
                statementEvents2NB2 = case Set.toList statementEvents2NB of
                                        [_, lastState] -> Set.singleton lastState
                                        _              -> Set.empty
                combinedEventNB1 = generateAndEvent statementEvents1NB statementEvents2NB1
                combinedEventNB2 = generateAndEvent statementEvents1NB statementEvents2NB2

                combinedStates = Set.union (states dfa1) (states dfa2)
                combinedEvents = Set.union (events dfa1) (events dfa2)
                statementStatesB = getBreachStates combinedStates oriDict
                statementEventsB = getBreachEvents combinedEvents oriDict
                combinedTransitions = Set.union (transitions dfa1) (transitions dfa2)
                cleanedTransitions = removeNonBreachTransitions combinedTransitions (Set.union (statementEvents1NB) (statementEvents2NB))
                statementStatesNB = getNonBreachStates combinedStates oriDict

            addToStateDictionary combinedStateNB1 (Set.singleton $ combinedEventNB1) 1
            addToStateDictionary combinedStateNB2 (Set.singleton $ combinedEventNB2) 1

            removeFromStateDictionary statementStatesNB

            return $ DFA
                { states = Set.unions [(statementStatesB), (Set.singleton combinedStateNB1), (Set.singleton combinedStateNB1)]
                , events = Set.unions [(statementEventsB), (Set.singleton combinedEventNB1), (Set.singleton combinedEventNB1)]
                , transitions = cleanedTransitions
                , startStates = Set.singleton $ StateAD "Start"
                , acceptingStates = Set.empty
                }

conditionalStatementToDFA :: ConditionalStatement -> State (StateDictionary, SelfLoopingEvents) DFA
conditionalStatementToDFA (ConStateIf statement condition) = do
    dfaCon <- conditionToDFA condition
    dfaState <- statementToDFA' statement
    oriDict <- gets fst
    oriList <- gets snd

    let conditionStates = states dfaCon
        allConditionEvents = events dfaCon

        conditionState = getNonBreachState conditionStates oriDict
        conditionEvents = getNonBreachEvents allConditionEvents oriDict
        statementEvents = getNonBreachEvents (events dfaState) oriDict
        statementStates = getNonBreachStates (states dfaState) oriDict
        statementState = generateThenState conditionState statementStates
        
        statementBreachEvents = getBreachEvents (events dfaState) oriDict
        statementBreachStates = getBreachStates (states dfaState) oriDict
        statementBreachState = generateThenState conditionState statementBreachStates

    addToStateDictionary statementState statementEvents 1
    addToStateDictionary statementBreachState statementBreachEvents 2
        
    newTransitions1 <- generateTransition conditionState statementState
    newTransitions2 <- generateTransition conditionState statementBreachState 

    return $ DFA
        { states = Set.unions [(conditionStates), (Set.singleton $ StateAD "Start"), (Set.singleton $ statementState), (Set.singleton $ statementBreachState)]
        , events = Set.unions [(allConditionEvents), (statementEvents), (statementBreachEvents)]
        , transitions = Set.unions [(transitions dfaCon), (Set.fromList newTransitions1), (Set.fromList newTransitions2)]
        , startStates = Set.singleton $ StateAD "Start"
        , acceptingStates = Set.singleton $ statementState
        }

conditionalStatementToDFA (ConStateIfThen condition statement) = do
    dfaCon <- conditionToDFA condition
    dfaState <- statementToDFA' statement
    oriDict <- gets fst
    oriList <- gets snd

    let conditionStates = states dfaCon
        allConditionEvents = events dfaCon

        conditionState = getNonBreachState conditionStates oriDict
        conditionEvents = getNonBreachEvents allConditionEvents oriDict
        statementEvents = getNonBreachEvents (events dfaState) oriDict
        statementStates = getNonBreachStates (states dfaState) oriDict
        statementState = generateThenState conditionState statementStates
        
        statementBreachEvents = getBreachEvents (events dfaState) oriDict
        statementBreachStates = getBreachStates (states dfaState) oriDict
        statementBreachState = generateThenState conditionState statementBreachStates

    addToStateDictionary statementState statementEvents 1
    addToStateDictionary statementBreachState statementBreachEvents 2
        
    newTransitions1 <- generateTransition conditionState statementState
    newTransitions2 <- generateTransition conditionState statementBreachState 

    return $ DFA
        { states = Set.unions [(conditionStates), (Set.singleton $ StateAD "Start"), (Set.singleton $ statementState), (Set.singleton $ statementBreachState)]
        , events = Set.unions [(allConditionEvents), (statementEvents), (statementBreachEvents)]
        , transitions = Set.unions [(transitions dfaCon), (Set.fromList newTransitions1), (Set.fromList newTransitions2)]
        , startStates = Set.singleton $ StateAD "Start"
        , acceptingStates = Set.singleton $ statementState
        }

conditionalStatementToDFA (ConStateIfElse statement1 condition statement2) = do
    dfaCon <- conditionToDFA condition
    dfaState1 <- statementToDFA' statement1
    dfaState2 <- statementToDFA' statement2
    oriDict <- gets fst
    oriList <- gets snd

    let conditionStates = states dfaCon
        allConditionEvents = events dfaCon

        conditionState = getNonBreachState conditionStates oriDict
        conditionEvents = getNonBreachEvents allConditionEvents oriDict
        statementEvents1 = getNonBreachEvents (events dfaState1) oriDict
        statementStates1 = getNonBreachStates (states dfaState1) oriDict
        statementState1 = generateThenState conditionState statementStates1
        
        statementBreachEvents1 = getBreachEvents (events dfaState1) oriDict
        statementBreachStates1 = getBreachStates (states dfaState1) oriDict
        statementBreachState1 = generateThenState conditionState statementBreachStates1

        nonConditionState = getBreachState conditionStates oriDict
        nonConditionEvents = getBreachEvents allConditionEvents oriDict
        statementEvents2 = getNonBreachEvents (events dfaState2) oriDict
        statementStates2 = getNonBreachStates (states dfaState2) oriDict
        statementState2 = generateThenState nonConditionState statementStates2

        statementBreachEvents2 = getBreachEvents (events dfaState2) oriDict
        statementBreachStates2 = getBreachStates (states dfaState2) oriDict
        statementBreachState2 = generateThenState nonConditionState statementBreachStates2

    addToStateDictionary statementState1 statementEvents1 1
    addToStateDictionary statementBreachState1 statementBreachEvents1 2
    addToStateDictionary statementState2 statementEvents2 1
    addToStateDictionary statementBreachState2 statementBreachEvents2 2
        
    newTransitions1 <- generateTransition conditionState statementState1
    newTransitions2 <- generateTransition conditionState statementBreachState1
    newTransitions3 <- generateTransition nonConditionState statementState2
    newTransitions4 <- generateTransition nonConditionState statementBreachState2

    return $ DFA
        { states = Set.unions [(conditionStates), (Set.singleton $ StateAD "Start"), (Set.singleton $ statementState1), (Set.singleton $ statementBreachState1), (Set.singleton $ statementState2), (Set.singleton $ statementBreachState2)]
        , events = Set.unions [(allConditionEvents), (statementEvents1), (statementBreachEvents1), (statementEvents2), (statementBreachEvents2)]
        , transitions = Set.unions [(transitions dfaCon), (Set.fromList newTransitions1), (Set.fromList newTransitions2), (Set.fromList newTransitions3), (Set.fromList newTransitions4)]
        , startStates = Set.singleton $ StateAD "Start"
        , acceptingStates = Set.union (Set.singleton $ statementState1) (Set.singleton $ statementState2)
        }

conditionalStatementToDFA (ConStateIfThenElse condition statement1 statement2) = do
    dfaCon <- conditionToDFA condition
    dfaState1 <- statementToDFA' statement1
    dfaState2 <- statementToDFA' statement2
    oriDict <- gets fst
    oriList <- gets snd

    let conditionStates = states dfaCon
        allConditionEvents = events dfaCon

        conditionState = getNonBreachState conditionStates oriDict
        conditionEvents = getNonBreachEvents allConditionEvents oriDict
        statementEvents1 = getNonBreachEvents (events dfaState1) oriDict
        statementStates1 = getNonBreachStates (states dfaState1) oriDict
        statementState1 = generateThenState conditionState statementStates1
        
        statementBreachEvents1 = getBreachEvents (events dfaState1) oriDict
        statementBreachStates1 = getBreachStates (states dfaState1) oriDict
        statementBreachState1 = generateThenState conditionState statementBreachStates1

        nonConditionState = getBreachState conditionStates oriDict
        nonConditionEvents = getBreachEvents allConditionEvents oriDict
        statementEvents2 = getNonBreachEvents (events dfaState2) oriDict
        statementStates2 = getNonBreachStates (states dfaState2) oriDict
        statementState2 = generateThenState nonConditionState statementStates2

        statementBreachEvents2 = getBreachEvents (events dfaState2) oriDict
        statementBreachStates2 = getBreachStates (states dfaState2) oriDict
        statementBreachState2 = generateThenState nonConditionState statementBreachStates2

    addToStateDictionary statementState1 statementEvents1 1
    addToStateDictionary statementBreachState1 statementBreachEvents1 2
    addToStateDictionary statementState2 statementEvents2 1
    addToStateDictionary statementBreachState2 statementBreachEvents2 2
        
    newTransitions1 <- generateTransition conditionState statementState1
    newTransitions2 <- generateTransition conditionState statementBreachState1
    newTransitions3 <- generateTransition nonConditionState statementState2
    newTransitions4 <- generateTransition nonConditionState statementBreachState2

    return $ DFA
        { states = Set.unions [(conditionStates), (Set.singleton $ StateAD "Start"), (Set.singleton $ statementState1), (Set.singleton $ statementBreachState1), (Set.singleton $ statementState2), (Set.singleton $ statementBreachState2)]
        , events = Set.unions [(allConditionEvents), (statementEvents1), (statementBreachEvents1), (statementEvents2), (statementBreachEvents2)]
        , transitions = Set.unions [(transitions dfaCon), (Set.fromList newTransitions1), (Set.fromList newTransitions2), (Set.fromList newTransitions3), (Set.fromList newTransitions4)]
        , startStates = Set.singleton $ StateAD "Start"
        , acceptingStates = Set.union (Set.singleton $ statementState1) (Set.singleton $ statementState2)
        }

simpleDefinitionToDFA :: SimpleDefinition -> State (StateDictionary, SelfLoopingEvents) DFA
simpleDefinitionToDFA (SimDefIs id subject1 subject2) = do
    let subjectStr1 = subjectToString subject1
        subjectStr2 = subjectToString subject2
        eventStr = subjectStr1 ++ " IS " ++ subjectStr2

        simDefEvent = EventD eventStr

    addSelfLoopingEvents simDefEvent

    return $ DFA
        { states = Set.singleton $ StateAD "Start"
        , events = Set.singleton simDefEvent
        , transitions = Set.empty
        , startStates = Set.singleton $ StateAD "Start"
        , acceptingStates = Set.empty
        }
simpleDefinitionToDFA (SimDefEq id subject numericalExpression) = do
    let subjectStr = subjectToString subject
        numExpStr = numericalExpressionToString numericalExpression
        eventStr = subjectStr ++ " EQUALS " ++ numExpStr

        simDefEvent = EventD eventStr

    addSelfLoopingEvents simDefEvent

    return $ DFA
        { states = Set.singleton $ StateAD "Start"
        , events = Set.singleton simDefEvent
        , transitions = Set.empty
        , startStates = Set.singleton $ StateAD "Start"
        , acceptingStates = Set.empty
        }
simpleDefinitionToDFA (SimDefDate id subject day month year) = do
    let subjectStr = subjectToString subject
        dateStr = dateSpeToString day month year
        eventStr = subjectStr ++ " IS " ++ dateStr

        simDefEvent = EventD eventStr

    addSelfLoopingEvents simDefEvent

    return $ DFA
        { states = Set.singleton $ StateAD "Start"
        , events = Set.singleton simDefEvent
        , transitions = Set.empty
        , startStates = Set.singleton $ StateAD "Start"
        , acceptingStates = Set.empty
        }

conditionToDFA :: Condition -> State (StateDictionary, SelfLoopingEvents) DFA
conditionToDFA (CondiSim simpleCondition) = simpleConditionToDFA simpleCondition
conditionToDFA (CondiOr simpleCondition condition) = do
    dfa1 <- simpleConditionToDFA simpleCondition
    dfa2 <- conditionToDFA condition
    oriDict <- gets fst

    let combinedStates = Set.union (states dfa1) (states dfa2)
        combinedEvents = Set.union (events dfa1) (events dfa2)

        allNonBreachStates = getNonBreachStates combinedStates oriDict
        allNonBreachEvents = getNonBreachEvents combinedEvents oriDict
        combinedNonBreachState = generateOrState allNonBreachStates Set.empty

        allBreachStates = getBreachStates combinedStates oriDict
        allBreachEvents = getBreachEvents combinedEvents oriDict
        combinedBreachState = generateAndState allBreachStates Set.empty
        combinedBreachEvent = generateAndEvent allBreachEvents Set.empty

    addToStateDictionary combinedNonBreachState allNonBreachEvents 1
    addToStateDictionary combinedBreachState (Set.singleton $ combinedBreachEvent) 2
        
    newTransitions1 <- generateTransition (StateAD "Start") combinedNonBreachState
    newTransitions2 <- generateTransition (StateAD "Start") combinedBreachState

    return $ DFA
        { states = Set.fromList [combinedBreachState, combinedNonBreachState, StateAD "Start"]
        , events = Set.union (allNonBreachEvents) (Set.singleton combinedBreachEvent)
        , transitions = Set.union (Set.fromList newTransitions1) (Set.fromList newTransitions2)
        , startStates = Set.singleton $ StateAD "Start"
        , acceptingStates = Set.empty
        }

conditionToDFA (CondiAnd simpleCondition condition) = do
    dfa1 <- simpleConditionToDFA simpleCondition
    dfa2 <- conditionToDFA condition
    oriDict <- gets fst

    let combinedStates = Set.union (states dfa1) (states dfa2)
        combinedEvents = Set.union (events dfa1) (events dfa2)

        allNonBreachStates = getNonBreachStates combinedStates oriDict
        allNonBreachEvents = getNonBreachEvents combinedEvents oriDict
        combinedNonBreachState = generateAndState allNonBreachStates Set.empty
        combinedNonBreachEvent = generateAndEvent allNonBreachEvents Set.empty

        allBreachStates = getBreachStates combinedStates oriDict
        allBreachEvents = getNonBreachEvents combinedEvents oriDict
        combinedBreachState = generateOrState allBreachStates Set.empty

    addToStateDictionary combinedNonBreachState (Set.singleton $ combinedNonBreachEvent) 1
    addToStateDictionary combinedBreachState allBreachEvents 2
        
    newTransitions1 <- generateTransition (StateAD "Start") combinedNonBreachState
    newTransitions2 <- generateTransition (StateAD "Start") combinedBreachState

    return $ DFA
        { states = Set.fromList [combinedBreachState, combinedNonBreachState, StateAD "Start"]
        , events = Set.union (Set.singleton $ combinedNonBreachEvent) allBreachEvents
        , transitions = Set.union (Set.fromList newTransitions1) (Set.fromList newTransitions2)
        , startStates = Set.singleton $ StateAD "Start"
        , acceptingStates = Set.empty
        }

simpleStatementToDFA :: SimpleStatement -> State (StateDictionary, SelfLoopingEvents) DFA
simpleStatementToDFA (SimStateOne id holds subject modalVerb verb object receiver date) = 
    createDFASimpleStatement holds subject modalVerb verb object receiver date
simpleStatementToDFA (SimStateTwo id holds subject date modalVerb verb object receiver) =
    createDFASimpleStatement holds subject modalVerb verb object receiver date
simpleStatementToDFA (SimStateThree id holds date subject modalVerb verb object receiver) =
    createDFASimpleStatement holds subject modalVerb verb object receiver date
simpleStatementToDFA (SimStateFour id holds subject verbStatus object receiver date) =
    createDFASimpleCondition holds subject verbStatus object receiver date
simpleStatementToDFA (SimStateOneNH id subject modalVerb verb object receiver date) = 
    createDFASimpleStatementNH subject modalVerb verb object receiver date
simpleStatementToDFA (SimStateTwoNH id subject date modalVerb verb object receiver) =
    createDFASimpleStatementNH subject modalVerb verb object receiver date
simpleStatementToDFA (SimStateThreeNH id date subject modalVerb verb object receiver) =
    createDFASimpleStatementNH subject modalVerb verb object receiver date
simpleStatementToDFA (SimStateFourNH id subject verbStatus object receiver date) =
    createDFASimpleConditionNH subject verbStatus object receiver date

createDFASimpleStatement :: Holds -> Subject -> ModalVerb -> Verb -> Object -> Receiver -> Date -> State (StateDictionary, SelfLoopingEvents) DFA
createDFASimpleStatement holds subject modalVerb verb object receiver date = 
    case (holds, modalVerb) of
         (HoldYes, ModalObli _) -> must
         (HoldNo, ModalObli _) -> may
         (HoldYes, ModalPermi) -> may
         (HoldNo, ModalPermi) -> mustNot
         (HoldYes, ModalForbi) -> mustNot
         (HoldNo, ModalForbi) -> may

    where
        mustStateStrNB = objectToString object ++ " " ++ verbToVerbStatusString verb ++ " by " ++ subjectToString subject ++ " to " ++ receiverToString receiver ++ " on time"
        mustEventStrNB = subjectToString subject ++ " " ++ verbToVerbStatusString verb ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        mustStateStrB = "BREACH: " ++ objectToString object ++ " not " ++ verbToVerbStatusString verb ++ " by " ++ subjectToString subject ++ " to " ++ receiverToString receiver ++ " on time"
        mustEventB = generateBreachEvent subject verb object receiver date
        mustStateNB = StateAD mustStateStrNB
        mustEventNB = EventD mustEventStrNB
        mustStateB = StateAD mustStateStrB

        mayEventStr = subjectToString subject ++ " " ++ verbToVerbStatusString verb ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        mayEvent = EventD mayEventStr

        -- mustNotStateStrNB = objectToString object ++ " not " ++ verbToVerbStatusString verb ++ " by " ++ subjectToString subject ++ " to " ++ receiverToString receiver
        -- mustNotEventStrNB = subjectToString subject ++ " DIDN'T " ++ verbToString verb ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        mustNotStateStrB = "BREACH: " ++ objectToString object ++ " " ++ verbToVerbStatusString verb ++ " by " ++ subjectToString subject ++ " to " ++ receiverToString receiver
        mustNotEventStrB = subjectToString subject ++ " " ++ verbToVerbStatusString verb ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        -- mustNotStateNB = StateAD mustNotStateStrNB
        -- mustNotEventNB = EventD mustNotEventStrNB
        mustNotStateB = StateAD mustNotStateStrB
        mustNotEventB = EventD mustNotEventStrB

        must = do
            addToStateDictionary mustStateNB (Set.singleton $ mustEventNB) 1
            addToStateDictionary mustStateB (Set.singleton $ mustEventB) 2
            newTransitions1 <- generateTransition (StateAD "Start") mustStateNB
            newTransitions2 <- generateTransition (StateAD "Start") mustStateB
            return $ DFA
                { states = Set.fromList [mustStateNB, mustStateB, StateAD "Start"]
                , events = Set.fromList [mustEventNB, mustEventB]
                , transitions = Set.union (Set.fromList newTransitions1) (Set.fromList newTransitions2)
                , startStates = Set.singleton $ StateAD "Start"
                , acceptingStates = Set.singleton mustStateNB
                }
    
        may = do
            addSelfLoopingEvents mayEvent
            return $ DFA
                { states = Set.singleton $ StateAD "Start"
                , events = Set.singleton $ mayEvent
                , transitions = Set.empty
                , startStates = Set.singleton $ StateAD "Start"
                , acceptingStates = Set.empty
                }
    
        mustNot = do
            -- addToStateDictionary mustNotStateNB (Set.singleton $ mustNotEventNB) 1
            addToStateDictionary mustNotStateB (Set.singleton $ mustNotEventB) 3
            -- newTransitions1 <- generateTransition (StateAD "Start") mustNotStateNB
            newTransitions2 <- generateTransition (StateAD "Start") mustNotStateB
            -- return $ DFA
            --     { states = Set.fromList [mustNotStateNB, mustNotStateB, StateAD "Start"]
            --     , events = Set.fromList [mustNotEventNB, mustNotEventB]
            --     , transitions = Set.union (Set.fromList newTransitions1) (Set.fromList newTransitions2)
            --     , startStates = Set.singleton $ StateAD "Start"
            --     , acceptingStates = Set.singleton mustNotStateNB
            --     }
            return $ DFA
                { states = Set.fromList [mustNotStateB, StateAD "Start"]
                , events = Set.fromList [mustNotEventB]
                , transitions = Set.fromList newTransitions2
                , startStates = Set.singleton $ StateAD "Start"
                , acceptingStates = Set.empty
                }

createDFASimpleStatementNH :: Subject -> ModalVerb -> Verb -> Object -> Receiver -> Date ->  State (StateDictionary, SelfLoopingEvents) DFA
createDFASimpleStatementNH subject modalVerb verb object receiver date = 
    case (modalVerb) of
         (ModalObli _) -> must
         (ModalPermi) -> may
         (ModalForbi) -> mustNot

    where
        mustStateStrNB = objectToString object ++ " " ++ verbToVerbStatusString verb ++ " by " ++ subjectToString subject ++ " to " ++ receiverToString receiver ++ " on time"
        mustEventStrNB = subjectToString subject ++ " " ++ verbToVerbStatusString verb ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        mustStateStrB = "BREACH: " ++ objectToString object ++ " not " ++ verbToVerbStatusString verb ++ " by " ++ subjectToString subject ++ " to " ++ receiverToString receiver ++ " on time"
        mustEventB = generateBreachEvent subject verb object receiver date
        mustStateNB = StateAD mustStateStrNB
        mustEventNB = EventD mustEventStrNB
        mustStateB = StateAD mustStateStrB

        mayEventStr = subjectToString subject ++ " " ++ verbToVerbStatusString verb ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        mayEvent = EventD mayEventStr

        -- mustNotStateStrNB = objectToString object ++ " not " ++ verbToVerbStatusString verb ++ " by " ++ subjectToString subject ++ " to " ++ receiverToString receiver
        -- mustNotEventStrNB = subjectToString subject ++ " DIDN'T " ++ verbToString verb ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        mustNotStateStrB = "BREACH: " ++ objectToString object ++ " " ++ verbToVerbStatusString verb ++ " by " ++ subjectToString subject ++ " to " ++ receiverToString receiver
        mustNotEventStrB = subjectToString subject ++ " " ++ verbToVerbStatusString verb ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        -- mustNotStateNB = StateAD mustNotStateStrNB
        -- mustNotEventNB = EventD mustNotEventStrNB
        mustNotStateB = StateAD mustNotStateStrB
        mustNotEventB = EventD mustNotEventStrB

        must = do
            addToStateDictionary mustStateNB (Set.singleton $ mustEventNB) 1
            addToStateDictionary mustStateB (Set.singleton $ mustEventB) 2
            newTransitions1 <- generateTransition (StateAD "Start") mustStateNB
            newTransitions2 <- generateTransition (StateAD "Start") mustStateB
            return $ DFA
                { states = Set.fromList [mustStateNB, mustStateB, StateAD "Start"]
                , events = Set.fromList [mustEventNB, mustEventB]
                , transitions = Set.union (Set.fromList newTransitions1) (Set.fromList newTransitions2)
                , startStates = Set.singleton $ StateAD "Start"
                , acceptingStates = Set.singleton mustStateNB
                }

        may = do
            addSelfLoopingEvents mayEvent
            return $ DFA
                { states = Set.singleton $ StateAD "Start"
                , events = Set.singleton $ mayEvent
                , transitions = Set.empty
                , startStates = Set.singleton $ StateAD "Start"
                , acceptingStates = Set.empty
                }

        mustNot = do
            -- addToStateDictionary mustNotStateNB (Set.singleton $ mustNotEventNB) 1
            addToStateDictionary mustNotStateB (Set.singleton $ mustNotEventB) 3
            -- newTransitions1 <- generateTransition (StateAD "Start") mustNotStateNB
            newTransitions2 <- generateTransition (StateAD "Start") mustNotStateB
            return $ DFA
                { states = Set.fromList [mustNotStateB, StateAD "Start"]
                , events = Set.fromList [mustNotEventB]
                , transitions = Set.fromList newTransitions2
                , startStates = Set.singleton $ StateAD "Start"
                , acceptingStates = Set.empty
                }

simpleConditionToDFA :: SimpleCondition -> State (StateDictionary, SelfLoopingEvents) DFA
simpleConditionToDFA (SimConOne id holds subject verbStatus object receiver date) =
    createDFASimpleCondition holds subject verbStatus object receiver date
simpleConditionToDFA (SimConTwo id holds subject date verbStatus object receiver) =
    createDFASimpleCondition holds subject verbStatus object receiver date
simpleConditionToDFA (SimConThree id holds date subject verbStatus object receiver) =
    createDFASimpleCondition holds subject verbStatus object receiver date
simpleConditionToDFA (SimConFour id holds subject modalVerb verb object receiver date) =
    createDFASimpleStatement holds subject modalVerb verb object receiver date
simpleConditionToDFA (SimConFive id holds booleanExpression) =
    createDFABooleanExpression holds booleanExpression
simpleConditionToDFA (SimConOneNH id subject verbStatus object receiver date) =
    createDFASimpleConditionNH subject verbStatus object receiver date
simpleConditionToDFA (SimConTwoNH id subject date verbStatus object receiver) =
    createDFASimpleConditionNH subject verbStatus object receiver date
simpleConditionToDFA (SimConThreeNH id date subject verbStatus object receiver) =
    createDFASimpleConditionNH subject verbStatus object receiver date
simpleConditionToDFA (SimConFourNH id subject modalVerb verb object receiver date) =
    createDFASimpleStatementNH subject modalVerb verb object receiver date
simpleConditionToDFA (SimConFiveNH id booleanExpression) =
    createDFABooleanExpressionNH booleanExpression

createDFASimpleCondition :: Holds -> Subject -> VerbStatus -> Object -> Receiver -> Date -> State (StateDictionary, SelfLoopingEvents) DFA
createDFASimpleCondition holds subject verbStatus object receiver date = 
    case holds of
        (HoldYes) -> did
        (HoldNo) -> didNot

    where 
        didStateStrNB = objectToString object ++ " " ++ verbStatusToString verbStatus ++ " by " ++ subjectToString subject ++ " to " ++ receiverToString receiver ++ " on time"
        didEventStrNB = subjectToString subject ++ " " ++ verbStatusToString verbStatus ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        didStateNB = StateAD didStateStrNB
        didEventNB = EventD didEventStrNB
        
        didStateStrB = "BREACH: " ++ objectToString object ++ " not " ++ verbStatusToString verbStatus ++ " by " ++ subjectToString subject ++ " to " ++ receiverToString receiver ++ " on time"
        didEventB = generateBreachEvent subject (verbStatusToVerb verbStatus) object receiver date
        didStateB = StateAD didStateStrB

        didNotStateStrNB =  objectToString object ++ " not " ++ verbStatusToString verbStatus ++ " by " ++ subjectToString subject ++ " to " ++ receiverToString receiver ++ dateToString date
        didNotEventStrNB = subjectToString subject ++ " DIDN'T " ++ verbStatusToVerbString verbStatus ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        didNotStateStrB = "BREACH: " ++ objectToString object ++ " " ++ verbStatusToString verbStatus ++ " by " ++ subjectToString subject ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        didNotEventStrB = subjectToString subject ++ " " ++ verbStatusToString verbStatus ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date

        didNotStateNB = StateAD didNotStateStrNB
        didNotEventNB = EventD didNotEventStrNB
        didNotStateB = StateAD didNotStateStrB
        didNotEventB = EventD didNotEventStrB

        did = do
            addToStateDictionary didStateNB (Set.singleton $ didEventNB) 1
            addToStateDictionary didStateB (Set.singleton $ didEventB) 2
            newTransitions1 <- generateTransition (StateAD "Start") didStateNB
            newTransitions2 <- generateTransition (StateAD "Start") didStateB
            return $ DFA
                { states = Set.fromList [didStateNB, didStateB, StateAD "Start"]
                , events = Set.fromList [didEventNB, didEventB]
                , transitions = Set.union (Set.fromList newTransitions1) (Set.fromList newTransitions2)
                , startStates = Set.singleton $ StateAD "Start"
                , acceptingStates = Set.singleton didStateNB
                }
        didNot = do
            addToStateDictionary didNotStateNB (Set.singleton $ didNotEventNB) 1
            addToStateDictionary didNotStateB (Set.singleton $ didNotEventB) 2
            newTransitions1 <- generateTransition (StateAD "Start") didNotStateNB
            newTransitions2 <- generateTransition (StateAD "Start") didNotStateB
            return $ DFA
                { states = Set.fromList [didNotStateNB, didNotStateB, StateAD "Start"]
                , events = Set.fromList [didNotEventNB, didNotEventB]
                , transitions = Set.union (Set.fromList newTransitions1) (Set.fromList newTransitions2)
                , startStates = Set.singleton $ StateAD "Start"
                , acceptingStates = Set.singleton $ didNotStateNB
                }

createDFASimpleConditionNH :: Subject -> VerbStatus -> Object -> Receiver -> Date -> State (StateDictionary, SelfLoopingEvents) DFA
createDFASimpleConditionNH subject verbStatus object receiver date = do
    let didStateStrNB = objectToString object ++ " " ++ verbStatusToString verbStatus ++ " by " ++ subjectToString subject ++ " to " ++ receiverToString receiver ++ " on time"
        didEventStrNB = subjectToString subject ++ " " ++ verbStatusToString verbStatus ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        didStateNB = StateAD didStateStrNB
        didEventNB = EventD didEventStrNB
        
        didStateStrB = "BREACH: " ++ objectToString object ++ " not " ++ verbStatusToString verbStatus ++ " by " ++ subjectToString subject ++ "to " ++ receiverToString receiver ++ " on time"
        didEventB = generateBreachEvent subject (verbStatusToVerb verbStatus) object receiver date
        didStateB = StateAD didStateStrB

    addToStateDictionary didStateNB (Set.singleton $ didEventNB) 1
    addToStateDictionary didStateB (Set.singleton $ didEventB) 2
    newTransitions1 <- generateTransition (StateAD "Start") didStateNB
    newTransitions2 <- generateTransition (StateAD "Start") didStateB

    return $ DFA
        { states = Set.fromList [didStateNB, didStateB, StateAD "Start"]
        , events = Set.fromList [didEventNB, didEventB]
        , transitions = Set.union (Set.fromList newTransitions1) (Set.fromList newTransitions2)
        , startStates = Set.singleton $ StateAD "Start"
        , acceptingStates = Set.singleton didStateNB
        }

createDFABooleanExpression :: Holds -> BooleanExpression -> State (StateDictionary, SelfLoopingEvents) DFA
createDFABooleanExpression holds boolEx = 
    case holds of
      (HoldYes) -> yes
      (HoldNo) -> no

    where 
        yesStateStrNB = yesBooleanExpressionToString boolEx
        yesEventStrNB = yesBooleanExpressionToString boolEx
        yesStateNB = StateAD yesStateStrNB
        yesEventNB = EventD yesEventStrNB

        yesStateStrB = "BREACH: " ++ noBooleanExpressionToString boolEx
        yesEventStrB = noBooleanExpressionToString boolEx
        yesStateB = StateAD yesStateStrB
        yesEventB = EventD yesEventStrB

        noStateStrNB = noBooleanExpressionToString boolEx
        noEventStrNB = noBooleanExpressionToString boolEx
        noStateNB = StateAD noStateStrNB
        noEventNB = EventD noEventStrNB

        noStateStrB = "BREACH: " ++ yesBooleanExpressionToString boolEx
        noEventStrB = yesBooleanExpressionToString boolEx
        noStateB = StateAD noStateStrB
        noEventB = EventD noEventStrB

        yes = do
            addToStateDictionary yesStateNB (Set.singleton $ yesEventNB) 1
            addToStateDictionary yesStateB (Set.singleton $ yesEventB) 2
            newTransitions1 <- generateTransition (StateAD "Start") yesStateNB
            newTransitions2 <- generateTransition (StateAD "Start") yesStateB
            return $ DFA
                { states = Set.fromList [yesStateNB, yesStateB, StateAD "Start"]
                , events = Set.fromList [yesEventNB, yesEventB]
                , transitions = Set.union (Set.fromList newTransitions1) (Set.fromList newTransitions2)
                , startStates = Set.singleton $ StateAD "Start"
                , acceptingStates = Set.empty
                }
        no = do
            addToStateDictionary noStateNB (Set.singleton $ noEventNB) 1
            addToStateDictionary noStateB (Set.singleton $ noEventB) 2
            newTransitions1 <- generateTransition (StateAD "Start") noStateNB
            newTransitions2 <- generateTransition (StateAD "Start") noStateB
            return $ DFA
                { states = Set.fromList [noStateNB, noStateB, StateAD "Start"]
                , events = Set.fromList [noEventNB, noEventB]
                , transitions = Set.union (Set.fromList newTransitions1) (Set.fromList newTransitions2)
                , startStates = Set.singleton $ StateAD "Start"
                , acceptingStates = Set.empty
                }

createDFABooleanExpressionNH :: BooleanExpression -> State (StateDictionary, SelfLoopingEvents) DFA
createDFABooleanExpressionNH boolEx = do
    let yesStateStrNB = yesBooleanExpressionToString boolEx
        yesEventStrNB = yesBooleanExpressionToString boolEx
        yesStateNB = StateAD yesStateStrNB
        yesEventNB = EventD yesEventStrNB

        yesStateStrB = "BREACH: " ++ noBooleanExpressionToString boolEx
        yesEventStrB = noBooleanExpressionToString boolEx
        yesStateB = StateAD yesStateStrB
        yesEventB = EventD yesEventStrB

    addToStateDictionary yesStateNB (Set.singleton $ yesEventNB) 1
    addToStateDictionary yesStateB (Set.singleton $ yesEventB) 2
    newTransitions1 <- generateTransition (StateAD "Start") yesStateNB
    newTransitions2 <- generateTransition (StateAD "Start") yesStateB
    return $ DFA
        { states = Set.fromList [yesStateNB, yesStateB, StateAD "Start"]
        , events = Set.fromList [yesEventNB, yesEventB]
        , transitions = Set.union (Set.fromList newTransitions1) (Set.fromList newTransitions2)
        , startStates = Set.singleton $ StateAD "Start"
        , acceptingStates = Set.empty
        }

runDFAConversion :: Contract -> DFA
runDFAConversion contract = evalState (contractToDFA contract) (Map.empty, [])

runDFAConversion' :: Contract -> (DFA, StateDictionary, SelfLoopingEvents)
runDFAConversion' contract = (result, newStateDict, newSLEvents)
  where
    -- Run the conversion and get the result and the updated state
    (result, (newStateDict, newSLEvents)) = runState (contractToDFA contract) (Map.empty, [])

extractForbiddenState :: (DFA, StateDictionary, SelfLoopingEvents) -> [StateAD]
extractForbiddenState (dfa, stateDict, _) =
    let forbiddenStates = [state | (state, (_, value)) <- Map.toList stateDict, value == 3]
        liveStates = states dfa
        fs = Set.intersection (liveStates) (Set.fromList forbiddenStates)
    in Set.toList fs

extractNonBreachState :: (DFA, StateDictionary, SelfLoopingEvents) -> [StateAD]
extractNonBreachState (dfa, stateDict, _) =
    let nbStates = [state | (state, (_, value)) <- Map.toList stateDict, value == 1]
        liveStates = states dfa
        nbs = Set.intersection (liveStates) (Set.fromList nbStates)
    in Set.toList nbs

generateTransitionsForbidden :: [StateAD] -> [StateAD] -> State (StateDictionary, SelfLoopingEvents) [TransitionD]
generateTransitionsForbidden nbs fs = concat <$> mapM (\state1 -> concat <$> mapM (generateTransition state1) fs) nbs

generateTransitionsSelfLooping :: [StateAD] -> [EventD] -> State (StateDictionary, SelfLoopingEvents) [TransitionD]
generateTransitionsSelfLooping states events = concat <$> mapM (\state -> concat <$> mapM (generateSelfLoopingTransition state) events) states

generateSelfLoopingTransition :: StateAD -> EventD -> State (StateDictionary, SelfLoopingEvents) [TransitionD]
generateSelfLoopingTransition state event = do
    let transition = TransitionD state event state
    return [transition]

createForbiddenTransitions :: (DFA, StateDictionary, SelfLoopingEvents) -> [TransitionD]
createForbiddenTransitions (dfa, stateDict, slEvents) =
    let nbs = extractNonBreachState (dfa, stateDict, slEvents)
        fs = extractForbiddenState (dfa, stateDict, slEvents)
        tl = evalState (generateTransitionsForbidden nbs fs) (stateDict, slEvents)
    in tl

createSelfLoopingTransitions :: (DFA, StateDictionary, SelfLoopingEvents) -> [TransitionD]
createSelfLoopingTransitions (dfa, stateDict, slEvents) =
    let s = Set.toList (states dfa)
        e = slEvents
        tl = evalState (generateTransitionsSelfLooping s e) (stateDict, slEvents)
    in tl

runDFAConversionFinal :: Contract -> DFA
runDFAConversionFinal contract = 
    let (dfa, _, _) = runDFAConversion' contract
        ft = createForbiddenTransitions (runDFAConversion' contract)
        slt = createSelfLoopingTransitions (runDFAConversion' contract)
    in DFA
        { states = states dfa
        , events = events dfa
        , transitions = Set.unions [(transitions dfa), (Set.fromList ft), (Set.fromList slt)]
        , startStates = Set.singleton $ StateAD "Start"
        , acceptingStates = acceptingStates dfa
        }

getNumberOfStates :: DFA -> Integer
getNumberOfStates dfa = fromIntegral $ Set.size $ states dfa

getNumberOfTransitions :: DFA -> Integer
getNumberOfTransitions dfa = fromIntegral $ Set.size $ transitions dfa

dfaToGraph :: DFA -> Gr Text Text
dfaToGraph dfa =
    mkGraph nodes edges
    where
        allStates = Set.toList $ states dfa
        nodeMap = zip allStates [1 ..]
        nodes = [(nodeId, stateToLabelDFA state) | (state, nodeId) <- nodeMap]
        
        sortedTransitions = sort $ Set.toList $ transitions dfa
        groupedTransitions = group sortedTransitions
        
        edges = concatMap (\transGroup -> 
                    case transGroup of
                        [] -> []
                        (TransitionD state1 event state2 : _) -> 
                            let (Just nodeId1) = lookup state1 nodeMap
                                (Just nodeId2) = lookup state2 nodeMap
                                labeledEvent = eventToLabelDFA event
                            in [(nodeId1, nodeId2, labeledEvent)]
                    ) groupedTransitions

stateToLabelDFA :: StateAD -> Text
stateToLabelDFA (StateAD s) = L.pack s

eventToLabelDFA :: EventD -> Text
eventToLabelDFA (EventD s) = L.pack s

isAcceptingDFA :: Text -> DFA -> Bool
isAcceptingDFA state dfa = (StateAD $ (L.unpack state)) `Set.member` (acceptingStates dfa)

isBreachDFA :: Text -> Bool
isBreachDFA state = "BREACH" `isInfixOf` (L.unpack state)

visualizeGraphDFA :: DFA -> Gr Text Text -> String
visualizeGraphDFA dfa graph = L.unpack dotText
    where
        dotGraph = graphToDot (labelledNodesParamsDFA dfa) graph :: DotGraph Node
        dotText = renderDot $ toDot dotGraph  
    
visualizeGraphDFA' :: DFA -> Gr Text Text -> IO ()
visualizeGraphDFA' dfa graph = do
    let dotGraph = graphToDot (labelledNodesParamsDFA dfa) graph :: DotGraph Node
        dotText = renderDot $ toDot dotGraph
    putStrLn $ L.unpack dotText

labelledNodesParamsDFA :: DFA -> GraphvizParams Node Text Text () Text
labelledNodesParamsDFA dfa = nonClusteredParams
    { fmtNode = \(_, label) ->
        let nodeColor = if (isAcceptingDFA label dfa) then Green else if (isBreachDFA label) then LightSalmon else LightGray
            -- Insert newline characters to break the label into lines with a maximum of five words
            wordsList = words (L.unpack label)
            linesList = groupWordsIntoLines 5 wordsList
            multiLineString = intercalate "\n" linesList
            multiLineLabel = L.pack multiLineString
        in [Gv.Label (Gv.StrLabel multiLineLabel), Gv.FillColor [Gvc.toWColor nodeColor], Gv.Style [filled]]
    , fmtEdge = \(_, _, edgeLabel) -> [Gv.Label (Gv.StrLabel edgeLabel)]
    }

groupWordsIntoLines :: Int -> [String] -> [String]
groupWordsIntoLines _ [] = [] -- If the input list is empty, return an empty list
groupWordsIntoLines maxWordsPerLine wordsList = go wordsList []
    where
    go [] linesAcc = reverse linesAcc
    go (word:restWords) linesAcc =
        let (currentLine, remainingWords) = splitAt maxWordsPerLine (word:restWords)
        in go remainingWords (unwords currentLine : linesAcc)
