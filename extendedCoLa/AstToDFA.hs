module AstToDFA where

import Prelude
  ( ($), (++), (||), (==)
  , Int
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
  , head
  , fst, snd
  , error, all, max, not
  )

import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad.State
import Data.List (map, lookup, sort, group, concatMap, intercalate, find) 
import qualified Data.List as List
import Data.Maybe (catMaybes)

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

generateThenState :: Set.Set EventD -> Set.Set EventD -> State (StateDictionary, SelfLoopingEvents) StateAD
generateThenState ifEvents thenEvents = do
    originalStateDictionary <- gets fst
    let thenState = StateAD $ combineSets " OR " ifEvents `combine` combineSets " OR " thenEvents
    addToStateDictionary thenState thenEvents 1
    
    return thenState
  
combineSets :: String -> Set.Set EventD -> String
combineSets separator events = foldr (\(EventD s) acc -> s ++ if null acc then "" else separator ++ acc) "" (Set.toList events)

combine :: String -> String -> String
combine x y = if null x || null y then x ++ y else x ++ " AND " ++ y

generateAndEvent :: Set.Set EventD -> Set.Set EventD -> EventD
generateAndEvent events1 events2 =
    EventD $ intercalate " AND " $ map (\(EventD s) -> s) (Set.toList events1 ++ Set.toList events2)

generateAndState :: Set.Set StateAD -> Set.Set StateAD -> StateAD
generateAndState states1 states2 =
    StateAD $ intercalate " AND " $ map (\(StateAD s) -> s) (Set.toList states1 ++ Set.toList states2)

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
            Just (_, 2) -> Just state
            _           -> Nothing

getBreachState :: Set.Set StateAD -> StateDictionary -> StateAD
getBreachState conditionStates stateDict =
    case findBreachState conditionStates stateDict of
        Just breachState -> breachState
        Nothing          -> error "No breach state found in the given set"

findBreachState :: Set.Set StateAD -> StateDictionary -> Maybe StateAD
findBreachState conditionStates stateDict =
    find (\state -> case Map.lookup state stateDict of
                        Just (_, 2) -> True
                        _           -> False) (Set.toList conditionStates)

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
                Just val -> val == 2
                Nothing  -> False

createMappingOfEvents :: StateDictionary -> Map.Map EventD Int
createMappingOfEvents stateDict =
    Map.fromListWith max $ concatMap (\(_, (events, val)) -> [(event, val) | event <- Set.toList events]) (Map.toList stateDict)

removeNonBreachTransitions :: Set.Set TransitionD -> Set.Set EventD -> Set.Set TransitionD
removeNonBreachTransitions transitions events =
    Set.filter (\(TransitionD _ event _) -> not (event `Set.member` events)) transitions

exampleStateDict :: StateDictionary
exampleStateDict = Map.fromList
    [ (StateAD "Hello", (Set.fromList [EventD "World", EventD "Bye"], 1))
    , (StateAD "Chris", (Set.singleton (EventD "Clack"), 3))
    , (StateAD "hate", (Set.fromList [EventD "mad", EventD "everyone"], 1))
    ]

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
        definitionState = evalState (generateThenState conditionEvents definitionEvents) (oriDict, oriList)
        conditionState = getNonBreachState conditionStates oriDict

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
        definitionState = evalState (generateThenState conditionEvents definitionEvents) (oriDict, oriList)
        conditionState = getNonBreachState conditionStates oriDict

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
        definitionState1 = evalState (generateThenState conditionEvents definitionEvents1) (oriDict, oriList)
        
        nonConditionState = getBreachState conditionStates oriDict
        nonConditionEvents = getBreachEvents allConditionEvents oriDict
        definitionEvents2 = events dfaDef2
        definitionState2 = evalState (generateThenState nonConditionEvents definitionEvents2) (oriDict, oriList)

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
        definitionState1 = evalState (generateThenState conditionEvents definitionEvents1) (oriDict, oriList)
        
        nonConditionState = getBreachState conditionStates oriDict
        nonConditionEvents = getBreachEvents allConditionEvents oriDict
        definitionEvents2 = events dfaDef2
        definitionState2 = evalState (generateThenState nonConditionEvents definitionEvents2) (oriDict, oriList)

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
                combinedAccepting = Set.union (acceptingStates dfa1) (acceptingStates dfa2)
                statementStatesNB = getNonBreachStates combinedStates oriDict
                statementEventsNB = getNonBreachEvents combinedEvents oriDict
                statementStatesB = getBreachStates combinedStates oriDict
                statementEventsB = getBreachEvents combinedEvents oriDict

                combinedStateNB = generateAndState statementStatesNB Set.empty
                combinedEventNB = generateAndEvent statementEventsNB Set.empty

                cleanedTransitions = removeNonBreachTransitions combinedTransitions statementEventsNB

                addToStateDictionary combinedState (Set.singleton combinedEvents) 1
                removeFromStateDictionary statementStatesNB

            return $ DFA
                { states = Set.union (statementStatesB) (Set.singleton combinedStateNB)
                , events = Set.union (statementEventsB) (Set.singleton combinedEventNB)
                , transitions = cleanedTransitions
                , startStates = Set.singleton $ StateAD "Start"
                , acceptingStates = combinedAccepting
                }
        caseTwo = do
            dfa1 <- simpleStatementToDFA simpleStatement
            dfa2 <- statementToDFA statement
            oriDict <- gets fst

            let statementStates1NB = getNonBreachStates (states dfa1) oriDict
                statementEvents1NB = getNonBreachEvents (events dfa1) oriDict
                statementStates2NB = getNonBreachStates (states dfa2) oriDict
                statementEvents2NB = getNonBreachEvents (events dfa2) oriDict


            return $ DFA
                { states = Set.union (statementStatesB) (Set.singleton combinedStateNB)
                , events = Set.union (statementEventsB) (Set.singleton combinedEventNB)
                , transitions = cleanedTransitions
                , startStates = Set.singleton $ StateAD "Start"
                , acceptingStates = combinedAccepting
                }

conditionalStatementToDFA :: ConditionalStatement -> State (StateDictionary, SelfLoopingEvents) DFA
conditionalStatementToDFA (ConStateIf statement condition) = do
    dfaCon <- conditionToDFA condition
    dfaState <- statementToDFA' statement
    oriDict <- gets fst
    oriList <- gets snd

    let combinedStates = Set.union (states dfaCon) (states dfaState)
        combinedEvents = Set.union (events dfaCon) (events dfaState)
        combinedTransitions = Set.union (transitions dfaCon) (transitions dfaState)
        conditionStates = states dfaCon
        conditionEvents = events dfaCon
        statementEvents = events dfaState
        statementState = evalState (generateThenState conditionEvents definitionEvents) (oriDict, oriList)
        conditionState = head (Set.toList conditionStates)

    newTransitions <- generateTransition conditionState statementState

    return $ DFA
        { states = Set.union (combinedStates) (Set.singleton $ StateAD "Start")
        , events = combinedEvents
        , transitions = Set.union (combinedTransitions) (Set.fromList newTransitions)
        , startStates = Set.singleton $ StateAD "Start"
        , acceptingStates = Set.singleton $ statementState
        }

conditionalStatementToDFA (ConStateIfThen condition statement) = do
    dfaCon <- conditionToDFA condition
    dfaState <- statementToDFA statement

    let combinedStates = Set.union (states dfaCon) (states dfaState)
        combinedEvents = Set.union (events dfaCon) (events dfaState)
        combinedTransitions = Set.union (transitions dfaCon) (transitions dfaState)
        conditionStates = states dfaCon
        statementStates = states dfaState

    newTransitions <- generateTransitions conditionStates statementStates 

    return $ DFA
        { states = combinedStates
        , events = combinedEvents
        , transitions = Set.union (combinedTransitions) (Set.fromList newTransitions)
        , startStates = conditionStates
        , acceptingStates = statementStates
        }
conditionalStatementToDFA (ConStateIfElse statement1 condition statement2) = do
    dfaCon <- conditionToDFA condition
    dfaState1 <- statementToDFA statement1
    dfaConNot <- notConditionToDFA condition
    dfaState2 <- statementToDFA statement2

    let combinedStates = Set.unions [(states dfaCon), (states dfaState1), (states dfaConNot), (states dfaState2)]
        combinedEvents = Set.unions [(events dfaCon), (events dfaState1), (events dfaConNot), (events dfaState2)]
        combinedTransitions = Set.unions [(transitions dfaCon), (transitions dfaState1), (transitions dfaConNot), (transitions dfaState2)]
        conditionStates = states dfaCon
        statementStates1 = states dfaState1
        nonConditionStates = states dfaConNot
        statementStates2 = states dfaState2

    newTransitions1 <- generateTransitions conditionStates statementStates1
    newTransitions2 <- generateTransitions nonConditionStates statementStates2

    return $ DFA
        { states = combinedStates
        , events = combinedEvents
        , transitions = Set.unions [(combinedTransitions), (Set.fromList newTransitions1), (Set.fromList newTransitions2)]
        , startStates = Set.union conditionStates nonConditionStates
        , acceptingStates = Set.union statementStates1 statementStates2
        }
conditionalStatementToDFA (ConStateIfThenElse condition statement1 statement2) = do
    dfaCon <- conditionToDFA condition
    dfaState1 <- statementToDFA statement1
    dfaConNot <- notConditionToDFA condition
    dfaState2 <- statementToDFA statement2

    let combinedStates = Set.unions [(states dfaCon), (states dfaState1), (states dfaConNot), (states dfaState2)]
        combinedEvents = Set.unions [(events dfaCon), (events dfaState1), (events dfaConNot), (events dfaState2)]
        combinedTransitions = Set.unions [(transitions dfaCon), (transitions dfaState1), (transitions dfaConNot), (transitions dfaState2)]
        conditionStates = states dfaCon
        statementStates1 = states dfaState1
        nonConditionStates = states dfaConNot
        statementStates2 = states dfaState2

    newTransitions1 <- generateTransitions conditionStates statementStates1
    newTransitions2 <- generateTransitions nonConditionStates statementStates2

    return $ DFA
        { states = combinedStates
        , events = combinedEvents
        , transitions = Set.unions [(combinedTransitions), (Set.fromList newTransitions1), (Set.fromList newTransitions2)]
        , startStates = Set.union conditionStates nonConditionStates
        , acceptingStates = Set.union statementStates1 statementStates2
        }
