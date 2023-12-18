module AstToNFA where

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
  , (*)
  , fst
  , snd
  , Bool
  , (>=)
  , (<=)
  , Bool(..)
  , Ord
  )

import Data.Time
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)

import AbsCoLa   
import LexCoLa   ( Token, mkPosToken )
import ParCoLa   ( pContract, myLexer )
import PrintCoLa ( Print, printTree )
import SkelCoLa  ()

type StateDict = Map.Map StateA Event

data StateA 
    = StateA String
  deriving (Eq, Ord, Show, Read)

data Event
    = Event String
  deriving (Eq, Ord, Show, Read)

data Transition
    = Transition StateA Event StateA
  deriving (Eq, Ord, Show, Read)

data NFA = NFA
    { states :: Set.Set StateA
    , events :: Set.Set Event
    , transitions :: Set.Set Transition
    , startStates :: Set.Set StateA
    , acceptingStates :: Set.Set StateA
    } 
  deriving (Eq, Show, Read)

modifyStateDict :: (StateDict -> StateDict) -> State StateDict ()
modifyStateDict f = modify f

modifyNFA :: (NFA -> NFA) -> State StateDict ()
modifyNFA f = modify (\s -> s { currentNFA = f (currentNFA s) })

-- Helper function to generate transitions for a pair of state sets
generateTransitions :: Set.Set StateA -> Set.Set StateA -> NFA -> NFA -> State StateDict [Transition]
generateTransitions stateSet1 stateSet2 nfa1 nfa2 = do
    let updatedMap = Map.union (stateDict nfa1) (stateDict nfa2)
    modifyStateDict (const updatedMap)

    fmap concat $ forM (Set.toList stateSet1) $ \frontState -> do
        let transitionEvent = Map.findWithDefault (Event "DefaultEvent") frontState updatedMap
        fmap concat $ forM (Set.toList stateSet2) $ \backState -> do
            let newTransition = Transition frontState transitionEvent backState
            return [newTransition]

contractToNFA :: Contract -> State StateDict NFA
contractToNFA (ConEmpty) = return $ NFA 
    { states = Set.singleton $ StateA "Empty",
      events = Set.empty,
      transitions = Set.empty,
      startStates = Set.singleton $ StateA "Empty",
      acceptingStates = Set.singleton $ StateA "Empty"
    }
contractToNFA (ConComp component) = componentToNFA component
contractToNFA (ConAnd component contract) = do
    nfa1 <- componentToNFA component
    nfa2 <- contractToNFA contract
  
    let combinedStates = Set.union (states nfa1) (states nfa2)
        combinedEvents = Set.union (events nfa1) (events nfa2)
        combinedTransitions = Set.union (transitions nfa1) (transitions nfa2)
        startStates1 = startStates nfa1
        startStates2 = startStates nfa2
        endStates1 = acceptingStates nfa1
        endStates2 = acceptingStates nfa2

    newTransitions1 <- generateTransitions endStates1 startStates2 nfa1 nfa2
    newTransitions2 <- generateTransitions endStates2 startStates1 nfa1 nfa2

    allNewTransitions = newTransitions1 ++ newTransitions2

    return $ NFA
      { states = combinedStates
      , events = combinedEvents
      , transitions = Set.union (combinedTransitions) (Set.fromList allNewTransitions)
      , startStates = Set.union (startStates1) (startStates2)
      , acceptingStates = Set.union (endStates1) (endStates2)
      }

componentToNFA :: Component -> State StateDict NFA
componentToNFA (ComDef definition) = definitionToNFA definition
componentToNFA (ComConDef confitionalDefinition) = conditonalDefinitionToNFA conditionalDefinition
componentToNFA (ComState statement) = statementToNFA statement
componentToNFA (conditionalStatement) = conditionalStatementToNFA conditionalStatement

definitionToNFA :: Definition -> State StateDict NFA
definitionToNFA (DefSim simpleDefinition) = simpleDefinitionToNFA simpleDefinition
definitionToNFA (DefAnd simpleDefinition definition) = do
    nfa1 <- simpleDefinitionToNFA simpleDefinition
    nfa2 <- definitionToNFA definition

    let combinedStates = Set.union (states nfa1) (states nfa2)
        combinedEvents = Set.union (events nfa1) (events nfa2)
        combinedTransitions = Set.union (transitions nfa1) (transitions nfa2)
        startStates1 = startStates nfa1
        startStates2 = startStates nfa2
        endStates1 = acceptingStates nfa1
        endStates2 = acceptingStates nfa2

    newTransitions1 <- generateTransitions endStates1 startStates2 nfa1 nfa2
    newTransitions2 <- generateTransitions endStates2 startStates1 nfa1 nfa2

    allNewTransitions = newTransitions1 ++ newTransitions2

    return $ NFA
      { states = combinedStates
      , events = combinedEvents
      , transitions = Set.union (combinedTransitions) (Set.fromList allNewTransitions)
      , startStates = Set.union (startStates1) (startStates2)
      , acceptingStates = Set.union (endStates1) (endStates2)
      }

conditionalDefinitionToNFA :: ConditionalDefinition -> State StateDict NFA
conditionalDefinitionToNFA (ConDefIf definition condition) = do
    nfaCon <- conditionToNFA condition
    nfaDef <- definitionToNFA definition

    let combinedStates = Set.union (states nfaCon) (states nfaDef)
        combinedEvents = Set.union (events nfaCon) (events nfaDef)
        combinedTransitions = Set.union (transitions nfaCon) (transitions nfaDef)
        conditionStates = states nfaCon
        definitionStates = states nfaDef

    newTransitions <- generateTransitions conditionStates definitionStates nfa1 nfa2

    return $ NFA
        { states = combinedStates
        , events = combinedEvents
        , transitions = Set.union (combinedTransitions) (Set.fromList newTransitions)
        , startStates = conditionStates
        , acceptingStates = definitionStates
        }
conditionalDefinitionToNFA (ConDefIfThen condition definition) = do
    nfaCon <- conditionToNFA condition
    nfaDef <- definitionToNFA definition

    let combinedStates = Set.union (states nfaCon) (states nfaDef)
        combinedEvents = Set.union (events nfaCon) (events nfaDef)
        combinedTransitions = Set.union (transitions nfaCon) (transitions nfaDef)
        conditionStates = states nfaCon
        definitionStates = states nfaDef

    newTransitions <- generateTransitions conditionStates definitionStates nfa1 nfa2

    return $ NFA
        { states = combinedStates
        , events = combinedEvents
        , transitions = Set.union (combinedTransitions) (Set.fromList newTransitions)
        , startStates = conditionStates
        , acceptingStates = definitionStates
        }

statementToNFA :: Statement -> State StateDict NFA
statementToNFA (StateSim simpleStatement) = simpleStatementToNFA simpleStatement
statementToNFA (StateOr simpleStatement statement) = do
    nfa1 <- simpleStatementToNFA simpleStatement
    nfa2 <- statementToNFA statement

    let combinedStates = Set.union (states nfa1) (states nfa2)
        combinedEvents = Set.union (events nfa1) (events nfa2)
        combinedTransitions = Set.union (transitions nfa1) (transitions nfa2)

    return $ NFA
      { states = combinedStates
      , events = combinedEvents
      , transitions = Set.union (combinedTransitions) (Set.fromList allNewTransitions)
      , startStates = combinedStates
      , acceptingStates = combinedStates
      }
statementToNFA (StateAnd simpleStatement statement) = do
    nfa1 <- simpleStatementToNFA simpleDefinition
    nfa2 <- statementToNFA definition

    let combinedStates = Set.union (states nfa1) (states nfa2)
        combinedEvents = Set.union (events nfa1) (events nfa2)
        combinedTransitions = Set.union (transitions nfa1) (transitions nfa2)
        startStates1 = startStates nfa1
        startStates2 = startStates nfa2
        endStates1 = acceptingStates nfa1
        endStates2 = acceptingStates nfa2

    newTransitions1 <- generateTransitions endStates1 startStates2 nfa1 nfa2
    newTransitions2 <- generateTransitions endStates2 startStates1 nfa1 nfa2

    allNewTransitions = newTransitions1 ++ newTransitions2

    return $ NFA
      { states = combinedStates
      , events = combinedEvents
      , transitions = Set.union (combinedTransitions) (Set.fromList allNewTransitions)
      , startStates = Set.union (startStates1) (startStates2)
      , acceptingStates = Set.union (endStates1) (endStates2)
      }

conditionalStatementToNFA :: ConditionalStatement -> State StateDict NFA
conditionalStatementToNFA (ConStateIf statement condition) = do
    nfaCon <- conditionToNFA condition
    nfaState <- statementToNFA definition

    let combinedStates = Set.union (states nfaCon) (states nfaState)
        combinedEvents = Set.union (events nfaCon) (events nfaState)
        combinedTransitions = Set.union (transitions nfaCon) (transitions nfaState)
        conditionStates = states nfaCon
        statementStates = states nfaState

    newTransitions <- generateTransitions conditionStates statementStates nfa1 nfa2

    return $ NFA
        { states = combinedStates
        , events = combinedEvents
        , transitions = Set.union (combinedTransitions) (Set.fromList newTransitions)
        , startStates = conditionStates
        , acceptingStates = statementStates
        }
conditionalStatementToNFA (ConStateIfThen condition statement) = do
    nfaCon <- conditionToNFA condition
    nfaState <- statementToNFA definition

    let combinedStates = Set.union (states nfaCon) (states nfaState)
        combinedEvents = Set.union (events nfaCon) (events nfaState)
        combinedTransitions = Set.union (transitions nfaCon) (transitions nfaState)
        conditionStates = states nfaCon
        statementStates = states nfaState

    newTransitions <- generateTransitions conditionStates statementStates nfa1 nfa2

    return $ NFA
        { states = combinedStates
        , events = combinedEvents
        , transitions = Set.union (combinedTransitions) (Set.fromList newTransitions)
        , startStates = conditionStates
        , acceptingStates = statementStates
        }

simpleDefinitionToNFA :: SimpleDefinition -> State StateDict NFA
simpleDefinitionToNFA (SimDefIs id subject1 subject2)
simpleDefinitionToNFA (SimDefEq id subject numericalExpression)
simpleDefinitionToNFA (SimDefDate id subject day month year)

conditionToNFA :: Condition -> State StateDict NFA

simpleStatementToNFA :: SimpleStatement -> State StateDict NFA



runNFAConversion :: Contract -> NFA
runNFAConversion contract = evalState (contractToNFA contract) (Map.empty)



