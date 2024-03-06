module ContractAnalysis.AstToGraph where

import Prelude
  ( ($), (++), (/=), (==), (+), (||), (-), (&&), (.), (<>)
  , Int, String, Show, Eq, Read, Ord, IO, Bool(..), Maybe(..)
  , zip, foldr, foldl, filter, otherwise, elem, reverse, break, tail, length, any, putStrLn, unlines, show
  )

import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad.State
import Data.List (map, lookup, sort, group, concatMap) 
import Data.List (foldl')
import Data.List (stripPrefix)
import qualified Data.List as List

import Data.Maybe (fromMaybe)

import Debug.Trace

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


import Parser.AbsCoLa   

type StateDict = Map.Map StateA Event

data StateA 
    = StateA String
  deriving (Eq, Ord, Read, Show)

data Event
    = Event String
  deriving (Eq, Ord, Read, Show)

data Transition
    = Transition StateA Event StateA
  deriving (Eq, Ord, Read, Show)

data NFA = NFA
    { states :: Set.Set StateA
    , events :: Set.Set Event
    , transitions :: Set.Set Transition
    , startStates :: Set.Set StateA
    , acceptingStates :: Set.Set StateA
    } 
  deriving (Eq, Ord, Read, Show)

-- -- Pretty-printing for StateA
printStateA :: StateA -> String
printStateA (StateA s) = s

-- -- Pretty-printing for Event
printEvent :: Event -> String
printEvent (Event e) = e

-- -- Pretty-printing for Transition
printTransition :: Transition -> String
printTransition (Transition from event to) = printStateA from ++ " --(" ++ printEvent event ++ ")--> " ++ printStateA to

printTransitions :: Set.Set Transition -> String
printTransitions transitionSet =
    unlines $ map printTransition (Set.toList transitionSet)

prettyPrintTransitions :: Set.Set Transition -> IO ()
prettyPrintTransitions transitions = putStrLn $ printTransitions transitions

prettyPrintListOfSets :: [Set.Set Transition] -> IO ()
prettyPrintListOfSets listOfSets = mapM_ prettyPrintTransitions listOfSets

-- -- Pretty-printing for NFA
printNFA :: NFA -> String
printNFA nfa =
    unlines
        [ "States:"
        , unlines (map printStateA (Set.toList $ states nfa))
        , "Events:"
        , unlines (map printEvent (Set.toList $ events nfa))
        , "Transitions:"
        , unlines (map printTransition (Set.toList $ transitions nfa))
        , "Start States:"
        , unlines (map printStateA (Set.toList $ startStates nfa))
        , "Accepting States:"
        , unlines (map printStateA (Set.toList $ acceptingStates nfa))
        ]

prettyPrintNFA nfa = putStrLn $ printNFA nfa

modifyStateDict :: (StateDict -> StateDict) -> State StateDict ()
modifyStateDict f = modify f

-- Helper function to add a state and event to the StateDict
addToStateDict :: StateA -> Event -> State StateDict ()
addToStateDict newState newEvent = do
    modifyStateDict (\dict -> Map.insert newState newEvent dict)

generateTransitions :: Set.Set StateA -> Set.Set StateA -> State StateDict [Transition]
generateTransitions stateSet1 stateSet2 = do
    updatedMap <- get
    let transitionsList = [Transition frontState (Map.findWithDefault (Event "DefaultEvent") frontState updatedMap) backState | frontState <- Set.toList stateSet1, backState <- Set.toList stateSet2]
    return transitionsList

subjectToString :: Subject -> String
subjectToString (SubQuoted str) = str
subjectToString (SubUnQuoted ident) = getIdentString ident

getIdentString :: Ident -> String
getIdentString (Ident str) = str

numericalExpressionToString :: NumericalExpression -> String
numericalExpressionToString (NumExpNum (NumInt n)) = show n 
numericalExpressionToString (NumExpObj numObj) = numericalObjectToString numObj
numericalExpressionToString (NumExpOp expr1 operator expr2) =
    let str1 = numericalExpressionToString expr1
        str2 = numericalExpressionToString expr2
        operatorStr = case operator of
            OpPlus -> " + "
            OpMin -> " - "
            OpMult -> " * "
            OpDiv -> " / "
    in str1 ++ operatorStr ++ str2

numericalObjectToString :: NumericalObject -> String
numericalObjectToString (NumPound _ (NumInt n)) = "£" ++ show n
numericalObjectToString (NumDol _ (NumInt n)) = "$" ++ show n
numericalObjectToString (NumEur _ (NumInt n)) = "€" ++ show n
numericalObjectToString (NumAmount subject) = subjectToString subject

dateSpeToString :: Num -> Month -> Num -> String
dateSpeToString (NumInt day) month (NumInt year) = show day ++ " " ++ monthToString month ++ " " ++ show year

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

verbToString :: Verb -> String
verbToString VDel = "deliver"
verbToString VPay = "pay"
verbToString VCharge = "charge"
verbToString VRefund = "refund"

objectToString :: Object -> String
objectToString (ObjNu (NumPound _ (NumInt num))) = "£" ++ show num
objectToString (ObjNu (NumDol _ (NumInt num))) = "$" ++ show num
objectToString (ObjNu (NumEur _ (NumInt num))) = "€" ++ show num
objectToString (ObjNu (NumAmount subject)) = "Amount \"" ++ subjectToString subject ++ "\""
objectToString (ObjNonNu (NonNumCurr subject)) = "SomeCurrency \"" ++ subjectToString subject ++ "\""
objectToString (ObjNonNu (NonNumRep subject)) = "Report \"" ++ subjectToString subject ++ "\""
objectToString (ObjNonNu (NonNumNamed subject)) = "NamedObject \"" ++ subjectToString subject ++ "\""
objectToString (ObjNonNu (NonNumOther subject)) = "OtherObject \"" ++ subjectToString subject ++ "\""

receiverToString :: Receiver -> String
receiverToString (Rec subject) = subjectToString subject

dateToString :: Date -> String
dateToString (DateSpe (DateSpeOnThe day month year)) = "on the " ++ dateSpeToString day month year
dateToString (DateSpe (DateSpeOn day month year)) = "on " ++ dateSpeToString day month year
dateToString (DateAny) = "on ANYDATE"
dateToString (DateSome subject) = "on SOMEDATE " ++ subjectToString subject
dateToString (DateThe subject) = "on THEDATE " ++ subjectToString subject
dateToString (DateQuanSpecific tq day month year) = temporalQuantifierToString tq ++ dateSpeToString day month year  
dateToString (DateQuanSome tq subject) = temporalQuantifierToString tq ++ "SOMEDATE " ++ subjectToString subject
dateToString (DateQuanThe tq subject) = temporalQuantifierToString tq ++ "THEDATE " ++ subjectToString subject
dateToString (DateQuanSomeWO to tq subject) = temporalOffsetToString to ++ temporalQuantifierToString tq ++ "SOMEDATE " ++ subjectToString subject
dateToString (DateQuanTheWO to tq subject) = temporalOffsetToString to ++ temporalQuantifierToString tq ++ "THEDATE " ++ subjectToString subject
dateToString (DateQuanTempSome tq1 to tq2 subject) = temporalQuantifierToString tq1 ++ temporalOffsetToString to ++ temporalQuantifierToString tq2 ++ "SOMEDATE " ++ subjectToString subject
dateToString (DateQuanTempThe tq1 to tq2 subject) = temporalQuantifierToString tq1 ++ temporalOffsetToString to ++ temporalQuantifierToString tq2 ++ "THEDATE " ++ subjectToString subject

temporalQuantifierToString :: TemporalQuantifier -> String
temporalQuantifierToString TempAfter = " AFTER "
temporalQuantifierToString TempBefore = " BEFORE "

temporalOffsetToString :: TemporalOffset -> String
temporalOffsetToString (TempOffDay num) = show num ++ " day"
temporalOffsetToString (TempOffYear num) = show num ++ " year"
temporalOffsetToString (TempOffWeek num) = show num ++ " week"
temporalOffsetToString (TempOffDays num) = show num ++ " days"
temporalOffsetToString (TempOffYears num) = show num ++ " years"
temporalOffsetToString (TempOffWeeks num) = show num ++ " weeks"

verbToVerbStatusString :: Verb -> String
verbToVerbStatusString VDel = "delivered"
verbToVerbStatusString VPay = "paid"
verbToVerbStatusString VCharge = "charged"
verbToVerbStatusString VRefund = "refunded"

verbStatusToString :: VerbStatus -> String
verbStatusToString VSDel = "delivered"
verbStatusToString VSPay = "paid"
verbStatusToString VSCharge = "charged"
verbStatusToString VSRefund = "refunded"

verbStatusToVerbString :: VerbStatus -> String
verbStatusToVerbString VSDel = "deliver"
verbStatusToVerbString VSPay = "pay"
verbStatusToVerbString VSCharge = "charge"
verbStatusToVerbString VSRefund = "refund"

comparisonToString :: Comparison -> String
comparisonToString (CompareLess) = "LESS THAN"
comparisonToString (CompareEq _) = "EQUAL TO"
comparisonToString (CompareMore _) = "MORE THAN"

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

    newTransitions1 <- generateTransitions endStates1 startStates2 
    newTransitions2 <- generateTransitions endStates2 startStates1 

    let allNewTransitions = newTransitions1 ++ newTransitions2 :: [Transition]

    return $ NFA
      { states = combinedStates
      , events = combinedEvents
      , transitions = Set.union (combinedTransitions) (Set.fromList allNewTransitions)
      , startStates = Set.union (startStates1) (startStates2)
      , acceptingStates = Set.union (endStates1) (endStates2)
      }

componentToNFA :: Component -> State StateDict NFA
componentToNFA (ComDef definition) = definitionToNFA definition
componentToNFA (ComConDef conditionalDefinition) = conditionalDefinitionToNFA conditionalDefinition
componentToNFA (ComState statement) = statementToNFA statement
componentToNFA (ComConState conditionalStatement) = conditionalStatementToNFA conditionalStatement

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

    newTransitions1 <- generateTransitions endStates1 startStates2 
    newTransitions2 <- generateTransitions endStates2 startStates1 

    let allNewTransitions = newTransitions1 ++ newTransitions2 ::[Transition]

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

    newTransitions <- generateTransitions conditionStates definitionStates 

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

    newTransitions <- generateTransitions conditionStates definitionStates 

    return $ NFA
        { states = combinedStates
        , events = combinedEvents
        , transitions = Set.union (combinedTransitions) (Set.fromList newTransitions)
        , startStates = conditionStates
        , acceptingStates = definitionStates
        }
conditionalDefinitionToNFA (ConDefIfElse definition1 condition definition2) = do
    nfaCon <- conditionToNFA condition
    nfaDef1 <- definitionToNFA definition1
    nfaConNot <- notConditionToNFA condition
    nfaDef2 <- definitionToNFA definition2

    let combinedStates = Set.unions [(states nfaCon), (states nfaDef1), (states nfaConNot), (states nfaDef2)]
        combinedEvents = Set.unions [(events nfaCon), (events nfaDef1), (events nfaConNot), (events nfaDef2)]
        combinedTransitions = Set.unions [(transitions nfaCon), (transitions nfaDef1), (transitions nfaConNot), (transitions nfaDef2)]
        conditionStates = states nfaCon
        definitionStates1 = states nfaDef1
        nonConditionStates = states nfaConNot
        definitionStates2 = states nfaDef2

    newTransitions1 <- generateTransitions conditionStates definitionStates1
    newTransitions2 <- generateTransitions nonConditionStates definitionStates2

    return $ NFA
        { states = combinedStates
        , events = combinedEvents
        , transitions = Set.unions [(combinedTransitions), (Set.fromList newTransitions1), (Set.fromList newTransitions2)]
        , startStates = Set.union conditionStates nonConditionStates
        , acceptingStates = Set.union definitionStates1 definitionStates2
        }
conditionalDefinitionToNFA (ConDefIfThenElse condition definition1 definition2) = do
    nfaCon <- conditionToNFA condition
    nfaDef1 <- definitionToNFA definition1
    nfaConNot <- notConditionToNFA condition
    nfaDef2 <- definitionToNFA definition2

    let combinedStates = Set.unions [(states nfaCon), (states nfaDef1), (states nfaConNot), (states nfaDef2)]
        combinedEvents = Set.unions [(events nfaCon), (events nfaDef1), (events nfaConNot), (events nfaDef2)]
        combinedTransitions = Set.unions [(transitions nfaCon), (transitions nfaDef1), (transitions nfaConNot), (transitions nfaDef2)]
        conditionStates = states nfaCon
        definitionStates1 = states nfaDef1
        nonConditionStates = states nfaConNot
        definitionStates2 = states nfaDef2

    newTransitions1 <- generateTransitions conditionStates definitionStates1
    newTransitions2 <- generateTransitions nonConditionStates definitionStates2

    return $ NFA
        { states = combinedStates
        , events = combinedEvents
        , transitions = Set.unions [(combinedTransitions), (Set.fromList newTransitions1), (Set.fromList newTransitions2)]
        , startStates = Set.union conditionStates nonConditionStates
        , acceptingStates = Set.union definitionStates1 definitionStates2
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
        , transitions = combinedTransitions
        , startStates = combinedStates
        , acceptingStates = combinedStates
        }
statementToNFA (StateAnd simpleStatement statement) = do
    nfa1 <- simpleStatementToNFA simpleStatement
    nfa2 <- statementToNFA statement

    let combinedStates = Set.union (states nfa1) (states nfa2)
        combinedEvents = Set.union (events nfa1) (events nfa2)
        combinedTransitions = Set.union (transitions nfa1) (transitions nfa2)
        startStates1 = startStates nfa1
        startStates2 = startStates nfa2
        endStates1 = acceptingStates nfa1
        endStates2 = acceptingStates nfa2

    newTransitions1 <- generateTransitions endStates1 startStates2 
    newTransitions2 <- generateTransitions endStates2 startStates1 

    let allNewTransitions = newTransitions1 ++ newTransitions2 :: [Transition]

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
    nfaState <- statementToNFA statement

    let combinedStates = Set.union (states nfaCon) (states nfaState)
        combinedEvents = Set.union (events nfaCon) (events nfaState)
        combinedTransitions = Set.union (transitions nfaCon) (transitions nfaState)
        conditionStates = states nfaCon
        statementStates = states nfaState

    newTransitions <- generateTransitions conditionStates statementStates 

    return $ NFA
        { states = combinedStates
        , events = combinedEvents
        , transitions = Set.union (combinedTransitions) (Set.fromList newTransitions)
        , startStates = conditionStates
        , acceptingStates = statementStates
        }
conditionalStatementToNFA (ConStateIfThen condition statement) = do
    nfaCon <- conditionToNFA condition
    nfaState <- statementToNFA statement

    let combinedStates = Set.union (states nfaCon) (states nfaState)
        combinedEvents = Set.union (events nfaCon) (events nfaState)
        combinedTransitions = Set.union (transitions nfaCon) (transitions nfaState)
        conditionStates = states nfaCon
        statementStates = states nfaState

    newTransitions <- generateTransitions conditionStates statementStates 

    return $ NFA
        { states = combinedStates
        , events = combinedEvents
        , transitions = Set.union (combinedTransitions) (Set.fromList newTransitions)
        , startStates = conditionStates
        , acceptingStates = statementStates
        }
conditionalStatementToNFA (ConStateIfElse statement1 condition statement2) = do
    nfaCon <- conditionToNFA condition
    nfaState1 <- statementToNFA statement1
    nfaConNot <- notConditionToNFA condition
    nfaState2 <- statementToNFA statement2

    let combinedStates = Set.unions [(states nfaCon), (states nfaState1), (states nfaConNot), (states nfaState2)]
        combinedEvents = Set.unions [(events nfaCon), (events nfaState1), (events nfaConNot), (events nfaState2)]
        combinedTransitions = Set.unions [(transitions nfaCon), (transitions nfaState1), (transitions nfaConNot), (transitions nfaState2)]
        conditionStates = states nfaCon
        statementStates1 = states nfaState1
        nonConditionStates = states nfaConNot
        statementStates2 = states nfaState2

    newTransitions1 <- generateTransitions conditionStates statementStates1
    newTransitions2 <- generateTransitions nonConditionStates statementStates2

    return $ NFA
        { states = combinedStates
        , events = combinedEvents
        , transitions = Set.unions [(combinedTransitions), (Set.fromList newTransitions1), (Set.fromList newTransitions2)]
        , startStates = Set.union conditionStates nonConditionStates
        , acceptingStates = Set.union statementStates1 statementStates2
        }
conditionalStatementToNFA (ConStateIfThenElse condition statement1 statement2) = do
    nfaCon <- conditionToNFA condition
    nfaState1 <- statementToNFA statement1
    nfaConNot <- notConditionToNFA condition
    nfaState2 <- statementToNFA statement2

    let combinedStates = Set.unions [(states nfaCon), (states nfaState1), (states nfaConNot), (states nfaState2)]
        combinedEvents = Set.unions [(events nfaCon), (events nfaState1), (events nfaConNot), (events nfaState2)]
        combinedTransitions = Set.unions [(transitions nfaCon), (transitions nfaState1), (transitions nfaConNot), (transitions nfaState2)]
        conditionStates = states nfaCon
        statementStates1 = states nfaState1
        nonConditionStates = states nfaConNot
        statementStates2 = states nfaState2

    newTransitions1 <- generateTransitions conditionStates statementStates1
    newTransitions2 <- generateTransitions nonConditionStates statementStates2

    return $ NFA
        { states = combinedStates
        , events = combinedEvents
        , transitions = Set.unions [(combinedTransitions), (Set.fromList newTransitions1), (Set.fromList newTransitions2)]
        , startStates = Set.union conditionStates nonConditionStates
        , acceptingStates = Set.union statementStates1 statementStates2
        }

simpleDefinitionToNFA :: SimpleDefinition -> State StateDict NFA
simpleDefinitionToNFA (SimDefIs id subject1 subject2) = do
    let subjectStr1 = subjectToString subject1
        subjectStr2 = subjectToString subject2
        stateStr = subjectStr1 ++ " IS " ++ subjectStr2
        eventStr = subjectStr1 ++ " IS " ++ subjectStr2

        simDefState = StateA stateStr
        simDefEvent = Event eventStr

    addToStateDict simDefState simDefEvent

    return $ NFA
        { states = Set.singleton simDefState
        , events = Set.singleton simDefEvent
        , transitions = Set.empty
        , startStates = Set.singleton simDefState
        , acceptingStates = Set.singleton simDefState
        }
simpleDefinitionToNFA (SimDefEq id subject numericalExpression) = do
    let subjectStr = subjectToString subject
        numExpStr = numericalExpressionToString numericalExpression
        stateStr = subjectStr ++ " EQUALS " ++ numExpStr
        eventStr = subjectStr ++ " EQUALS " ++ numExpStr

        simDefState = StateA stateStr
        simDefEvent = Event eventStr

    addToStateDict simDefState simDefEvent

    return $ NFA
        { states = Set.singleton simDefState
        , events = Set.singleton simDefEvent
        , transitions = Set.empty
        , startStates = Set.singleton simDefState
        , acceptingStates = Set.singleton simDefState
        }
simpleDefinitionToNFA (SimDefDate id subject day month year) = do
    let subjectStr = subjectToString subject
        dateStr = dateSpeToString day month year
        stateStr = subjectStr ++ " IS " ++ dateStr
        eventStr = subjectStr ++ " IS " ++ dateStr

        simDefState = StateA stateStr
        simDefEvent = Event eventStr

    addToStateDict simDefState simDefEvent

    return $ NFA
        { states = Set.singleton simDefState
        , events = Set.singleton simDefEvent
        , transitions = Set.empty
        , startStates = Set.singleton simDefState
        , acceptingStates = Set.singleton simDefState
        }

conditionToNFA :: Condition -> State StateDict NFA
conditionToNFA (CondiSim simpleCondition) = simpleConditionToNFA simpleCondition
conditionToNFA (CondiOr simpleCondition condition) = do
    nfa1 <- simpleConditionToNFA simpleCondition
    nfa2 <- conditionToNFA condition

    let combinedStates = Set.union (states nfa1) (states nfa2)
        combinedEvents = Set.union (events nfa1) (events nfa2)
        combinedTransitions = Set.union (transitions nfa1) (transitions nfa2)

    return $ NFA
        { states = combinedStates
        , events = combinedEvents
        , transitions = combinedTransitions
        , startStates = combinedStates
        , acceptingStates = combinedStates
        }
conditionToNFA (CondiAnd simpleCondition condition) = do
    nfa1 <- simpleConditionToNFA simpleCondition
    nfa2 <- conditionToNFA condition

    let combinedStates = Set.union (states nfa1) (states nfa2)
        combinedEvents = Set.union (events nfa1) (events nfa2)
        combinedTransitions = Set.union (transitions nfa1) (transitions nfa2)
        startStates1 = startStates nfa1
        startStates2 = startStates nfa2
        endStates1 = acceptingStates nfa1
        endStates2 = acceptingStates nfa2

    newTransitions1 <- generateTransitions endStates1 startStates2 
    newTransitions2 <- generateTransitions endStates2 startStates1 

    let allNewTransitions = newTransitions1 ++ newTransitions2 :: [Transition]

    return $ NFA
        { states = combinedStates
        , events = combinedEvents
        , transitions = Set.union (combinedTransitions) (Set.fromList allNewTransitions)
        , startStates = Set.union (startStates1) (startStates2)
        , acceptingStates = Set.union (endStates1) (endStates2)
        }

notConditionToNFA :: Condition -> State StateDict NFA
notConditionToNFA (CondiSim simpleCondition) = notSimpleConditionToNFA simpleCondition
notConditionToNFA (CondiOr simpleCondition condition) = do
    nfa1 <- notSimpleConditionToNFA simpleCondition
    nfa2 <- notConditionToNFA condition

    let combinedStates = Set.union (states nfa1) (states nfa2)
        combinedEvents = Set.union (events nfa1) (events nfa2)
        combinedTransitions = Set.union (transitions nfa1) (transitions nfa2)
        startStates1 = startStates nfa1
        startStates2 = startStates nfa2
        endStates1 = acceptingStates nfa1
        endStates2 = acceptingStates nfa2

    newTransitions1 <- generateTransitions endStates1 startStates2 
    newTransitions2 <- generateTransitions endStates2 startStates1 

    let allNewTransitions = newTransitions1 ++ newTransitions2 :: [Transition]

    return $ NFA
        { states = combinedStates
        , events = combinedEvents
        , transitions = Set.union (combinedTransitions) (Set.fromList allNewTransitions)
        , startStates = Set.union (startStates1) (startStates2)
        , acceptingStates = Set.union (endStates1) (endStates2)
        }
notconditionToNFA (CondiAnd simpleCondition condition) = do
    nfa1 <- notSimpleConditionToNFA simpleCondition
    nfa2 <- notConditionToNFA condition

    let combinedStates = Set.union (states nfa1) (states nfa2)
        combinedEvents = Set.union (events nfa1) (events nfa2)
        combinedTransitions = Set.union (transitions nfa1) (transitions nfa2)

    return $ NFA
        { states = combinedStates
        , events = combinedEvents
        , transitions = combinedTransitions
        , startStates = combinedStates
        , acceptingStates = combinedStates
        }

simpleStatementToNFA :: SimpleStatement -> State StateDict NFA
simpleStatementToNFA (SimStateOne id holds subject modalVerb verb object receiver date) = 
    createNFASimpleStatement holds subject modalVerb verb object receiver date
simpleStatementToNFA (SimStateTwo id holds subject date modalVerb verb object receiver) =
    createNFASimpleStatement holds subject modalVerb verb object receiver date
simpleStatementToNFA (SimStateThree id holds date subject modalVerb verb object receiver) =
    createNFASimpleStatement holds subject modalVerb verb object receiver date
simpleStatementToNFA (SimStateFour id holds subject verbStatus object receiver date) =
    createNFASimpleCondition holds subject verbStatus object receiver date
simpleStatementToNFA (SimStateOneNH id subject modalVerb verb object receiver date) = 
    createNFASimpleStatementNH subject modalVerb verb object receiver date
simpleStatementToNFA (SimStateTwoNH id subject date modalVerb verb object receiver) =
    createNFASimpleStatementNH subject modalVerb verb object receiver date
simpleStatementToNFA (SimStateThreeNH id date subject modalVerb verb object receiver) =
    createNFASimpleStatementNH subject modalVerb verb object receiver date
simpleStatementToNFA (SimStateFourNH id subject verbStatus object receiver date) =
    createNFASimpleConditionNH subject verbStatus object receiver date

createNFASimpleStatement :: Holds -> Subject -> ModalVerb -> Verb -> Object -> Receiver -> Date ->  State StateDict NFA
createNFASimpleStatement holds subject modalVerb verb object receiver date = 
    case (holds, modalVerb) of
         (HoldYes, ModalObli _) -> must
         (HoldNo, ModalObli _) -> may
         (HoldYes, ModalPermi) -> may
         (HoldNo, ModalPermi) -> mustNot
         (HoldYes, ModalForbi) -> mustNot
         (HoldNo, ModalForbi) -> may

    where
        mustStateStr = subjectToString subject ++ " MUST " ++ verbToString verb ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        mustEventStr = subjectToString subject ++ " " ++ verbToVerbStatusString verb ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        mayStateStr = subjectToString subject ++ " MAY " ++ verbToString verb ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        mayEventStr = subjectToString subject ++ " " ++ verbToVerbStatusString verb ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        mayEventStrDoNothing = "EPSILON"
        mustNotStateStr = subjectToString subject ++ " MUST NOT " ++ verbToString verb ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        mustNotEventStr = subjectToString subject ++ " DIDN'T " ++ verbToString verb ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date

        mustState = StateA mustStateStr
        mustEvent = Event mustEventStr
        mayState = StateA mayStateStr
        mayEvent = Event mayEventStr
        mayDefaultEvent = Event mayEventStrDoNothing
        mayDefaultTransition = Transition mayState mayEvent mayState
        mustNotState = StateA mustNotStateStr
        mustNotEvent = Event mustNotEventStr

        must = do
            addToStateDict mustState mustEvent
            return $ NFA
                { states = Set.singleton mustState
                , events = Set.singleton mustEvent
                , transitions = Set.empty
                , startStates = Set.singleton mustState
                , acceptingStates = Set.singleton mustState
                }
    
        may = do
            addToStateDict mayState mayEvent
            addToStateDict mayState mayDefaultEvent
            return $ NFA
                { states = Set.singleton mayState
                , events = Set.fromList [mayEvent, mayDefaultEvent]
                , transitions = Set.singleton mayDefaultTransition
                , startStates = Set.singleton mayState
                , acceptingStates = Set.singleton mayState
                }
    
        mustNot = do
            addToStateDict mustNotState mustNotEvent
            return $ NFA
                { states = Set.singleton mustNotState
                , events = Set.singleton mustNotEvent
                , transitions = Set.empty
                , startStates = Set.singleton mustNotState
                , acceptingStates = Set.singleton mustNotState
                }

notCreateNFASimpleStatement :: Holds -> Subject -> ModalVerb -> Verb -> Object -> Receiver -> Date ->  State StateDict NFA
notCreateNFASimpleStatement holds subject modalVerb verb object receiver date = 
    case (holds, modalVerb) of
         (HoldYes, ModalObli _) -> may
         (HoldNo, ModalObli _) -> must
         (HoldYes, ModalPermi) -> mustNot
         (HoldNo, ModalPermi) -> may
         (HoldYes, ModalForbi) -> may
         (HoldNo, ModalForbi) -> mustNot

    where
        mustStateStr = subjectToString subject ++ " MUST " ++ verbToString verb ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        mustEventStr = subjectToString subject ++ " " ++ verbToVerbStatusString verb ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        mayStateStr = subjectToString subject ++ " MAY " ++ verbToString verb ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        mayEventStr = subjectToString subject ++ " " ++ verbToVerbStatusString verb ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        mayEventStrDoNothing = "EPSILON"
        mustNotStateStr = subjectToString subject ++ " MUST NOT " ++ verbToString verb ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        mustNotEventStr = subjectToString subject ++ " DIDN'T " ++ verbToString verb ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date

        mustState = StateA mustStateStr
        mustEvent = Event mustEventStr
        mayState = StateA mayStateStr
        mayEvent = Event mayEventStr
        mayDefaultEvent = Event mayEventStrDoNothing
        mayDefaultTransition = Transition mayState mayEvent mayState
        mustNotState = StateA mustNotStateStr
        mustNotEvent = Event mustNotEventStr

        must = do
            addToStateDict mustState mustEvent
            return $ NFA
                { states = Set.singleton mustState
                , events = Set.singleton mustEvent
                , transitions = Set.empty
                , startStates = Set.singleton mustState
                , acceptingStates = Set.singleton mustState
                }
    
        may = do
            addToStateDict mayState mayEvent
            addToStateDict mayState mayDefaultEvent
            return $ NFA
                { states = Set.singleton mayState
                , events = Set.fromList [mayEvent, mayDefaultEvent]
                , transitions = Set.singleton mayDefaultTransition
                , startStates = Set.singleton mayState
                , acceptingStates = Set.singleton mayState
                }
    
        mustNot = do
            addToStateDict mustNotState mustNotEvent
            return $ NFA
                { states = Set.singleton mustNotState
                , events = Set.singleton mustNotEvent
                , transitions = Set.empty
                , startStates = Set.singleton mustNotState
                , acceptingStates = Set.singleton mustNotState
                }

createNFASimpleStatementNH :: Subject -> ModalVerb -> Verb -> Object -> Receiver -> Date ->  State StateDict NFA
createNFASimpleStatementNH subject modalVerb verb object receiver date = 
    case (modalVerb) of
         (ModalObli _) -> must
         (ModalPermi) -> may
         (ModalForbi) -> mustNot

    where
        mustStateStr = subjectToString subject ++ " MUST " ++ verbToString verb ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        mustEventStr = subjectToString subject ++ " " ++ verbToVerbStatusString verb ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        mayStateStr = subjectToString subject ++ " MAY " ++ verbToString verb ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        mayEventStr = subjectToString subject ++ " " ++ verbToVerbStatusString verb ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        mayEventStrDoNothing = "EPSILON"
        mustNotStateStr = subjectToString subject ++ " MUST NOT " ++ verbToString verb ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        mustNotEventStr = subjectToString subject ++ " DIDN'T " ++ verbToString verb ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date

        mustState = StateA mustStateStr
        mustEvent = Event mustEventStr
        mayState = StateA mayStateStr
        mayEvent = Event mayEventStr
        mayDefaultEvent = Event mayEventStrDoNothing
        mayDefaultTransition = Transition mayState mayEvent mayState
        mustNotState = StateA mustNotStateStr
        mustNotEvent = Event mustNotEventStr

        must = do
            addToStateDict mustState mustEvent
            return $ NFA
                { states = Set.singleton mustState
                , events = Set.singleton mustEvent
                , transitions = Set.empty
                , startStates = Set.singleton mustState
                , acceptingStates = Set.singleton mustState
                }
    
        may = do
            addToStateDict mayState mayEvent
            addToStateDict mayState mayDefaultEvent
            return $ NFA
                { states = Set.singleton mayState
                , events = Set.fromList [mayEvent, mayDefaultEvent]
                , transitions = Set.singleton mayDefaultTransition
                , startStates = Set.singleton mayState
                , acceptingStates = Set.singleton mayState
                }
    
        mustNot = do
            addToStateDict mustNotState mustNotEvent
            return $ NFA
                { states = Set.singleton mustNotState
                , events = Set.singleton mustNotEvent
                , transitions = Set.empty
                , startStates = Set.singleton mustNotState
                , acceptingStates = Set.singleton mustNotState
                }

notCreateNFASimpleStatementNH :: Subject -> ModalVerb -> Verb -> Object -> Receiver -> Date ->  State StateDict NFA
notCreateNFASimpleStatementNH subject modalVerb verb object receiver date = 
    case (modalVerb) of
         (ModalObli _) -> may
         (ModalPermi) -> mustNot
         (ModalForbi) -> may

    where
        mayStateStr = subjectToString subject ++ " MAY " ++ verbToString verb ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        mayEventStr = subjectToString subject ++ " " ++ verbToVerbStatusString verb ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        mayEventStrDoNothing = "EPSILON"
        mustNotStateStr = subjectToString subject ++ " MUST NOT " ++ verbToString verb ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        mustNotEventStr = subjectToString subject ++ " DIDN'T " ++ verbToString verb ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date

        mayState = StateA mayStateStr
        mayEvent = Event mayEventStr
        mayDefaultEvent = Event mayEventStrDoNothing
        mayDefaultTransition = Transition mayState mayEvent mayState
        mustNotState = StateA mustNotStateStr
        mustNotEvent = Event mustNotEventStr
    
        may = do
            addToStateDict mayState mayEvent
            addToStateDict mayState mayDefaultEvent
            return $ NFA
                { states = Set.singleton mayState
                , events = Set.fromList [mayEvent, mayDefaultEvent]
                , transitions = Set.singleton mayDefaultTransition
                , startStates = Set.singleton mayState
                , acceptingStates = Set.singleton mayState
                }
    
        mustNot = do
            addToStateDict mustNotState mustNotEvent
            return $ NFA
                { states = Set.singleton mustNotState
                , events = Set.singleton mustNotEvent
                , transitions = Set.empty
                , startStates = Set.singleton mustNotState
                , acceptingStates = Set.singleton mustNotState
                }

simpleConditionToNFA :: SimpleCondition -> State StateDict NFA
simpleConditionToNFA (SimConOne id holds subject verbStatus object receiver date) =
    createNFASimpleCondition holds subject verbStatus object receiver date
simpleConditionToNFA (SimConTwo id holds subject date verbStatus object receiver) =
    createNFASimpleCondition holds subject verbStatus object receiver date
simpleConditionToNFA (SimConThree id holds date subject verbStatus object receiver) =
    createNFASimpleCondition holds subject verbStatus object receiver date
simpleConditionToNFA (SimConFour id holds subject modalVerb verb object receiver date) =
    createNFASimpleStatement holds subject modalVerb verb object receiver date
simpleConditionToNFA (SimConFive id holds booleanExpression) =
    createNFABooleanExpression holds booleanExpression
simpleConditionToNFA (SimConOneNH id subject verbStatus object receiver date) =
    createNFASimpleConditionNH subject verbStatus object receiver date
simpleConditionToNFA (SimConTwoNH id subject date verbStatus object receiver) =
    createNFASimpleConditionNH subject verbStatus object receiver date
simpleConditionToNFA (SimConThreeNH id date subject verbStatus object receiver) =
    createNFASimpleConditionNH subject verbStatus object receiver date
simpleConditionToNFA (SimConFourNH id subject modalVerb verb object receiver date) =
    createNFASimpleStatementNH subject modalVerb verb object receiver date
simpleConditionToNFA (SimConFiveNH id booleanExpression) =
    createNFABooleanExpressionNH booleanExpression

notSimpleConditionToNFA :: SimpleCondition -> State StateDict NFA
notSimpleConditionToNFA (SimConOne id holds subject verbStatus object receiver date) =
    notCreateNFASimpleCondition holds subject verbStatus object receiver date
notSimpleConditionToNFA (SimConTwo id holds subject date verbStatus object receiver) =
    notCreateNFASimpleCondition holds subject verbStatus object receiver date
notSimpleConditionToNFA (SimConThree id holds date subject verbStatus object receiver) =
    notCreateNFASimpleCondition holds subject verbStatus object receiver date
notSimpleConditionToNFA (SimConFour id holds subject modalVerb verb object receiver date) =
    notCreateNFASimpleStatement holds subject modalVerb verb object receiver date
notSimpleConditionToNFA (SimConFive id holds booleanExpression) =
    notCreateNFABooleanExpression holds booleanExpression
notSimpleConditionToNFA (SimConOneNH id subject verbStatus object receiver date) =
    notCreateNFASimpleConditionNH subject verbStatus object receiver date
notSimpleConditionToNFA (SimConTwoNH id subject date verbStatus object receiver) =
    notCreateNFASimpleConditionNH subject verbStatus object receiver date
notSimpleConditionToNFA (SimConThreeNH id date subject verbStatus object receiver) =
    notCreateNFASimpleConditionNH subject verbStatus object receiver date
notSimpleConditionToNFA (SimConFourNH id subject modalVerb verb object receiver date) =
    notCreateNFASimpleStatementNH subject modalVerb verb object receiver date
notSimpleConditionToNFA (SimConFiveNH id booleanExpression) =
    notCreateNFABooleanExpressionNH booleanExpression

createNFASimpleCondition :: Holds -> Subject -> VerbStatus -> Object -> Receiver -> Date -> State StateDict NFA
createNFASimpleCondition holds subject verbStatus object receiver date = 
    case holds of
        (HoldYes) -> did
        (HoldNo) -> didNot

    where 
        didStateStr = subjectToString subject ++ " " ++ verbStatusToString verbStatus ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        didEventStr = subjectToString subject ++ " " ++ verbStatusToString verbStatus ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        didNotStateStr = subjectToString subject ++ " DIDN'T " ++ verbStatusToVerbString verbStatus ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        didNotEventStr = subjectToString subject ++ " DIDN'T " ++ verbStatusToVerbString verbStatus ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date

        didState = StateA didStateStr
        didEvent = Event didEventStr
        didNotState = StateA didNotStateStr
        didNotEvent = Event didNotEventStr

        did = do
            addToStateDict didState didEvent
            return $ NFA
                { states = Set.singleton didState
                , events = Set.singleton didEvent
                , transitions = Set.empty
                , startStates = Set.singleton didState
                , acceptingStates = Set.singleton didState
                }
        didNot = do
            addToStateDict didNotState didNotEvent
            return $ NFA
                { states = Set.singleton didNotState
                , events = Set.singleton didNotEvent
                , transitions = Set.empty
                , startStates = Set.singleton didNotState
                , acceptingStates = Set.singleton didNotState
                }

notCreateNFASimpleCondition :: Holds -> Subject -> VerbStatus -> Object -> Receiver -> Date -> State StateDict NFA
notCreateNFASimpleCondition holds subject verbStatus object receiver date = 
    case holds of
        (HoldYes) -> didNot
        (HoldNo) -> did

    where 
        didStateStr = subjectToString subject ++ " " ++ verbStatusToString verbStatus ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        didEventStr = subjectToString subject ++ " " ++ verbStatusToString verbStatus ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        didNotStateStr = subjectToString subject ++ " DIDN'T " ++ verbStatusToVerbString verbStatus ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        didNotEventStr = subjectToString subject ++ " DIDN'T " ++ verbStatusToVerbString verbStatus ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date

        didState = StateA didStateStr
        didEvent = Event didEventStr
        didNotState = StateA didNotStateStr
        didNotEvent = Event didNotEventStr

        did = do
            addToStateDict didState didEvent
            return $ NFA
                { states = Set.singleton didState
                , events = Set.singleton didEvent
                , transitions = Set.empty
                , startStates = Set.singleton didState
                , acceptingStates = Set.singleton didState
                }
        didNot = do
            addToStateDict didNotState didNotEvent
            return $ NFA
                { states = Set.singleton didNotState
                , events = Set.singleton didNotEvent
                , transitions = Set.empty
                , startStates = Set.singleton didNotState
                , acceptingStates = Set.singleton didNotState
                }

createNFASimpleConditionNH :: Subject -> VerbStatus -> Object -> Receiver -> Date -> State StateDict NFA
createNFASimpleConditionNH subject verbStatus object receiver date = do
    let didStateStr = subjectToString subject ++ " " ++ verbStatusToString verbStatus ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        didEventStr = subjectToString subject ++ " " ++ verbStatusToString verbStatus ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date

        didState = StateA didStateStr
        didEvent = Event didEventStr

    addToStateDict didState didEvent

    return $ NFA
        { states = Set.singleton didState
        , events = Set.singleton didEvent
        , transitions = Set.empty
        , startStates = Set.singleton didState
        , acceptingStates = Set.singleton didState
        }

notCreateNFASimpleConditionNH :: Subject -> VerbStatus -> Object -> Receiver -> Date -> State StateDict NFA
notCreateNFASimpleConditionNH subject verbStatus object receiver date = do
    let didStateStr = subjectToString subject ++ " DIDN'T " ++ verbStatusToString verbStatus ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date
        didEventStr = subjectToString subject ++ " DIDN'T " ++ verbStatusToString verbStatus ++ " " ++ objectToString object ++ " to " ++ receiverToString receiver ++ " " ++ dateToString date

        didState = StateA didStateStr
        didEvent = Event didEventStr

    addToStateDict didState didEvent

    return $ NFA
        { states = Set.singleton didState
        , events = Set.singleton didEvent
        , transitions = Set.empty
        , startStates = Set.singleton didState
        , acceptingStates = Set.singleton didState
        }

createNFABooleanExpression :: Holds -> BooleanExpression -> State StateDict NFA
createNFABooleanExpression holds boolEx = 
    case holds of
      (HoldYes) -> yes
      (HoldNo) -> no

    where 
        yesStateStr = yesBooleanExpressionToString boolEx
        yesEventStr = yesBooleanExpressionToString boolEx
        noStateStr = noBooleanExpressionToString boolEx
        noEventStr = noBooleanExpressionToString boolEx

        yesState = StateA yesStateStr
        yesEvent = Event yesEventStr
        noState = StateA noStateStr
        noEvent = Event noEventStr

        yes = do
            addToStateDict yesState yesEvent
            return $ NFA
                { states = Set.singleton yesState
                , events = Set.singleton yesEvent
                , transitions = Set.empty
                , startStates = Set.singleton yesState
                , acceptingStates = Set.singleton yesState
                }
        no = do
            addToStateDict noState noEvent
            return $ NFA
                { states = Set.singleton noState
                , events = Set.singleton noEvent
                , transitions = Set.empty
                , startStates = Set.singleton noState
                , acceptingStates = Set.singleton noState
                }

notCreateNFABooleanExpression :: Holds -> BooleanExpression -> State StateDict NFA
notCreateNFABooleanExpression holds boolEx = 
    case holds of
      (HoldYes) -> no
      (HoldNo) -> yes

    where 
        yesStateStr = yesBooleanExpressionToString boolEx
        yesEventStr = yesBooleanExpressionToString boolEx
        noStateStr = noBooleanExpressionToString boolEx
        noEventStr = noBooleanExpressionToString boolEx

        yesState = StateA yesStateStr
        yesEvent = Event yesEventStr
        noState = StateA noStateStr
        noEvent = Event noEventStr

        yes = do
            addToStateDict yesState yesEvent
            return $ NFA
                { states = Set.singleton yesState
                , events = Set.singleton yesEvent
                , transitions = Set.empty
                , startStates = Set.singleton yesState
                , acceptingStates = Set.singleton yesState
                }
        no = do
            addToStateDict noState noEvent
            return $ NFA
                { states = Set.singleton noState
                , events = Set.singleton noEvent
                , transitions = Set.empty
                , startStates = Set.singleton noState
                , acceptingStates = Set.singleton noState
                }

createNFABooleanExpressionNH :: BooleanExpression -> State StateDict NFA
createNFABooleanExpressionNH boolEx = do
    let yesStateStr = yesBooleanExpressionToString boolEx
        yesEventStr = yesBooleanExpressionToString boolEx

        yesState = StateA yesStateStr
        yesEvent = Event yesEventStr

    addToStateDict yesState yesEvent
    return $ NFA
        { states = Set.singleton yesState
        , events = Set.singleton yesEvent
        , transitions = Set.empty
        , startStates = Set.singleton yesState
        , acceptingStates = Set.singleton yesState
        }

notCreateNFABooleanExpressionNH :: BooleanExpression -> State StateDict NFA
notCreateNFABooleanExpressionNH boolEx = do
    let yesStateStr = noBooleanExpressionToString boolEx
        yesEventStr = noBooleanExpressionToString boolEx

        yesState = StateA yesStateStr
        yesEvent = Event yesEventStr

    addToStateDict yesState yesEvent
    return $ NFA
        { states = Set.singleton yesState
        , events = Set.singleton yesEvent
        , transitions = Set.empty
        , startStates = Set.singleton yesState
        , acceptingStates = Set.singleton yesState
        }

yesBooleanExpressionToString :: BooleanExpression -> String
yesBooleanExpressionToString (BoolEx subject1 verbStatus comparison subject2) =
    subjectToString subject1 ++ " " ++ verbStatusToString verbStatus ++ " " ++ comparisonToString comparison ++ " " ++ subjectToString subject2

noBooleanExpressionToString :: BooleanExpression -> String
noBooleanExpressionToString (BoolEx subject1 verbStatus comparison subject2) =
    subjectToString subject1 ++ " DIDN'T " ++ verbStatusToVerbString verbStatus ++ " " ++ comparisonToString comparison ++ " " ++ subjectToString subject2

runNFAConversion :: Contract -> NFA
runNFAConversion contract = evalState (contractToNFA contract) (Map.empty)

nfaToGraph :: NFA -> Gr Text Text
nfaToGraph nfa =
    mkGraph nodes edges
    where
        allStates = Set.toList $ states nfa
        nodeMap = zip allStates [1 ..]
        nodes = [(nodeId, labelWithId (nodeId) (stateToLabel state)) | (state, nodeId) <- nodeMap]
        
        sortedTransitions = sort $ Set.toList $ transitions nfa
        groupedTransitions = group sortedTransitions
        
        edges = concatMap (\transGroup -> 
                    case transGroup of
                        [] -> []
                        (Transition state1 event state2 : _) -> 
                            let (Just nodeId1) = lookup state1 nodeMap
                                (Just nodeId2) = lookup state2 nodeMap
                                labeledEvent = eventToLabel event
                            in [(nodeId1, nodeId2, labeledEvent)]
                    ) groupedTransitions

labelWithId :: Node -> Text -> Text
labelWithId nodeId label = L.pack "(" <> L.pack (show nodeId) <> L.pack ") " <> label
                    
stateToLabel :: StateA -> Text
stateToLabel (StateA s) = L.pack s

eventToLabel :: Event -> Text
eventToLabel (Event s) = L.pack s

isAccepting :: Text -> NFA -> Bool
isAccepting state nfa = (convertStringToState $ L.unpack state) `Set.member` (acceptingStates nfa)

convertStringToState :: String -> StateA
convertStringToState str = StateA (removeFirstSpace (stripIntegerLabel str))

-- Function to strip off "[integer]" from a string
stripIntegerLabel :: String -> String
stripIntegerLabel str =
    case stripPrefix "(" str of
        Just rest -> case break (== ')') rest of
            (number, ')':remainder) -> stripIntegerLabel remainder
            _                       -> str
        Nothing   -> str

-- Function to remove the first space in a string
removeFirstSpace :: String -> String
removeFirstSpace (' ' : rest) = rest
removeFirstSpace str = str

visualizeGraph :: NFA -> Gr Text Text -> String
visualizeGraph nfa graph = L.unpack dotText
    where
        dotGraph = graphToDot (labelledNodesParams nfa) graph :: DotGraph Node
        dotText = renderDot $ toDot dotGraph  
    
visualizeGraph' :: NFA -> Gr Text Text -> IO ()
visualizeGraph' nfa graph = do
    let dotGraph = graphToDot (labelledNodesParams nfa) graph :: DotGraph Node
        dotText = renderDot $ toDot dotGraph
    putStrLn $ L.unpack dotText

labelledNodesParams :: NFA -> GraphvizParams Node Text Text () Text
labelledNodesParams nfa = nonClusteredParams
    { fmtNode = \(_, label) ->
        let nodeColor = if (isAccepting label nfa) then Green else LightGray
        in [Gv.Label (Gv.StrLabel label), Gv.FillColor [Gvc.toWColor nodeColor], Gv.Style [filled]]
    , fmtEdge = \(_, _, edgeLabel) -> [Gv.Label (Gv.StrLabel edgeLabel)]
    }

removeSelfLoopingTransitions :: NFA -> NFA
removeSelfLoopingTransitions nfa =
    let noSelfLoopingTransitions = Set.filter (\(Transition from _ to) -> from /= to) (transitions nfa)
    in nfa { transitions = noSelfLoopingTransitions }

findPaths :: Gr Text Text -> Node -> Node -> Int -> [[Node]]
findPaths graph startNode endNode maxEdges = dfs startNode [startNode] 0
    where
    -- Helper function for DFS
    dfs :: Node -> [Node] -> Int -> [[Node]]
    dfs currentNode path edges
        | currentNode == endNode && edges == maxEdges = [reverse path]
        | edges == maxEdges = []  -- Reached the maximum number of edges without reaching the endNode
        | otherwise = concatMap (dfsNext currentNode path edges) (suc graph currentNode)

    -- Helper function to explore next nodes in DFS
    dfsNext :: Node -> [Node] -> Int -> Node -> [[Node]]
    dfsNext currentNode path edges nextNode
        | nextNode `elem` path = []  -- Avoid revisiting nodes
        | otherwise = dfs nextNode (nextNode : path) (edges + 1)

nodesToEdges :: [Node] -> [Edge]
nodesToEdges nodes = zip nodes (tail nodes)

nodesListsToEdgesLists :: [[Node]] -> [[Edge]]
nodesListsToEdgesLists = map nodesToEdges

countOccurrences :: (Eq a) => a -> [[a]] -> Int
countOccurrences elem = length . filter (any (elem ==))

checkNodeOccurrences :: [[Node]] -> Node -> Int
checkNodeOccurrences listsOfNodes node =
  case countOccurrences node listsOfNodes of
    0 -> 0  -- Node is not in any list
    1 -> 1  -- Node is in exactly one list
    _ -> 2  -- Node is in more than one list

checkEdgeOccurrences :: [[Edge]] -> Edge -> Int
checkEdgeOccurrences listsOfEdges edge =
  case countOccurrences edge listsOfEdges of
    0 -> 0  -- Edge is not in any list
    1 -> 1  -- Edge is in exactly one list
    _ -> 2  -- Edge is in more than one list

visualizeGraphPP :: [[Node]] -> Gr Text Text -> String
visualizeGraphPP pathNodes graph = L.unpack dotText
    where
        dotGraph = graphToDot (labelledNodesParamsPP pathNodes) graph :: DotGraph Node
        dotText = renderDot $ toDot dotGraph  

labelledNodesParamsPP :: [[Node]] -> GraphvizParams Node Text Text () Text
labelledNodesParamsPP pathNodes = nonClusteredParams
    { fmtNode = \(nodeId, label) ->
        let nodeColor
                | checkNodeOccurrences pathNodes nodeId == 0 = LightGray
                | checkNodeOccurrences pathNodes nodeId == 1 = LightBlue
                | checkNodeOccurrences pathNodes nodeId == 2 = Orange
        in [Gv.Label (Gv.StrLabel label), Gv.FillColor [Gvc.toWColor nodeColor], Gv.Style [filled]]
    , fmtEdge = \(nodeId1, nodeId2, edgeLabel) -> 
        let edge = (nodeId1, nodeId2)
            pathEdges = nodesListsToEdgesLists pathNodes
            edgeColor
                | checkEdgeOccurrences pathEdges edge == 0 = LightGray
                | checkEdgeOccurrences pathEdges edge == 1 = Blue
                | checkEdgeOccurrences pathEdges edge == 2 = Orange
            fontColor 
                | checkEdgeOccurrences pathEdges edge == 0 = Gvc.X11Color LightGray
                | checkEdgeOccurrences pathEdges edge == 1 = Gvc.X11Color Blue
                | checkEdgeOccurrences pathEdges edge == 2 = Gvc.X11Color Orange
        in [Gv.Label (Gv.StrLabel edgeLabel), Gv.FillColor [Gvc.toWColor edgeColor], Gv.FontColor fontColor, Gv.Color [Gvc.toWColor edgeColor]]
    }

visualisePossiblePath :: NFA -> Node -> Node -> Int -> String
visualisePossiblePath nfa startNode endNode numEdges =
    let nfa1 = removeSelfLoopingTransitions nfa
        graph = nfaToGraph nfa
        graph1 = nfaToGraph nfa1
        possiblePathNode = findPaths graph1 startNode endNode numEdges
        
    in visualizeGraphPP possiblePathNode graph
