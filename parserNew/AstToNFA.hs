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
  , zip
  )

import Data.Time
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad.State
import Data.List (map) 

-- import Data.GraphViz
-- import Data.GraphViz.Attributes.Complete
-- import Data.GraphViz.Printing
-- import Data.Graph.Inductive.Graph
-- import Data.Graph.Inductive.PatriciaTree
-- import qualified Data.Text.Lazy as L

import              Data.Functor                        ((<&>))
import qualified    Data.Text.Lazy as L                 (pack, unpack)
import qualified    Data.Text.Lazy.IO as IO             (putStrLn)
import              Data.Graph.Inductive.Graph          
import              Data.Graph.Inductive.PatriciaTree   (Gr)
import              Data.GraphViz                       
import              Data.GraphViz.Attributes.Complete   
import              Data.GraphViz.Printing              (renderDot, toDot)
import Data.Text.Internal.Lazy (Text)
import Data.Graph.Inductive (LEdge)

import AbsCoLa   
import LexCoLa   ( Token, mkPosToken )
import ParCoLa   ( pContract, myLexer )
import PrintCoLa ( Print, printTree )
import SkelCoLa  ()

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

-- -- Pretty-printing for StateA
-- instance Show StateA where
--     show (StateA s) = s

-- -- Pretty-printing for Event
-- instance Show Event where
--     show (Event e) = e

-- -- Pretty-printing for Transition
-- instance Show Transition where
--     show (Transition from event to) = show from ++ " --(" ++ show event ++ ")--> " ++ show to

data NFA = NFA
    { states :: Set.Set StateA
    , events :: Set.Set Event
    , transitions :: Set.Set Transition
    , startStates :: Set.Set StateA
    , acceptingStates :: Set.Set StateA
    } 
  deriving (Eq, Read, Show)

-- -- Pretty-printing for NFA
-- instance Show NFA where
--   show nfa =
--     unlines
--       [ "States:"
--       , unlines (map show (Set.toList $ states nfa))
--       , "Events:"
--       , unlines (map show (Set.toList $ events nfa))
--       , "Transitions:"
--       , unlines (map show (Set.toList $ transitions nfa))
--       , "Start States:"
--       , unlines (map show (Set.toList $ startStates nfa))
--       , "Accepting States:"
--       , unlines (map show (Set.toList $ acceptingStates nfa))
--       ]

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

yesBooleanExpressionToString :: BooleanExpression -> String
yesBooleanExpressionToString (BoolEx subject1 verbStatus comparison subject2) =
    subjectToString subject1 ++ " " ++ verbStatusToString verbStatus ++ " " ++ comparisonToString comparison ++ " " ++ subjectToString subject2

noBooleanExpressionToString :: BooleanExpression -> String
noBooleanExpressionToString (BoolEx subject1 verbStatus comparison subject2) =
    subjectToString subject1 ++ " DIDN'T " ++ verbStatusToVerbString verbStatus ++ " " ++ comparisonToString comparison ++ " " ++ subjectToString subject2

runNFAConversion :: Contract -> NFA
runNFAConversion contract = evalState (contractToNFA contract) (Map.empty)

-- The function is good but there is still a small bug to fix
-- all transitions is repeated for three times, need to check the loop to see what's wrong
-- jiayousss

nfaToGraph :: NFA -> Gr Text Text
nfaToGraph nfa =
    mkGraph nodes edges
    where
        allStates = Set.toList $ states nfa
        nodeMap = zip allStates [1 ..]
        nodes = [(nodeId, stateToLabel state) | (state, nodeId) <- nodeMap]
        edges = [(nodeId1, nodeId2, (eventToLabel event)) |
                Transition state1 event state2 <- Set.toList $ transitions nfa,
                (state1, nodeId1) <- nodeMap,
                (state2, nodeId2) <- nodeMap]

stateToLabel :: StateA -> Text
stateToLabel (StateA s) = L.pack s

eventToLabel :: Event -> Text
eventToLabel (Event s) = L.pack s

visualizeGraphText :: Gr Text Text -> IO ()
visualizeGraphText graph = do
  let dotGraph = graphToDot labelledNodesParamsText graph :: DotGraph Node
      dotText = renderDot $ toDot dotGraph
  putStrLn $ L.unpack dotText

labelledNodesParamsText :: GraphvizParams Node Text Text () Text
labelledNodesParamsText = nonClusteredParams
  { fmtNode = \(_, label) -> [Label (StrLabel label)]
  , fmtEdge = \(_, _, edgeLabel) -> [Label (StrLabel edgeLabel)]
  }
