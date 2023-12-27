module AstToPetriNet where

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

import AbsCoLa   
import LexCoLa   ( Token, mkPosToken )
import ParCoLa   ( pContract, myLexer )
import PrintCoLa ( Print, printTree )
import SkelCoLa  ()

import Data.List (map)

data PNTest = TRUE | FALSE

data PNModalVerb = PNSHALL | PNSHANT | PNMAY

data PNStatement 
    = PNTemporalActionStatement PNTest Subject PNModalVerb Verb Object Receiver Date

data PNConditionalStatement 
    = PNConditionalStatement PNCondition PNStatement

data PNCondition 
    = PNStatementCondition PNStatement
    | PNTemporalActionCondition PNTest Subject VerbStatus Object Receiver Date
    | PNExpressionCondition PNTest Subject VerbStatus Comparison Subject
    | PNAndCondition [PNCondition]
    | PNOrCondition [PNCondition]

data PNDefinition
    = PNIsDefinition Subject Subject
    | PNEqualsDefinition Subject NumericalExpression
    | PNDefAnd [PNDefinition] 

data PNConditionalDefinition 
    = PNConditionalDefinition PNCondition PNDefinition

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

dateToSubject :: Num -> Month -> Num -> Subject
dateToSubject day month year = SubQuoted (dateSpeToString day month year)

pnConditionalStatement :: PNCondition -> PNStatement -> PNConditionalStatement
pnConditionalStatement cond stmt = PNConditionalStatement cond stmt

pnConditionalDefinition :: PNCondition -> PNDefinition -> PNConditionalDefinition
pnConditionalDefinition cond def = PNConditionalDefinition cond def

contractToPN :: Contract -> ([PNStatement], [PNConditionalStatement], [PNDefinition], [PNConditionalDefinition])
contractToPN (ConEmpty) = ([], [], [], [])
contractToPN (ConComp component) = componentToPN component
contractToPN (ConAnd component contract) = combineComponents (componentToPN component) (contractToPN contract)

-- Helper function to help concatenating lists into one tuple
combineComponents (s1, cs1, d1, cd1) (s2, cs2, d2, cd2) =
    (s1 ++ s2, cs1 ++ cs2, d1 ++ d2, cd1 ++ cd2)

componentToPN :: Component -> ([PNStatement], [PNConditionalStatement], [PNDefinition], [PNConditionalDefinition]) 
componentToPN (ComDef definition) = ([], [], [definitionToPN definition], [])
componentToPN (ComConDef conditionalDefinition) =  ([], [], [], [conditionalDefinitionToPN conditionalDefinition])
componentToPN (ComState statement) = ((statementToPN statement), [], [], [])
componentToPN (ComConState conditionalStatement) = ([], (conditionalStatementToPN), [], []) 

definitionToPN :: Definition -> PNDefinition
definitionToPN (DefSim simpleDefinition) = simpleDefinitionToPN simpleDefinition
definitionToPN (DefAnd simpleDefinition definition) = 
    case definitionToPN definition of
        PNDefAnd ds -> PNDefAnd (simpleDefinitionToPN simpleDefinition : ds)
        d -> PNDefAnd [simpleDefinitionToPN simpleDefinition, d]

conditionalDefinitionToPN :: ConditionalDefinition -> PNConditionalDefinition
conditionalDefinitionToPN (ConDefIf definition condition) = PNConditionalDefinition (conditionToPN condition) (definitionToPN definition)
conditionalDefinitionToPN (ConDefIfThen condition definition) = PNConditionalDefinition (conditionToPN condition) (definitionToPN definition)

statementToPN :: Statement -> [PNStatement]
statementToPN (StateSim simpleStatement) = simpleStatementToPN simpleStatement
statementToPN (StateOr simpleStatement statement) = simpleStatementToPN simpleStatement ++ statementToPN statement
statementToPN (StateAnd simpleStatement statement) = simpleStatementToPN simpleStatement ++ statementToPN statement

conditionalStatementToPN :: ConditionalStatement -> [PNConditionalStatement]
conditionalStatementToPN (ConStateIf statement condition) = xConditionalStatementToPN (conditionToPN condition) (statementToPN statement)
conditionalStatementToPN (ConStateIfThen statement condition) = xConditionalStatementToPN (conditionToPN condition) (statementToPN statement)

-- Looks wrong to me, check when come back
xConditionalStatementToPN :: PNCondition -> [PNStatement] -> [PNConditionalStatement]
xConditionalStatementToPN pnCondition statementList = map (pnConditionalStatement pnCondition) statementList

simpleDefinitionToPN :: SimpleDefinition -> PNDefinition
simpleDefinitionToPN (SimDefIs id subject1 subject2) = PNIsDefinition subject1 subject2
simpleDefinitionToPN (SimDefEq id subject numericalExpression) = PNEqualsDefinition subject numericalExpression
simpleDefinitionToPN (SimDefDate id subject day month year) = PNIsDefinition subject (dateToSubject day month year)

conditionToPN :: Condition -> PNCondition

simpleStatementToPN :: SimpleStatement -> [PNStatement]