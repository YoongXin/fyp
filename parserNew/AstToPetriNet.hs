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

pnConditionalStatement :: PNCondition -> PNStatement -> PNConditionalStatement
pnConditionalStatement cond stmt = PNConditionalStatement cond stmt

pnConditionalDefinition :: PNCondition -> PNDefinition -> PNConditionalDefinition
pnConditionalDefinition cond def = PNConditionalDefinition cond def

contractToPN :: Contract -> ([PNStatement], [PNConditionalStatement], [PNDefinition], [PNConditionalDefinition])
contractToPN (ConEmpty) = ([], [], [], [])

