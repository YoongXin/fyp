-- File generated by the BNF Converter (bnfc 2.9.5).

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The abstract syntax of language CoLa.

module AbsCoLa where

import Prelude (Integer, String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

data Contract
    = ConEmpty | ConComp Component | ConAnd Component Contract
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Component
    = ComDef Definition
    | ComConDef ConditionalDefinition
    | ComState Statement
    | ComConState ConditionalStatement
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Definition
    = DefSim SimpleDefinition | DefAnd SimpleDefinition Definition
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data SimpleDefinition
    = SimDefIs ID Subject Subject
    | SimDefEq ID Subject NumericalExpression
    | SimDefDate ID Subject Num Month Num
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data NumericalExpression
    = NumExpNum Num
    | NumExpObj NumericalObject
    | NumExpOp NumericalExpression Operator NumericalExpression
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Operator = OpPlus | OpMin | OpMult | OpDiv
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ConditionalDefinition
    = ConDefIf Definition Condition
    | ConDefIfThen Condition Definition
    | ConDefIfElse Definition Condition Definition
    | ConDefIfThenElse Condition Definition Definition
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Statement
    = StateSim SimpleStatement
    | StateOr SimpleStatement Statement
    | StateAnd SimpleStatement Statement
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ConditionalStatement
    = ConStateIf Statement Condition
    | ConStateIfThen Condition Statement
    | ConStateIfElse Statement Condition Statement
    | ConStateIfThenElse Condition Statement Statement
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data SimpleStatement
    = SimStateOne ID Holds Subject ModalVerb Verb Object Receiver Date
    | SimStateTwo ID Holds Subject Date ModalVerb Verb Object Receiver
    | SimStateThree ID Holds Date Subject ModalVerb Verb Object Receiver
    | SimStateFour ID Holds Subject VerbStatus Object Receiver Date
    | SimStateOneNH ID Subject ModalVerb Verb Object Receiver Date
    | SimStateTwoNH ID Subject Date ModalVerb Verb Object Receiver
    | SimStateThreeNH ID Date Subject ModalVerb Verb Object Receiver
    | SimStateFourNH ID Subject VerbStatus Object Receiver Date
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Condition
    = CondiSim SimpleCondition
    | CondiOr SimpleCondition Condition
    | CondiAnd SimpleCondition Condition
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data SimpleCondition
    = SimConOne ID Holds Subject VerbStatus Object Receiver Date
    | SimConTwo ID Holds Subject Date VerbStatus Object Receiver
    | SimConThree ID Holds Date Subject VerbStatus Object Receiver
    | SimConFour ID Holds Subject ModalVerb Verb Object Receiver Date
    | SimConFive ID Holds BooleanExpression
    | SimConOneNH ID Subject VerbStatus Object Receiver Date
    | SimConTwoNH ID Subject Date VerbStatus Object Receiver
    | SimConThreeNH ID Date Subject VerbStatus Object Receiver
    | SimConFourNH ID Subject ModalVerb Verb Object Receiver Date
    | SimConFiveNH ID BooleanExpression
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data BooleanExpression
    = BoolEx Subject VerbStatus Comparison Subject
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ID = IDSim Num | IDRep Num Num
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Holds = HoldYes | HoldNo
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Subject = SubQuoted String | SubUnQuoted Ident
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Verb = VDel | VPay | VCharge | VRefund
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data VerbStatus = VSDel | VSPay | VSCharge | VSRefund
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Comparison = CompareLess | CompareEq Equal | CompareMore More
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Equal = EqOne | EqTwo
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data More = MoreOne | MoreTwo
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ModalVerb = ModalObli Obligation | ModalPermi | ModalForbi
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Obligation = ObliOne | ObliTwo
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Date
    = DateSpe SpecificDate
    | DateAny
    | DateSome Subject
    | DateThe Subject
    | DateQuanSpecific TemporalQuantifier Num Month Num
    | DateQuanSome TemporalQuantifier Subject
    | DateQuanThe TemporalQuantifier Subject
    | DateQuanSomeWO TemporalOffset TemporalQuantifier Subject
    | DateQuanTheWO TemporalOffset TemporalQuantifier Subject
    | DateQuanTempSome TemporalQuantifier TemporalOffset TemporalQuantifier Subject
    | DateQuanTempThe TemporalQuantifier TemporalOffset TemporalQuantifier Subject
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data SpecificDate
    = DateSpeOnThe Num Month Num | DateSpeOn Num Month Num
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data TemporalQuantifier = TempAfter | TempBefore
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data TemporalOffset
    = TempOffDay Num
    | TempOffYear Num
    | TempOffWeek Num
    | TempOffDays Num
    | TempOffYears Num
    | TempOffWeeks Num
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Month
    = MJan
    | MFeb
    | MMar
    | MApr
    | MMay
    | MJun
    | MJul
    | MAug
    | MSep
    | MOct
    | MNov
    | MDec
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Object = ObjNu NumericalObject | ObjNonNu NonNumericalObject
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data NumericalObject
    = NumPound Pounds Num
    | NumDol Dollars Num
    | NumEur Euros Num
    | NumAmount Subject
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Pounds = PoundOne | PoundTwo | PoundThree
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Dollars = DollarOne | DollarTwo | DollarThree
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Euros = EuroOne | EuroTwo
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data NonNumericalObject
    = NonNumCurr Subject
    | NonNumRep Subject
    | NonNumNamed Subject
    | NonNumOther Subject
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Num = NumInt Integer
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Receiver = Rec Subject
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Ident = Ident String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

