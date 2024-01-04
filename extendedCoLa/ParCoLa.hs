{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module ParCoLa
  ( happyError
  , myLexer
  , pContract
  , pComponent
  , pDefinition
  , pSimpleDefinition
  , pNumericalExpression
  , pOperator
  , pConditionalDefinition
  , pStatement
  , pConditionalStatement
  , pSimpleStatement
  , pCondition
  , pSimpleCondition
  , pBooleanExpression
  , pID
  , pHolds
  , pSubject
  , pVerb
  , pVerbStatus
  , pComparison
  , pEqual
  , pMore
  , pModalVerb
  , pObligation
  , pDate
  , pSpecificDate
  , pTemporalQuantifier
  , pTemporalOffset
  , pMonth
  , pObject
  , pNumericalObject
  , pPounds
  , pDollars
  , pEuros
  , pNonNumericalObject
  , pNum
  , pReceiver
  ) where

import Prelude

import qualified AbsCoLa
import LexCoLa
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
newtype HappyWrap39 = HappyWrap39 (AbsCoLa.Ident)
happyIn39 :: (AbsCoLa.Ident) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap39 x)
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> HappyWrap39
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
newtype HappyWrap40 = HappyWrap40 (Integer)
happyIn40 :: (Integer) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap40 x)
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> HappyWrap40
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
newtype HappyWrap41 = HappyWrap41 (String)
happyIn41 :: (String) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap41 x)
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> HappyWrap41
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
newtype HappyWrap42 = HappyWrap42 (AbsCoLa.Contract)
happyIn42 :: (AbsCoLa.Contract) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap42 x)
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> HappyWrap42
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
newtype HappyWrap43 = HappyWrap43 (AbsCoLa.Component)
happyIn43 :: (AbsCoLa.Component) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap43 x)
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> HappyWrap43
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
newtype HappyWrap44 = HappyWrap44 (AbsCoLa.Definition)
happyIn44 :: (AbsCoLa.Definition) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap44 x)
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> HappyWrap44
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
newtype HappyWrap45 = HappyWrap45 (AbsCoLa.SimpleDefinition)
happyIn45 :: (AbsCoLa.SimpleDefinition) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap45 x)
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> HappyWrap45
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
newtype HappyWrap46 = HappyWrap46 (AbsCoLa.NumericalExpression)
happyIn46 :: (AbsCoLa.NumericalExpression) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap46 x)
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> HappyWrap46
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
newtype HappyWrap47 = HappyWrap47 (AbsCoLa.Operator)
happyIn47 :: (AbsCoLa.Operator) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap47 x)
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> HappyWrap47
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
newtype HappyWrap48 = HappyWrap48 (AbsCoLa.ConditionalDefinition)
happyIn48 :: (AbsCoLa.ConditionalDefinition) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap48 x)
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> HappyWrap48
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
newtype HappyWrap49 = HappyWrap49 (AbsCoLa.Statement)
happyIn49 :: (AbsCoLa.Statement) -> (HappyAbsSyn )
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap49 x)
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> HappyWrap49
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
newtype HappyWrap50 = HappyWrap50 (AbsCoLa.ConditionalStatement)
happyIn50 :: (AbsCoLa.ConditionalStatement) -> (HappyAbsSyn )
happyIn50 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap50 x)
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> HappyWrap50
happyOut50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut50 #-}
newtype HappyWrap51 = HappyWrap51 (AbsCoLa.SimpleStatement)
happyIn51 :: (AbsCoLa.SimpleStatement) -> (HappyAbsSyn )
happyIn51 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap51 x)
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> HappyWrap51
happyOut51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut51 #-}
newtype HappyWrap52 = HappyWrap52 (AbsCoLa.Condition)
happyIn52 :: (AbsCoLa.Condition) -> (HappyAbsSyn )
happyIn52 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap52 x)
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn ) -> HappyWrap52
happyOut52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut52 #-}
newtype HappyWrap53 = HappyWrap53 (AbsCoLa.SimpleCondition)
happyIn53 :: (AbsCoLa.SimpleCondition) -> (HappyAbsSyn )
happyIn53 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap53 x)
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn ) -> HappyWrap53
happyOut53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut53 #-}
newtype HappyWrap54 = HappyWrap54 (AbsCoLa.BooleanExpression)
happyIn54 :: (AbsCoLa.BooleanExpression) -> (HappyAbsSyn )
happyIn54 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap54 x)
{-# INLINE happyIn54 #-}
happyOut54 :: (HappyAbsSyn ) -> HappyWrap54
happyOut54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut54 #-}
newtype HappyWrap55 = HappyWrap55 (AbsCoLa.ID)
happyIn55 :: (AbsCoLa.ID) -> (HappyAbsSyn )
happyIn55 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap55 x)
{-# INLINE happyIn55 #-}
happyOut55 :: (HappyAbsSyn ) -> HappyWrap55
happyOut55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut55 #-}
newtype HappyWrap56 = HappyWrap56 (AbsCoLa.Holds)
happyIn56 :: (AbsCoLa.Holds) -> (HappyAbsSyn )
happyIn56 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap56 x)
{-# INLINE happyIn56 #-}
happyOut56 :: (HappyAbsSyn ) -> HappyWrap56
happyOut56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut56 #-}
newtype HappyWrap57 = HappyWrap57 (AbsCoLa.Subject)
happyIn57 :: (AbsCoLa.Subject) -> (HappyAbsSyn )
happyIn57 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap57 x)
{-# INLINE happyIn57 #-}
happyOut57 :: (HappyAbsSyn ) -> HappyWrap57
happyOut57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut57 #-}
newtype HappyWrap58 = HappyWrap58 (AbsCoLa.Verb)
happyIn58 :: (AbsCoLa.Verb) -> (HappyAbsSyn )
happyIn58 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap58 x)
{-# INLINE happyIn58 #-}
happyOut58 :: (HappyAbsSyn ) -> HappyWrap58
happyOut58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut58 #-}
newtype HappyWrap59 = HappyWrap59 (AbsCoLa.VerbStatus)
happyIn59 :: (AbsCoLa.VerbStatus) -> (HappyAbsSyn )
happyIn59 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap59 x)
{-# INLINE happyIn59 #-}
happyOut59 :: (HappyAbsSyn ) -> HappyWrap59
happyOut59 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut59 #-}
newtype HappyWrap60 = HappyWrap60 (AbsCoLa.Comparison)
happyIn60 :: (AbsCoLa.Comparison) -> (HappyAbsSyn )
happyIn60 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap60 x)
{-# INLINE happyIn60 #-}
happyOut60 :: (HappyAbsSyn ) -> HappyWrap60
happyOut60 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut60 #-}
newtype HappyWrap61 = HappyWrap61 (AbsCoLa.Equal)
happyIn61 :: (AbsCoLa.Equal) -> (HappyAbsSyn )
happyIn61 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap61 x)
{-# INLINE happyIn61 #-}
happyOut61 :: (HappyAbsSyn ) -> HappyWrap61
happyOut61 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut61 #-}
newtype HappyWrap62 = HappyWrap62 (AbsCoLa.More)
happyIn62 :: (AbsCoLa.More) -> (HappyAbsSyn )
happyIn62 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap62 x)
{-# INLINE happyIn62 #-}
happyOut62 :: (HappyAbsSyn ) -> HappyWrap62
happyOut62 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut62 #-}
newtype HappyWrap63 = HappyWrap63 (AbsCoLa.ModalVerb)
happyIn63 :: (AbsCoLa.ModalVerb) -> (HappyAbsSyn )
happyIn63 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap63 x)
{-# INLINE happyIn63 #-}
happyOut63 :: (HappyAbsSyn ) -> HappyWrap63
happyOut63 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut63 #-}
newtype HappyWrap64 = HappyWrap64 (AbsCoLa.Obligation)
happyIn64 :: (AbsCoLa.Obligation) -> (HappyAbsSyn )
happyIn64 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap64 x)
{-# INLINE happyIn64 #-}
happyOut64 :: (HappyAbsSyn ) -> HappyWrap64
happyOut64 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut64 #-}
newtype HappyWrap65 = HappyWrap65 (AbsCoLa.Date)
happyIn65 :: (AbsCoLa.Date) -> (HappyAbsSyn )
happyIn65 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap65 x)
{-# INLINE happyIn65 #-}
happyOut65 :: (HappyAbsSyn ) -> HappyWrap65
happyOut65 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut65 #-}
newtype HappyWrap66 = HappyWrap66 (AbsCoLa.SpecificDate)
happyIn66 :: (AbsCoLa.SpecificDate) -> (HappyAbsSyn )
happyIn66 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap66 x)
{-# INLINE happyIn66 #-}
happyOut66 :: (HappyAbsSyn ) -> HappyWrap66
happyOut66 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut66 #-}
newtype HappyWrap67 = HappyWrap67 (AbsCoLa.TemporalQuantifier)
happyIn67 :: (AbsCoLa.TemporalQuantifier) -> (HappyAbsSyn )
happyIn67 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap67 x)
{-# INLINE happyIn67 #-}
happyOut67 :: (HappyAbsSyn ) -> HappyWrap67
happyOut67 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut67 #-}
newtype HappyWrap68 = HappyWrap68 (AbsCoLa.TemporalOffset)
happyIn68 :: (AbsCoLa.TemporalOffset) -> (HappyAbsSyn )
happyIn68 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap68 x)
{-# INLINE happyIn68 #-}
happyOut68 :: (HappyAbsSyn ) -> HappyWrap68
happyOut68 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut68 #-}
newtype HappyWrap69 = HappyWrap69 (AbsCoLa.Month)
happyIn69 :: (AbsCoLa.Month) -> (HappyAbsSyn )
happyIn69 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap69 x)
{-# INLINE happyIn69 #-}
happyOut69 :: (HappyAbsSyn ) -> HappyWrap69
happyOut69 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut69 #-}
newtype HappyWrap70 = HappyWrap70 (AbsCoLa.Object)
happyIn70 :: (AbsCoLa.Object) -> (HappyAbsSyn )
happyIn70 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap70 x)
{-# INLINE happyIn70 #-}
happyOut70 :: (HappyAbsSyn ) -> HappyWrap70
happyOut70 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut70 #-}
newtype HappyWrap71 = HappyWrap71 (AbsCoLa.NumericalObject)
happyIn71 :: (AbsCoLa.NumericalObject) -> (HappyAbsSyn )
happyIn71 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap71 x)
{-# INLINE happyIn71 #-}
happyOut71 :: (HappyAbsSyn ) -> HappyWrap71
happyOut71 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut71 #-}
newtype HappyWrap72 = HappyWrap72 (AbsCoLa.Pounds)
happyIn72 :: (AbsCoLa.Pounds) -> (HappyAbsSyn )
happyIn72 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap72 x)
{-# INLINE happyIn72 #-}
happyOut72 :: (HappyAbsSyn ) -> HappyWrap72
happyOut72 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut72 #-}
newtype HappyWrap73 = HappyWrap73 (AbsCoLa.Dollars)
happyIn73 :: (AbsCoLa.Dollars) -> (HappyAbsSyn )
happyIn73 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap73 x)
{-# INLINE happyIn73 #-}
happyOut73 :: (HappyAbsSyn ) -> HappyWrap73
happyOut73 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut73 #-}
newtype HappyWrap74 = HappyWrap74 (AbsCoLa.Euros)
happyIn74 :: (AbsCoLa.Euros) -> (HappyAbsSyn )
happyIn74 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap74 x)
{-# INLINE happyIn74 #-}
happyOut74 :: (HappyAbsSyn ) -> HappyWrap74
happyOut74 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut74 #-}
newtype HappyWrap75 = HappyWrap75 (AbsCoLa.NonNumericalObject)
happyIn75 :: (AbsCoLa.NonNumericalObject) -> (HappyAbsSyn )
happyIn75 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap75 x)
{-# INLINE happyIn75 #-}
happyOut75 :: (HappyAbsSyn ) -> HappyWrap75
happyOut75 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut75 #-}
newtype HappyWrap76 = HappyWrap76 (AbsCoLa.Num)
happyIn76 :: (AbsCoLa.Num) -> (HappyAbsSyn )
happyIn76 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap76 x)
{-# INLINE happyIn76 #-}
happyOut76 :: (HappyAbsSyn ) -> HappyWrap76
happyOut76 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut76 #-}
newtype HappyWrap77 = HappyWrap77 (AbsCoLa.Receiver)
happyIn77 :: (AbsCoLa.Receiver) -> (HappyAbsSyn )
happyIn77 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap77 x)
{-# INLINE happyIn77 #-}
happyOut77 :: (HappyAbsSyn ) -> HappyWrap77
happyOut77 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut77 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\xc4\x02\x00\x01\x21\x00\x00\x08\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x40\x40\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x88\x00\x40\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x88\x00\x10\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\xa5\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x21\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x29\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x20\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x11\xe2\x96\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\xc4\x02\x24\x07\x21\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x62\x01\x80\x80\x10\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x40\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x20\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x31\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\xc0\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x01\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x50\x00\x00\x00\x00\x04\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x01\x20\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x10\x04\x00\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x41\x00\xe0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x20\x20\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x40\x10\x00\x38\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x40\x00\xe0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x01\x00\x00\x23\x42\x6a\x0c\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x88\x05\x00\x02\x42\x00\x00\x10\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x44\x48\x8d\x01\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x40\x00\xe0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x23\x42\x6a\x0c\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4b\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x88\x5b\x82\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8c\x10\xb7\x04\x01\x60\x00\x00\x80\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x14\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x14\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8c\x10\xb7\x04\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x14\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x00\x88\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x88\x05\x48\x0e\x42\xc0\x52\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x08\x00\x14\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x08\x00\x11\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x23\x42\x6a\x0c\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x88\x05\x48\x0e\x42\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x08\x00\x14\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x29\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x0b\x00\x04\x84\x00\x00\x20\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x05\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x88\x90\x1a\x03\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x0a\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x40\x2c\x40\x72\x10\x02\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x00\xa0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x48\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x11\x00\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x21\x6e\x09\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x08\x08\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x00\xa0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x0b\x90\x1c\x84\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x11\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x62\x01\x92\x83\x10\xb0\x14\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x02\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x02\x40\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x40\x2c\x40\x72\x10\x02\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x20\x16\x20\x39\x08\x01\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x88\x05\x48\x0e\x42\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x40\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x20\x16\x20\x39\x08\x01\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x0b\x90\x1c\x84\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\xc4\x02\x24\x07\x21\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x80\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x81\x58\x80\xe4\x20\x04\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x88\x05\x48\x0e\x42\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x08\x00\x14\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x62\x01\x92\x83\x10\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x01\x80\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\xc4\x02\x24\x07\x21\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x80\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x81\x58\x80\xe4\x20\x04\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x02\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x40\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x00\x04\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x00\x08\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x10\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pContract","%start_pComponent","%start_pDefinition","%start_pSimpleDefinition","%start_pNumericalExpression","%start_pOperator","%start_pConditionalDefinition","%start_pStatement","%start_pConditionalStatement","%start_pSimpleStatement","%start_pCondition","%start_pSimpleCondition","%start_pBooleanExpression","%start_pID","%start_pHolds","%start_pSubject","%start_pVerb","%start_pVerbStatus","%start_pComparison","%start_pEqual","%start_pMore","%start_pModalVerb","%start_pObligation","%start_pDate","%start_pSpecificDate","%start_pTemporalQuantifier","%start_pTemporalOffset","%start_pMonth","%start_pObject","%start_pNumericalObject","%start_pPounds","%start_pDollars","%start_pEuros","%start_pNonNumericalObject","%start_pNum","%start_pReceiver","Ident","Integer","String","Contract","Component","Definition","SimpleDefinition","NumericalExpression","Operator","ConditionalDefinition","Statement","ConditionalStatement","SimpleStatement","Condition","SimpleCondition","BooleanExpression","ID","Holds","Subject","Verb","VerbStatus","Comparison","Equal","More","ModalVerb","Obligation","Date","SpecificDate","TemporalQuantifier","TemporalOffset","Month","Object","NumericalObject","Pounds","Dollars","Euros","NonNumericalObject","Num","Receiver","'('","')'","'AMOUNT'","'AND'","'ANYDATE'","'April'","'August'","'C-AND'","'DIVIDE'","'DOLLARS'","'December'","'ELSE'","'EQUALS'","'EUR'","'EUROS'","'February'","'GBP'","'IF'","'IS'","'January'","'July'","'June'","'MINUS'","'March'","'May'","'NAMEDOBJECT'","'November'","'OR'","'OTHEROBJECT'","'October'","'PLUS'","'POUNDS'","'REPORT'","'SOMECURRENCY'","'SOMEDATE'","'September'","'THEDATE'","'THEN'","'TIMES'","'USD'","'['","']'","'after'","'before'","'buck'","'case'","'charge'","'charged'","'day'","'days'","'deliver'","'delivered'","'equal'","'equals'","'forbidden'","'greater'","'is'","'it'","'less'","'may'","'more'","'must'","'not'","'on'","'paid'","'pay'","'quid'","'refund'","'refunded'","'shall'","'than'","'that'","'the'","'to'","'week'","'weeks'","'year'","'years'","L_Ident","L_integ","L_quoted","%eof"]
        bit_start = st Prelude.* 159
        bit_end = (st Prelude.+ 1) Prelude.* 159
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..158]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x53\x00\x53\x00\xea\xff\xea\xff\xfe\xff\x0f\x01\x55\x00\xea\xff\x65\x00\xea\xff\xea\xff\xea\xff\x15\x00\xea\xff\xdc\xff\x15\x00\x48\x01\xd7\xff\x5b\x02\x43\x01\xf4\xff\xf4\x00\xd9\xff\x18\x00\xe8\xff\x5d\x01\xed\xff\xbc\x01\x88\x00\x52\x00\xf5\xff\x97\x00\xa2\x01\x0a\x02\xed\xff\xfb\xff\x0c\x00\x00\x00\x35\x00\x15\x00\x00\x00\x35\x00\x00\x00\x35\x00\x15\x00\x15\x00\x15\x00\x15\x00\x35\x00\x00\x00\x00\x00\x35\x00\x00\x00\x00\x00\x00\x00\x35\x00\x00\x00\x00\x00\x00\x00\x35\x00\x41\x00\x41\x00\x41\x00\x15\x00\x4b\x00\x00\x00\x00\x00\x4b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4b\x00\x99\x01\x4b\x00\x00\x00\x00\x00\x4b\x00\xfa\x00\x4b\x00\x00\x00\xeb\xff\xbf\x01\xfd\xff\x4b\x00\x00\x00\x00\x00\x4b\x00\x00\x00\x70\x00\x00\x00\x7d\x00\x76\x00\xa5\x00\x9d\x00\xb5\x00\x00\x00\xb3\x00\x00\x00\x00\x00\xd3\x00\xc0\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x00\x00\xc0\x00\x1e\x01\xe9\x00\x17\x01\xf2\x00\xd7\xff\xf2\x00\x78\x00\xf2\x00\x01\x00\xf2\x00\x78\x00\x6b\x01\x32\x01\x0e\x00\x68\x01\x42\x01\x96\x01\xa0\x01\x73\x01\x15\x00\x9f\x01\x89\x01\x00\x00\x00\x00\x00\x00\x00\x00\xfa\xff\x00\x00\x00\x00\x89\x01\x89\x01\x89\x01\xcf\x01\x00\x00\xd6\x01\x00\x00\x78\x00\xd7\x01\x9e\x01\x04\x02\x67\x00\xde\x01\xef\xff\x44\x00\x15\x00\xe6\x01\xe6\x01\xfe\xff\xe1\x01\xed\x00\xea\x01\xf2\x01\xf7\x01\xf7\x01\x9a\x00\xf7\x01\xf7\x01\x00\x00\xef\xff\x9a\x00\x15\x00\x5b\x02\x03\x00\x14\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe5\x01\xbc\x01\x00\x00\x15\x00\x15\x00\xe3\x01\x50\x00\xbf\x01\x1e\x00\xe0\x00\xe0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe3\x01\x14\x01\x11\x01\x11\x01\xbc\x01\x00\x00\x00\x00\xe3\x01\x00\x00\xef\x01\x0e\x02\xf5\x01\x00\x00\x11\x01\xd7\xff\x7e\x00\x74\x01\xa8\x01\x00\x00\x9a\x00\x11\x01\x00\x00\x00\x00\x88\x00\xaa\x01\xf4\x00\x00\x00\x00\x00\x44\x02\x00\x00\xfe\xff\x03\x01\x44\x02\x0f\x01\x67\x02\x68\x02\xf4\x00\x9a\x00\x11\x01\x54\x02\x00\x00\x78\x02\x86\x02\xf4\x00\x88\x00\xaa\x01\xf4\x00\xaa\x01\x6b\x02\x6b\x02\x00\x00\xbc\x01\x0f\x01\xaa\x01\x88\x00\xfe\x01\xae\x01\x7e\x00\xb0\x01\xc1\x01\x88\x00\x88\x00\xfe\x01\x88\x00\x00\x00\x9c\x02\x4f\x02\x73\x02\x00\x00\x56\x02\x00\x00\x00\x00\x11\x01\x11\x01\x00\x00\x00\x00\x00\x00\x00\x00\x61\x02\x00\x00\x8d\x02\x65\x02\x18\x00\x65\x02\x65\x02\x88\x00\x88\x00\x65\x02\x88\x00\x18\x00\x65\x02\x88\x00\xbe\x02\x00\x00\x00\x00\x88\x00\xc7\x01\x88\x00\xc8\x02\xc7\x01\xed\x02\xed\x02\x00\x00\x00\x00\x88\x00\x18\x00\xd0\x02\x88\x00\xd0\x02\x00\x00\xd0\x02\x18\x00\x00\x00\xd0\x02\x18\x00\xd0\x02\xd0\x02\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd0\x02\x18\x00\x00\x00\xd0\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x5e\x02\x75\x02\x04\x00\x6b\x00\x5e\x01\x16\x03\x3e\x02\x7b\x01\x90\x02\x0b\x01\x38\x01\xac\x00\xc4\x01\x12\x03\x17\x03\xd2\x00\x18\x03\x15\x03\xb8\x01\x14\x03\x19\x03\x4d\x02\x1a\x03\x92\x00\x11\x03\x1b\x03\x39\x00\x10\x03\x92\x02\x1e\x02\x1c\x03\x0d\x03\x0a\x03\x1d\x03\x22\x00\x1e\x03\x00\x00\x00\x00\x00\x00\xd7\x00\x00\x00\x00\x00\x00\x00\x00\x00\x61\x01\xa9\x01\xfb\x01\xfc\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x26\x00\x2f\x00\x34\x00\xff\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5e\x00\x00\x00\x00\x00\x51\x00\x1f\x03\x5e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x61\x00\x00\x00\x20\x03\x00\x00\xdc\x00\x00\x00\x00\x00\x00\x00\xe1\x00\x00\x00\x00\x00\x00\x00\xac\x01\x00\x00\x00\x00\x00\x00\x00\x00\x03\x02\xff\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x29\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x01\x03\x03\x00\x00\x00\x00\x6c\x02\x00\x00\x0e\x01\x24\x01\x08\x02\x07\x03\x0b\x03\x6d\x01\x00\x00\x00\x00\xbd\x00\x00\x00\x7c\x02\x93\x02\x24\x01\x0f\x03\x13\x03\x00\x00\x07\x01\x33\x01\x10\x02\xe2\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x21\x03\x00\x00\x14\x02\x17\x02\x99\x00\x00\x00\x22\x03\x24\x03\x1b\x02\x1c\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9b\x00\x00\x00\x1f\x02\x23\x02\x25\x03\x00\x00\x00\x00\xb0\x00\x00\x00\x00\x00\x00\x00\xcf\x00\x00\x00\x28\x02\x26\x03\x35\x02\x23\x03\x28\x03\x00\x00\x41\x01\x30\x02\x00\x00\x00\x00\x99\x02\x2d\x03\x53\x02\x00\x00\x00\x00\x98\x02\x00\x00\x75\x01\x48\x00\x4a\x02\x2a\x03\x00\x00\x00\x00\x95\x02\x50\x01\x34\x02\x7d\x02\x00\x00\x00\x00\x00\x00\x0c\x03\x9f\x02\x32\x03\x0e\x03\x33\x03\x9b\x02\x85\x02\x00\x00\x2b\x03\x30\x03\x34\x03\xa5\x02\x27\x03\x36\x03\x3c\x02\x35\x03\x37\x03\xab\x02\xb1\x02\x2c\x03\xb7\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd0\x00\x00\x00\x00\x00\x37\x02\x3b\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2e\x03\xcb\x00\x2f\x03\x31\x03\xbd\x02\xc3\x02\x38\x03\xc9\x02\xf0\x00\x39\x03\xcf\x02\x10\x01\x00\x00\x00\x00\xd5\x02\x3b\x03\xdb\x02\x3a\x03\x3c\x03\xa0\x02\x89\x02\x00\x00\x00\x00\xe1\x02\x55\x01\x3d\x03\xe7\x02\x3e\x03\x00\x00\x3f\x03\x81\x01\x00\x00\x40\x03\x85\x01\x41\x03\x42\x03\x00\x00\x92\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x98\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x43\x03\xa4\x01\x00\x00\x44\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xd8\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdb\xff\x00\x00\x00\x00\x57\xff\x00\x00\xda\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5d\xff\x5c\xff\x00\x00\x5f\xff\x60\xff\x5e\xff\x00\x00\x63\xff\x62\xff\x61\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x69\xff\x68\xff\x00\x00\x72\xff\x6e\xff\x6a\xff\x74\xff\x75\xff\x6f\xff\x70\xff\x73\xff\x71\xff\x6b\xff\x6c\xff\x6d\xff\x00\x00\x00\x00\x00\x00\x7d\xff\x7c\xff\x00\x00\x00\x00\x00\x00\x8a\xff\x00\x00\x00\x00\x00\x00\x00\x00\x8b\xff\x8c\xff\x00\x00\x8f\xff\x00\x00\x8e\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x93\xff\x00\x00\x95\xff\x94\xff\x00\x00\x00\x00\x98\xff\x9a\xff\x99\xff\x97\xff\x00\x00\x9c\xff\x9e\xff\x9d\xff\x9b\xff\x9f\xff\xa0\xff\x00\x00\xd9\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb2\xff\x00\x00\x00\x00\x00\x00\x00\x00\xc1\xff\x00\x00\x00\x00\x00\x00\xd1\xff\x00\x00\x00\x00\x00\x00\x00\x00\xc6\xff\xc8\xff\xc9\xff\xc7\xff\x00\x00\xcb\xff\xcc\xff\x00\x00\x00\x00\x00\x00\xd5\xff\xd4\xff\xd3\xff\xd2\xff\x00\x00\x00\x00\x00\x00\xd7\xff\xd8\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa6\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x96\xff\x92\xff\x91\xff\x90\xff\x00\x00\x00\x00\x89\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7b\xff\x78\xff\x79\xff\x76\xff\x7a\xff\x77\xff\x64\xff\x65\xff\x66\xff\x67\xff\x5b\xff\x5a\xff\x58\xff\x59\xff\x56\xff\x84\xff\x85\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x87\xff\x88\xff\x00\x00\x8d\xff\x00\x00\x00\x00\x00\x00\xa4\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xab\xff\x00\x00\x00\x00\xb1\xff\xb0\xff\x00\x00\x00\x00\x00\x00\xc0\xff\xbf\xff\x00\x00\xd0\xff\x00\x00\x00\x00\x00\x00\xca\xff\xc5\xff\xbe\xff\x00\x00\x00\x00\x00\x00\x00\x00\xd6\xff\xc4\xff\xbd\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcf\xff\x00\x00\xce\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa5\xff\x00\x00\x00\x00\x00\x00\x7e\xff\x00\x00\x82\xff\x83\xff\x00\x00\x00\x00\x86\xff\x80\xff\x81\xff\x7f\xff\x00\x00\xa2\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc3\xff\xbc\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc2\xff\xbb\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcd\xff\x00\x00\x00\x00\xb3\xff\x00\x00\x00\x00\x00\x00\x00\x00\xa9\xff\x00\x00\xaa\xff\xa8\xff\xa3\xff\xa1\xff\xa7\xff\xae\xff\x00\x00\xaf\xff\xad\xff\xb6\xff\xb5\xff\xb4\xff\x00\x00\x00\x00\xb7\xff\x00\x00\xb8\xff\xba\xff\xb9\xff\xac\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x03\x00\x05\x00\x09\x00\x01\x00\x04\x00\x11\x00\x30\x00\x0a\x00\x05\x00\x06\x00\x34\x00\x0e\x00\x0f\x00\x23\x00\x11\x00\x25\x00\x17\x00\x04\x00\x29\x00\x10\x00\x20\x00\x3a\x00\x3e\x00\x41\x00\x1f\x00\x2b\x00\x2c\x00\x45\x00\x1c\x00\x20\x00\x46\x00\x23\x00\x27\x00\x25\x00\x01\x00\x06\x00\x07\x00\x28\x00\x01\x00\x40\x00\x0b\x00\x1c\x00\x2d\x00\x38\x00\x2a\x00\x10\x00\x40\x00\x01\x00\x3d\x00\x14\x00\x15\x00\x16\x00\x01\x00\x18\x00\x19\x00\x43\x00\x1b\x00\x01\x00\x50\x00\x1e\x00\x50\x00\x4f\x00\x50\x00\x51\x00\x43\x00\x24\x00\x2b\x00\x2c\x00\x4a\x00\x49\x00\x25\x00\x00\x00\x01\x00\x02\x00\x25\x00\x52\x00\x50\x00\x50\x00\x31\x00\x32\x00\x0d\x00\x01\x00\x3f\x00\x25\x00\x03\x00\x1d\x00\x13\x00\x40\x00\x25\x00\x12\x00\x4f\x00\x0a\x00\x49\x00\x25\x00\x01\x00\x0e\x00\x0f\x00\x01\x00\x11\x00\x4f\x00\x12\x00\x51\x00\x12\x00\x50\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x25\x00\x1d\x00\x2b\x00\x2c\x00\x06\x00\x20\x00\x23\x00\x30\x00\x25\x00\x25\x00\x12\x00\x34\x00\x12\x00\x28\x00\x10\x00\x29\x00\x39\x00\x29\x00\x2d\x00\x3c\x00\x03\x00\x3e\x00\x25\x00\x40\x00\x41\x00\x25\x00\x52\x00\x0a\x00\x45\x00\x46\x00\x03\x00\x0e\x00\x0f\x00\x29\x00\x11\x00\x29\x00\x50\x00\x0a\x00\x01\x00\x50\x00\x43\x00\x0e\x00\x0f\x00\x1a\x00\x11\x00\x01\x00\x1d\x00\x01\x00\x52\x00\x20\x00\x21\x00\x22\x00\x0a\x00\x1a\x00\x2b\x00\x2c\x00\x1d\x00\x28\x00\x37\x00\x20\x00\x21\x00\x22\x00\x2d\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x28\x00\x01\x00\x3a\x00\x35\x00\x36\x00\x2d\x00\x38\x00\x25\x00\x40\x00\x3b\x00\x0e\x00\x3d\x00\x10\x00\x47\x00\x25\x00\x28\x00\x25\x00\x43\x00\x05\x00\x06\x00\x2d\x00\x2b\x00\x2c\x00\x4f\x00\x50\x00\x51\x00\x30\x00\x43\x00\x01\x00\x10\x00\x34\x00\x52\x00\x01\x00\x01\x00\x00\x00\x39\x00\x02\x00\x25\x00\x3c\x00\x00\x00\x3e\x00\x02\x00\x40\x00\x41\x00\x00\x00\x01\x00\x02\x00\x45\x00\x46\x00\x00\x00\x01\x00\x02\x00\x12\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x12\x00\x50\x00\x0f\x00\x47\x00\x11\x00\x12\x00\x52\x00\x25\x00\x01\x00\x11\x00\x12\x00\x25\x00\x25\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x0d\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x4a\x00\x13\x00\x25\x00\x00\x00\x01\x00\x02\x00\x52\x00\x25\x00\x00\x00\x01\x00\x02\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x00\x00\x01\x00\x02\x00\x01\x00\x52\x00\x11\x00\x12\x00\x25\x00\x0f\x00\x0c\x00\x09\x00\x12\x00\x47\x00\x10\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x12\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x01\x00\x17\x00\x25\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x25\x00\x39\x00\x1f\x00\x4f\x00\x3c\x00\x51\x00\x3e\x00\x25\x00\x01\x00\x25\x00\x27\x00\x23\x00\x14\x00\x25\x00\x46\x00\x52\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x01\x00\x49\x00\x52\x00\x0d\x00\x0e\x00\x14\x00\x10\x00\x25\x00\x50\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x01\x00\x4f\x00\x50\x00\x51\x00\x14\x00\x01\x00\x39\x00\x25\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x01\x00\x4f\x00\x00\x00\x51\x00\x02\x00\x14\x00\x07\x00\x25\x00\x50\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x01\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x12\x00\x07\x00\x25\x00\x01\x00\x2f\x00\x35\x00\x36\x00\x25\x00\x33\x00\x07\x00\x12\x00\x20\x00\x21\x00\x22\x00\x23\x00\x01\x00\x25\x00\x52\x00\x0a\x00\x01\x00\x0c\x00\x2b\x00\x2c\x00\x42\x00\x10\x00\x44\x00\x20\x00\x21\x00\x22\x00\x23\x00\x29\x00\x25\x00\x01\x00\x52\x00\x20\x00\x21\x00\x22\x00\x23\x00\x01\x00\x25\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x2f\x00\x04\x00\x01\x00\x25\x00\x33\x00\x12\x00\x00\x00\x25\x00\x02\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x0e\x00\x0f\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x42\x00\x25\x00\x44\x00\x0d\x00\x0e\x00\x12\x00\x10\x00\x25\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x06\x00\x07\x00\x00\x00\x52\x00\x02\x00\x0b\x00\x29\x00\x25\x00\x31\x00\x32\x00\x10\x00\x15\x00\x16\x00\x17\x00\x14\x00\x15\x00\x16\x00\x0f\x00\x18\x00\x19\x00\x12\x00\x1b\x00\x30\x00\x2f\x00\x1e\x00\x52\x00\x34\x00\x33\x00\x30\x00\x2f\x00\x24\x00\x12\x00\x34\x00\x33\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x12\x00\x41\x00\x2b\x00\x2c\x00\x42\x00\x45\x00\x44\x00\x41\x00\x52\x00\x30\x00\x42\x00\x45\x00\x44\x00\x34\x00\x2f\x00\x15\x00\x16\x00\x17\x00\x33\x00\x00\x00\x00\x00\x02\x00\x02\x00\x00\x00\x29\x00\x02\x00\x41\x00\x00\x00\x26\x00\x02\x00\x45\x00\x26\x00\x00\x00\x42\x00\x02\x00\x44\x00\x08\x00\x12\x00\x12\x00\x29\x00\x00\x00\x12\x00\x02\x00\x29\x00\x00\x00\x12\x00\x02\x00\x00\x00\x26\x00\x02\x00\x12\x00\x00\x00\x00\x00\x02\x00\x02\x00\x00\x00\x29\x00\x02\x00\x12\x00\x00\x00\x1a\x00\x02\x00\x12\x00\x1d\x00\x00\x00\x12\x00\x02\x00\x21\x00\x22\x00\x12\x00\x12\x00\x4a\x00\x00\x00\x12\x00\x02\x00\x50\x00\x00\x00\x12\x00\x02\x00\x00\x00\x49\x00\x02\x00\x12\x00\x00\x00\x2e\x00\x02\x00\x20\x00\x21\x00\x22\x00\x23\x00\x12\x00\x05\x00\x06\x00\x50\x00\x12\x00\x09\x00\x4a\x00\x12\x00\x15\x00\x16\x00\x17\x00\x12\x00\x10\x00\x05\x00\x06\x00\x15\x00\x16\x00\x17\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x10\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x03\x00\x04\x00\x05\x00\x06\x00\x18\x00\x19\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x18\x00\x19\x00\x29\x00\x10\x00\x03\x00\x04\x00\x05\x00\x06\x00\x0c\x00\x0c\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x04\x00\x05\x00\x06\x00\x10\x00\x29\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x05\x00\x06\x00\x0c\x00\x10\x00\x0a\x00\x0a\x00\x0c\x00\x0c\x00\x05\x00\x06\x00\x10\x00\x10\x00\x05\x00\x06\x00\x35\x00\x36\x00\x0c\x00\x38\x00\x29\x00\x10\x00\x3b\x00\x48\x00\x3d\x00\x10\x00\x0a\x00\x0b\x00\x0c\x00\x0a\x00\x02\x00\x0c\x00\x10\x00\x2e\x00\x0a\x00\x10\x00\x0c\x00\x0a\x00\x50\x00\x0c\x00\x10\x00\x48\x00\x0a\x00\x10\x00\x0c\x00\x18\x00\x19\x00\x4a\x00\x10\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x2a\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x0d\x00\x0e\x00\x50\x00\x10\x00\x0d\x00\x0e\x00\x4a\x00\x10\x00\x0d\x00\x0e\x00\x29\x00\x10\x00\x0d\x00\x0e\x00\x4a\x00\x10\x00\x0d\x00\x0e\x00\x08\x00\x10\x00\x0d\x00\x0e\x00\x10\x00\x10\x00\x18\x00\x19\x00\x18\x00\x19\x00\x11\x00\x14\x00\x16\x00\x13\x00\x1b\x00\x23\x00\x1e\x00\x22\x00\x17\x00\x08\x00\x08\x00\x19\x00\x14\x00\xff\xff\x13\x00\x1c\x00\x08\x00\xff\xff\x14\x00\x1c\x00\x14\x00\x21\x00\x1c\x00\x1e\x00\x13\x00\x24\x00\x1e\x00\x1e\x00\x26\x00\x13\x00\x13\x00\x13\x00\x13\x00\x1e\x00\x14\x00\x14\x00\xff\xff\x26\x00\x13\x00\x13\x00\xff\xff\xff\xff\x26\x00\xff\xff\x26\x00\x26\x00\xff\xff\x26\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x26\x00\x26\x00\x26\x00\xff\xff\xff\xff\x26\x00\x26\x00\x26\x00\x26\x00\x26\x00\x26\x00\x26\x00\x26\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x40\x00\xc3\x00\x93\x00\xe8\x00\xb4\x00\x39\x00\x6f\x00\x35\x00\x9a\x00\x8d\x00\x70\x00\x32\x00\x33\x00\xca\x00\x39\x00\xcb\x00\x94\x00\xb1\x00\x7f\x00\x8f\x00\x3a\x00\x7d\x00\x5e\x00\x71\x00\x95\x00\x54\x00\x55\x00\x72\x00\xb5\x00\x3a\x00\x5f\x00\xc4\x00\x96\x00\xc5\x00\x28\x00\x45\x00\x46\x00\x36\x00\x28\x00\x57\x00\x47\x00\xb2\x00\x37\x00\x65\x00\xe9\x00\x48\x00\x5c\x00\x28\x00\x66\x00\x49\x00\x4a\x00\x4b\x00\x28\x00\x4c\x00\x4d\x00\x3b\x00\x4e\x00\x28\x00\x2b\x00\x4f\x00\x2b\x00\x26\x00\x2b\x00\x7b\x00\x3b\x00\x50\x00\x54\x00\x55\x00\x28\x00\xc6\x00\x29\x00\x77\x00\x28\x00\x78\x00\xd4\x00\xff\xff\x2b\x00\x2b\x00\xcc\x00\xcd\x00\xfb\x00\x28\x00\xe6\x00\xd3\x00\x40\x00\x50\x00\xfc\x00\x5c\x00\xd2\x00\x0e\x01\x26\x00\x35\x00\xe7\x00\x51\x00\x28\x00\x32\x00\x33\x00\x28\x00\x39\x00\x26\x00\xa2\x00\x7b\x00\x91\x00\x2b\x00\xce\x00\xcf\x00\xd0\x00\xd1\x00\x0f\x01\xc7\x00\x54\x00\x55\x00\x99\x00\x3a\x00\xdf\x00\x6f\x00\xe0\x00\xc8\x00\x8b\x00\x70\x00\xa2\x00\x36\x00\x8f\x00\x7f\x00\x62\x00\x7f\x00\x37\x00\x63\x00\x40\x00\x5e\x00\xc1\x00\x5c\x00\x71\x00\xba\x00\xff\xff\x35\x00\x72\x00\x5f\x00\x40\x00\x32\x00\x33\x00\x7f\x00\x39\x00\x7f\x00\x2b\x00\x35\x00\x28\x00\x2b\x00\x3b\x00\x32\x00\x33\x00\x2d\x00\x39\x00\x28\x00\x2e\x00\x28\x00\xff\xff\x3a\x00\x2f\x00\x30\x00\x35\x00\x2d\x00\x54\x00\x55\x00\x2e\x00\x36\x00\xc1\x00\x3a\x00\x2f\x00\x30\x00\x37\x00\x57\x00\x58\x00\x59\x00\x5a\x00\x36\x00\x28\x00\x7d\x00\x68\x00\x69\x00\x37\x00\x65\x00\x51\x00\x5c\x00\x6d\x00\x81\x00\x66\x00\x82\x00\xc0\x00\xe0\x00\x36\x00\x26\x01\x3b\x00\xf9\x00\x8d\x00\x37\x00\x54\x00\x55\x00\x26\x00\x2b\x00\x7b\x00\x6f\x00\x3b\x00\x28\x00\x8f\x00\x70\x00\xff\xff\x28\x00\x28\x00\x77\x00\x62\x00\x78\x00\x20\x01\x63\x00\x77\x00\x5e\x00\x78\x00\x5c\x00\x71\x00\x77\x00\x28\x00\x78\x00\x72\x00\x5f\x00\x77\x00\x28\x00\x78\x00\x79\x00\x53\x01\x58\x00\x59\x00\x5a\x00\xd9\x00\x2b\x00\xb5\x00\xbf\x00\xb6\x00\xb7\x00\xff\xff\x51\x00\x28\x00\xa6\x00\xb2\x00\x1d\x01\x29\x01\xb8\x00\x58\x00\x59\x00\x5a\x00\xfb\x00\xa8\x00\x58\x00\x59\x00\x5a\x00\xbe\x00\xfc\x00\x51\x00\x77\x00\x28\x00\x78\x00\xff\xff\x51\x00\x77\x00\x28\x00\x78\x00\x4c\x01\x58\x00\x59\x00\x5a\x00\x77\x00\x28\x00\x78\x00\x28\x00\xff\xff\xa6\x00\xa7\x00\x51\x00\xee\x00\x85\x00\x93\x00\xef\x00\xbd\x00\x86\x00\xa8\x00\x58\x00\x59\x00\x5a\x00\x01\x01\xf0\x00\x58\x00\x59\x00\x5a\x00\x28\x00\x94\x00\x51\x00\x02\x01\x58\x00\x59\x00\x5a\x00\x51\x00\x62\x00\x95\x00\x26\x00\x63\x00\x7b\x00\x5e\x00\x51\x00\x28\x00\x49\x01\x96\x00\x25\x01\xf3\x00\x26\x01\x5f\x00\xff\xff\xf4\x00\x60\x00\xf5\x00\x58\x00\x59\x00\x5a\x00\x28\x00\xc6\x00\xff\xff\x83\x00\x84\x00\xeb\x00\x82\x00\x51\x00\x2b\x00\xec\x00\x60\x00\xed\x00\x58\x00\x59\x00\x5a\x00\x28\x00\x26\x00\x2b\x00\x7b\x00\x15\x01\x28\x00\xbc\x00\x51\x00\x16\x01\x60\x00\x17\x01\x58\x00\x59\x00\x5a\x00\x28\x00\x26\x00\x77\x00\x7b\x00\x78\x00\x08\x01\x96\x00\x51\x00\x2b\x00\x09\x01\x60\x00\x0a\x01\x58\x00\x59\x00\x5a\x00\x28\x00\x61\x01\x58\x00\x59\x00\x5a\x00\xd8\x00\xfd\x00\x51\x00\x28\x00\x74\x00\x68\x00\x69\x00\x51\x00\x75\x00\x10\x01\xaa\x00\x97\x00\x3c\x00\x3d\x00\x3e\x00\x28\x00\x98\x00\xff\xff\x8b\x00\x28\x00\x89\x00\x54\x00\x55\x00\x76\x00\x86\x00\x77\x00\x97\x00\x3c\x00\x3d\x00\x3e\x00\x7f\x00\x98\x00\x28\x00\xff\xff\x97\x00\x3c\x00\x3d\x00\x3e\x00\x28\x00\x98\x00\x5c\x01\x58\x00\x59\x00\x5a\x00\x5a\x01\x58\x00\x59\x00\x5a\x00\x74\x00\xaf\x00\x28\x00\x51\x00\x75\x00\xab\x00\x77\x00\x51\x00\x78\x00\x57\x01\x58\x00\x59\x00\x5a\x00\x32\x00\x33\x00\x66\x01\x58\x00\x59\x00\x5a\x00\x76\x00\x51\x00\x77\x00\xaf\x00\x84\x00\xd7\x00\x82\x00\x51\x00\x64\x01\x58\x00\x59\x00\x5a\x00\x45\x00\x46\x00\x77\x00\xff\xff\x78\x00\x47\x00\x7f\x00\x51\x00\xcc\x00\xcd\x00\x48\x00\x69\x00\x6a\x00\x6b\x00\x49\x00\x4a\x00\x4b\x00\x7f\x00\x4c\x00\x4d\x00\x80\x00\x4e\x00\x6f\x00\x74\x00\x4f\x00\xff\xff\x70\x00\x75\x00\x6f\x00\x74\x00\x50\x00\xab\x00\x70\x00\x75\x00\xce\x00\xcf\x00\xd0\x00\xd1\x00\xaa\x00\x71\x00\x54\x00\x55\x00\x76\x00\x72\x00\x77\x00\x71\x00\xff\xff\x6f\x00\x76\x00\x72\x00\x77\x00\x70\x00\x74\x00\xe9\x00\x6a\x00\x6b\x00\x75\x00\x77\x00\x77\x00\x78\x00\x78\x00\x77\x00\x7f\x00\x78\x00\x71\x00\x77\x00\x04\x01\x78\x00\x72\x00\xfd\x00\x77\x00\x76\x00\x78\x00\x77\x00\xa5\x00\xd6\x00\xd5\x00\x7f\x00\x77\x00\xd1\x00\x78\x00\x7f\x00\x77\x00\xad\x00\x78\x00\x77\x00\xf9\x00\x78\x00\x00\x01\x77\x00\x77\x00\x78\x00\x78\x00\x77\x00\x7f\x00\x78\x00\xea\x00\x77\x00\x2d\x00\x78\x00\xe2\x00\x2e\x00\x77\x00\xe1\x00\x78\x00\x2f\x00\x30\x00\xdb\x00\xda\x00\xe5\x00\x77\x00\x23\x01\x78\x00\x2b\x00\x77\x00\x22\x01\x78\x00\x77\x00\x20\x01\x78\x00\x1c\x01\x77\x00\x1f\x01\x78\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x14\x01\x8c\x00\x8d\x00\x2b\x00\x07\x01\x8e\x00\x28\x00\x28\x01\xe9\x00\x6a\x00\x6b\x00\x27\x01\x8f\x00\x05\x01\x8d\x00\xe9\x00\x6a\x00\x6b\x00\x1a\x01\x41\x00\x3c\x00\x3d\x00\x3e\x00\x42\x00\x8f\x00\x33\x01\x41\x00\x3c\x00\x3d\x00\x3e\x00\x42\x00\xa2\x00\xa3\x00\x9c\x00\x8d\x00\x5f\x00\x60\x00\x9d\x00\x9e\x00\x9f\x00\x89\x00\x11\x01\x60\x00\x7f\x00\xa0\x00\x04\x01\xa3\x00\x9c\x00\x8d\x00\x0e\x01\x0d\x01\x9d\x00\x9e\x00\x9f\x00\x89\x00\x9b\x00\x9c\x00\x8d\x00\xa0\x00\x7f\x00\x9d\x00\x9e\x00\x9f\x00\x89\x00\x05\x01\x8d\x00\x42\x01\xa0\x00\xf7\x00\x06\x01\x89\x00\x89\x00\x39\x01\x8d\x00\x86\x00\xa0\x00\x42\x01\x8d\x00\x68\x00\x69\x00\x41\x01\x65\x00\x7f\x00\x8f\x00\x6d\x00\x2c\x01\x66\x00\x8f\x00\x87\x00\x88\x00\x89\x00\xf6\x00\x2d\x01\x89\x00\x86\x00\x2b\x01\x06\x01\x86\x00\x89\x00\x3a\x01\x2b\x00\x89\x00\x86\x00\x57\x01\x43\x01\x86\x00\x89\x00\x0b\x01\x60\x00\x28\x00\x86\x00\x40\x00\x41\x00\x3c\x00\x3d\x00\x3e\x00\x42\x00\x56\x01\x13\x01\x41\x00\x3c\x00\x3d\x00\x3e\x00\x42\x00\x3e\x01\x41\x00\x3c\x00\x3d\x00\x3e\x00\x42\x00\x36\x01\x41\x00\x3c\x00\x3d\x00\x3e\x00\x42\x00\x30\x01\x41\x00\x3c\x00\x3d\x00\x3e\x00\x42\x00\x2f\x01\x41\x00\x3c\x00\x3d\x00\x3e\x00\x42\x00\x2d\x01\x41\x00\x3c\x00\x3d\x00\x3e\x00\x42\x00\x50\x01\x41\x00\x3c\x00\x3d\x00\x3e\x00\x42\x00\x4f\x01\x41\x00\x3c\x00\x3d\x00\x3e\x00\x42\x00\x4d\x01\x41\x00\x3c\x00\x3d\x00\x3e\x00\x42\x00\x4a\x01\x41\x00\x3c\x00\x3d\x00\x3e\x00\x42\x00\x48\x01\x41\x00\x3c\x00\x3d\x00\x3e\x00\x42\x00\x46\x01\x41\x00\x3c\x00\x3d\x00\x3e\x00\x42\x00\x62\x01\x41\x00\x3c\x00\x3d\x00\x3e\x00\x42\x00\x5f\x01\x41\x00\x3c\x00\x3d\x00\x3e\x00\x42\x00\xac\x00\x84\x00\x2b\x00\x82\x00\xa5\x00\x84\x00\x28\x00\x82\x00\xff\x00\x84\x00\x7f\x00\x82\x00\xfe\x00\x84\x00\x28\x00\x82\x00\xf2\x00\x84\x00\x91\x00\x82\x00\xf1\x00\x84\x00\x7d\x00\x82\x00\x3f\x01\x60\x00\x3c\x01\x60\x00\x7b\x00\x6d\x00\x66\x00\x72\x00\x55\x00\x30\x00\x43\x00\x33\x00\x63\x00\xab\x00\xab\x00\x5c\x00\xb9\x00\x00\x00\x19\x01\x52\x00\xab\x00\x00\x00\x1b\x01\xc6\x00\x18\x01\x37\x00\xdd\x00\xe3\x00\x12\x01\x2b\x00\xdc\x00\x21\x01\x26\x00\x3d\x01\x3b\x01\x37\x01\x32\x01\x38\x01\x34\x01\x31\x01\x00\x00\x35\x01\x47\x01\x44\x01\x00\x00\x00\x00\x2e\x01\x00\x00\x54\x01\x52\x01\x00\x00\x51\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4e\x01\x4b\x01\x45\x01\x00\x00\x00\x00\x60\x01\x5e\x01\x5d\x01\x5b\x01\x59\x01\x58\x01\x65\x01\x63\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (36, 169) [
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106),
	(107 , happyReduce_107),
	(108 , happyReduce_108),
	(109 , happyReduce_109),
	(110 , happyReduce_110),
	(111 , happyReduce_111),
	(112 , happyReduce_112),
	(113 , happyReduce_113),
	(114 , happyReduce_114),
	(115 , happyReduce_115),
	(116 , happyReduce_116),
	(117 , happyReduce_117),
	(118 , happyReduce_118),
	(119 , happyReduce_119),
	(120 , happyReduce_120),
	(121 , happyReduce_121),
	(122 , happyReduce_122),
	(123 , happyReduce_123),
	(124 , happyReduce_124),
	(125 , happyReduce_125),
	(126 , happyReduce_126),
	(127 , happyReduce_127),
	(128 , happyReduce_128),
	(129 , happyReduce_129),
	(130 , happyReduce_130),
	(131 , happyReduce_131),
	(132 , happyReduce_132),
	(133 , happyReduce_133),
	(134 , happyReduce_134),
	(135 , happyReduce_135),
	(136 , happyReduce_136),
	(137 , happyReduce_137),
	(138 , happyReduce_138),
	(139 , happyReduce_139),
	(140 , happyReduce_140),
	(141 , happyReduce_141),
	(142 , happyReduce_142),
	(143 , happyReduce_143),
	(144 , happyReduce_144),
	(145 , happyReduce_145),
	(146 , happyReduce_146),
	(147 , happyReduce_147),
	(148 , happyReduce_148),
	(149 , happyReduce_149),
	(150 , happyReduce_150),
	(151 , happyReduce_151),
	(152 , happyReduce_152),
	(153 , happyReduce_153),
	(154 , happyReduce_154),
	(155 , happyReduce_155),
	(156 , happyReduce_156),
	(157 , happyReduce_157),
	(158 , happyReduce_158),
	(159 , happyReduce_159),
	(160 , happyReduce_160),
	(161 , happyReduce_161),
	(162 , happyReduce_162),
	(163 , happyReduce_163),
	(164 , happyReduce_164),
	(165 , happyReduce_165),
	(166 , happyReduce_166),
	(167 , happyReduce_167),
	(168 , happyReduce_168),
	(169 , happyReduce_169)
	]

happy_n_terms = 83 :: Prelude.Int
happy_n_nonterms = 39 :: Prelude.Int

happyReduce_36 = happySpecReduce_1  0# happyReduction_36
happyReduction_36 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TV happy_var_1)) -> 
	happyIn39
		 (AbsCoLa.Ident happy_var_1
	)}

happyReduce_37 = happySpecReduce_1  1# happyReduction_37
happyReduction_37 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TI happy_var_1)) -> 
	happyIn40
		 ((read happy_var_1) :: Integer
	)}

happyReduce_38 = happySpecReduce_1  2# happyReduction_38
happyReduction_38 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TL happy_var_1)) -> 
	happyIn41
		 (happy_var_1
	)}

happyReduce_39 = happySpecReduce_0  3# happyReduction_39
happyReduction_39  =  happyIn42
		 (AbsCoLa.ConEmpty
	)

happyReduce_40 = happySpecReduce_1  3# happyReduction_40
happyReduction_40 happy_x_1
	 =  case happyOut43 happy_x_1 of { (HappyWrap43 happy_var_1) -> 
	happyIn42
		 (AbsCoLa.ConComp happy_var_1
	)}

happyReduce_41 = happySpecReduce_3  3# happyReduction_41
happyReduction_41 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { (HappyWrap43 happy_var_1) -> 
	case happyOut42 happy_x_3 of { (HappyWrap42 happy_var_3) -> 
	happyIn42
		 (AbsCoLa.ConAnd happy_var_1 happy_var_3
	)}}

happyReduce_42 = happySpecReduce_1  4# happyReduction_42
happyReduction_42 happy_x_1
	 =  case happyOut44 happy_x_1 of { (HappyWrap44 happy_var_1) -> 
	happyIn43
		 (AbsCoLa.ComDef happy_var_1
	)}

happyReduce_43 = happySpecReduce_1  4# happyReduction_43
happyReduction_43 happy_x_1
	 =  case happyOut48 happy_x_1 of { (HappyWrap48 happy_var_1) -> 
	happyIn43
		 (AbsCoLa.ComConDef happy_var_1
	)}

happyReduce_44 = happySpecReduce_1  4# happyReduction_44
happyReduction_44 happy_x_1
	 =  case happyOut49 happy_x_1 of { (HappyWrap49 happy_var_1) -> 
	happyIn43
		 (AbsCoLa.ComState happy_var_1
	)}

happyReduce_45 = happySpecReduce_1  4# happyReduction_45
happyReduction_45 happy_x_1
	 =  case happyOut50 happy_x_1 of { (HappyWrap50 happy_var_1) -> 
	happyIn43
		 (AbsCoLa.ComConState happy_var_1
	)}

happyReduce_46 = happySpecReduce_1  5# happyReduction_46
happyReduction_46 happy_x_1
	 =  case happyOut45 happy_x_1 of { (HappyWrap45 happy_var_1) -> 
	happyIn44
		 (AbsCoLa.DefSim happy_var_1
	)}

happyReduce_47 = happySpecReduce_3  5# happyReduction_47
happyReduction_47 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_1 of { (HappyWrap45 happy_var_1) -> 
	case happyOut44 happy_x_3 of { (HappyWrap44 happy_var_3) -> 
	happyIn44
		 (AbsCoLa.DefAnd happy_var_1 happy_var_3
	)}}

happyReduce_48 = happyReduce 4# 6# happyReduction_48
happyReduction_48 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut55 happy_x_1 of { (HappyWrap55 happy_var_1) -> 
	case happyOut57 happy_x_2 of { (HappyWrap57 happy_var_2) -> 
	case happyOut57 happy_x_4 of { (HappyWrap57 happy_var_4) -> 
	happyIn45
		 (AbsCoLa.SimDefIs happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_49 = happyReduce 4# 6# happyReduction_49
happyReduction_49 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut55 happy_x_1 of { (HappyWrap55 happy_var_1) -> 
	case happyOut57 happy_x_2 of { (HappyWrap57 happy_var_2) -> 
	case happyOut46 happy_x_4 of { (HappyWrap46 happy_var_4) -> 
	happyIn45
		 (AbsCoLa.SimDefEq happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_50 = happyReduce 6# 6# happyReduction_50
happyReduction_50 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut55 happy_x_1 of { (HappyWrap55 happy_var_1) -> 
	case happyOut57 happy_x_2 of { (HappyWrap57 happy_var_2) -> 
	case happyOut76 happy_x_4 of { (HappyWrap76 happy_var_4) -> 
	case happyOut69 happy_x_5 of { (HappyWrap69 happy_var_5) -> 
	case happyOut76 happy_x_6 of { (HappyWrap76 happy_var_6) -> 
	happyIn45
		 (AbsCoLa.SimDefDate happy_var_1 happy_var_2 happy_var_4 happy_var_5 happy_var_6
	) `HappyStk` happyRest}}}}}

happyReduce_51 = happySpecReduce_1  7# happyReduction_51
happyReduction_51 happy_x_1
	 =  case happyOut76 happy_x_1 of { (HappyWrap76 happy_var_1) -> 
	happyIn46
		 (AbsCoLa.NumExpNum happy_var_1
	)}

happyReduce_52 = happySpecReduce_1  7# happyReduction_52
happyReduction_52 happy_x_1
	 =  case happyOut71 happy_x_1 of { (HappyWrap71 happy_var_1) -> 
	happyIn46
		 (AbsCoLa.NumExpObj happy_var_1
	)}

happyReduce_53 = happySpecReduce_3  7# happyReduction_53
happyReduction_53 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut46 happy_x_1 of { (HappyWrap46 happy_var_1) -> 
	case happyOut47 happy_x_2 of { (HappyWrap47 happy_var_2) -> 
	case happyOut46 happy_x_3 of { (HappyWrap46 happy_var_3) -> 
	happyIn46
		 (AbsCoLa.NumExpOp happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_54 = happySpecReduce_1  8# happyReduction_54
happyReduction_54 happy_x_1
	 =  happyIn47
		 (AbsCoLa.OpPlus
	)

happyReduce_55 = happySpecReduce_1  8# happyReduction_55
happyReduction_55 happy_x_1
	 =  happyIn47
		 (AbsCoLa.OpMin
	)

happyReduce_56 = happySpecReduce_1  8# happyReduction_56
happyReduction_56 happy_x_1
	 =  happyIn47
		 (AbsCoLa.OpMult
	)

happyReduce_57 = happySpecReduce_1  8# happyReduction_57
happyReduction_57 happy_x_1
	 =  happyIn47
		 (AbsCoLa.OpDiv
	)

happyReduce_58 = happySpecReduce_3  9# happyReduction_58
happyReduction_58 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut44 happy_x_1 of { (HappyWrap44 happy_var_1) -> 
	case happyOut52 happy_x_3 of { (HappyWrap52 happy_var_3) -> 
	happyIn48
		 (AbsCoLa.ConDefIf happy_var_1 happy_var_3
	)}}

happyReduce_59 = happyReduce 4# 9# happyReduction_59
happyReduction_59 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut52 happy_x_2 of { (HappyWrap52 happy_var_2) -> 
	case happyOut44 happy_x_4 of { (HappyWrap44 happy_var_4) -> 
	happyIn48
		 (AbsCoLa.ConDefIfThen happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_60 = happyReduce 5# 9# happyReduction_60
happyReduction_60 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut44 happy_x_1 of { (HappyWrap44 happy_var_1) -> 
	case happyOut52 happy_x_3 of { (HappyWrap52 happy_var_3) -> 
	case happyOut44 happy_x_5 of { (HappyWrap44 happy_var_5) -> 
	happyIn48
		 (AbsCoLa.ConDefIfElse happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_61 = happyReduce 6# 9# happyReduction_61
happyReduction_61 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut52 happy_x_2 of { (HappyWrap52 happy_var_2) -> 
	case happyOut44 happy_x_4 of { (HappyWrap44 happy_var_4) -> 
	case happyOut44 happy_x_6 of { (HappyWrap44 happy_var_6) -> 
	happyIn48
		 (AbsCoLa.ConDefIfThenElse happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_62 = happySpecReduce_1  10# happyReduction_62
happyReduction_62 happy_x_1
	 =  case happyOut51 happy_x_1 of { (HappyWrap51 happy_var_1) -> 
	happyIn49
		 (AbsCoLa.StateSim happy_var_1
	)}

happyReduce_63 = happySpecReduce_3  10# happyReduction_63
happyReduction_63 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut51 happy_x_1 of { (HappyWrap51 happy_var_1) -> 
	case happyOut49 happy_x_3 of { (HappyWrap49 happy_var_3) -> 
	happyIn49
		 (AbsCoLa.StateOr happy_var_1 happy_var_3
	)}}

happyReduce_64 = happySpecReduce_3  10# happyReduction_64
happyReduction_64 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut51 happy_x_1 of { (HappyWrap51 happy_var_1) -> 
	case happyOut49 happy_x_3 of { (HappyWrap49 happy_var_3) -> 
	happyIn49
		 (AbsCoLa.StateAnd happy_var_1 happy_var_3
	)}}

happyReduce_65 = happySpecReduce_3  11# happyReduction_65
happyReduction_65 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut49 happy_x_1 of { (HappyWrap49 happy_var_1) -> 
	case happyOut52 happy_x_3 of { (HappyWrap52 happy_var_3) -> 
	happyIn50
		 (AbsCoLa.ConStateIf happy_var_1 happy_var_3
	)}}

happyReduce_66 = happyReduce 4# 11# happyReduction_66
happyReduction_66 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut52 happy_x_2 of { (HappyWrap52 happy_var_2) -> 
	case happyOut49 happy_x_4 of { (HappyWrap49 happy_var_4) -> 
	happyIn50
		 (AbsCoLa.ConStateIfThen happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_67 = happyReduce 5# 11# happyReduction_67
happyReduction_67 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut49 happy_x_1 of { (HappyWrap49 happy_var_1) -> 
	case happyOut52 happy_x_3 of { (HappyWrap52 happy_var_3) -> 
	case happyOut49 happy_x_5 of { (HappyWrap49 happy_var_5) -> 
	happyIn50
		 (AbsCoLa.ConStateIfElse happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_68 = happyReduce 6# 11# happyReduction_68
happyReduction_68 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut52 happy_x_2 of { (HappyWrap52 happy_var_2) -> 
	case happyOut49 happy_x_4 of { (HappyWrap49 happy_var_4) -> 
	case happyOut49 happy_x_6 of { (HappyWrap49 happy_var_6) -> 
	happyIn50
		 (AbsCoLa.ConStateIfThenElse happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_69 = happyReduce 8# 12# happyReduction_69
happyReduction_69 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut55 happy_x_1 of { (HappyWrap55 happy_var_1) -> 
	case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	case happyOut57 happy_x_3 of { (HappyWrap57 happy_var_3) -> 
	case happyOut63 happy_x_4 of { (HappyWrap63 happy_var_4) -> 
	case happyOut58 happy_x_5 of { (HappyWrap58 happy_var_5) -> 
	case happyOut70 happy_x_6 of { (HappyWrap70 happy_var_6) -> 
	case happyOut77 happy_x_7 of { (HappyWrap77 happy_var_7) -> 
	case happyOut65 happy_x_8 of { (HappyWrap65 happy_var_8) -> 
	happyIn51
		 (AbsCoLa.SimStateOne happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6 happy_var_7 happy_var_8
	) `HappyStk` happyRest}}}}}}}}

happyReduce_70 = happyReduce 8# 12# happyReduction_70
happyReduction_70 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut55 happy_x_1 of { (HappyWrap55 happy_var_1) -> 
	case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	case happyOut57 happy_x_3 of { (HappyWrap57 happy_var_3) -> 
	case happyOut65 happy_x_4 of { (HappyWrap65 happy_var_4) -> 
	case happyOut63 happy_x_5 of { (HappyWrap63 happy_var_5) -> 
	case happyOut58 happy_x_6 of { (HappyWrap58 happy_var_6) -> 
	case happyOut70 happy_x_7 of { (HappyWrap70 happy_var_7) -> 
	case happyOut77 happy_x_8 of { (HappyWrap77 happy_var_8) -> 
	happyIn51
		 (AbsCoLa.SimStateTwo happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6 happy_var_7 happy_var_8
	) `HappyStk` happyRest}}}}}}}}

happyReduce_71 = happyReduce 8# 12# happyReduction_71
happyReduction_71 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut55 happy_x_1 of { (HappyWrap55 happy_var_1) -> 
	case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	case happyOut65 happy_x_3 of { (HappyWrap65 happy_var_3) -> 
	case happyOut57 happy_x_4 of { (HappyWrap57 happy_var_4) -> 
	case happyOut63 happy_x_5 of { (HappyWrap63 happy_var_5) -> 
	case happyOut58 happy_x_6 of { (HappyWrap58 happy_var_6) -> 
	case happyOut70 happy_x_7 of { (HappyWrap70 happy_var_7) -> 
	case happyOut77 happy_x_8 of { (HappyWrap77 happy_var_8) -> 
	happyIn51
		 (AbsCoLa.SimStateThree happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6 happy_var_7 happy_var_8
	) `HappyStk` happyRest}}}}}}}}

happyReduce_72 = happyReduce 7# 12# happyReduction_72
happyReduction_72 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut55 happy_x_1 of { (HappyWrap55 happy_var_1) -> 
	case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	case happyOut57 happy_x_3 of { (HappyWrap57 happy_var_3) -> 
	case happyOut59 happy_x_4 of { (HappyWrap59 happy_var_4) -> 
	case happyOut70 happy_x_5 of { (HappyWrap70 happy_var_5) -> 
	case happyOut77 happy_x_6 of { (HappyWrap77 happy_var_6) -> 
	case happyOut65 happy_x_7 of { (HappyWrap65 happy_var_7) -> 
	happyIn51
		 (AbsCoLa.SimStateFour happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6 happy_var_7
	) `HappyStk` happyRest}}}}}}}

happyReduce_73 = happyReduce 7# 12# happyReduction_73
happyReduction_73 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut55 happy_x_1 of { (HappyWrap55 happy_var_1) -> 
	case happyOut57 happy_x_2 of { (HappyWrap57 happy_var_2) -> 
	case happyOut63 happy_x_3 of { (HappyWrap63 happy_var_3) -> 
	case happyOut58 happy_x_4 of { (HappyWrap58 happy_var_4) -> 
	case happyOut70 happy_x_5 of { (HappyWrap70 happy_var_5) -> 
	case happyOut77 happy_x_6 of { (HappyWrap77 happy_var_6) -> 
	case happyOut65 happy_x_7 of { (HappyWrap65 happy_var_7) -> 
	happyIn51
		 (AbsCoLa.SimStateOneNH happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6 happy_var_7
	) `HappyStk` happyRest}}}}}}}

happyReduce_74 = happyReduce 7# 12# happyReduction_74
happyReduction_74 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut55 happy_x_1 of { (HappyWrap55 happy_var_1) -> 
	case happyOut57 happy_x_2 of { (HappyWrap57 happy_var_2) -> 
	case happyOut65 happy_x_3 of { (HappyWrap65 happy_var_3) -> 
	case happyOut63 happy_x_4 of { (HappyWrap63 happy_var_4) -> 
	case happyOut58 happy_x_5 of { (HappyWrap58 happy_var_5) -> 
	case happyOut70 happy_x_6 of { (HappyWrap70 happy_var_6) -> 
	case happyOut77 happy_x_7 of { (HappyWrap77 happy_var_7) -> 
	happyIn51
		 (AbsCoLa.SimStateTwoNH happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6 happy_var_7
	) `HappyStk` happyRest}}}}}}}

happyReduce_75 = happyReduce 7# 12# happyReduction_75
happyReduction_75 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut55 happy_x_1 of { (HappyWrap55 happy_var_1) -> 
	case happyOut65 happy_x_2 of { (HappyWrap65 happy_var_2) -> 
	case happyOut57 happy_x_3 of { (HappyWrap57 happy_var_3) -> 
	case happyOut63 happy_x_4 of { (HappyWrap63 happy_var_4) -> 
	case happyOut58 happy_x_5 of { (HappyWrap58 happy_var_5) -> 
	case happyOut70 happy_x_6 of { (HappyWrap70 happy_var_6) -> 
	case happyOut77 happy_x_7 of { (HappyWrap77 happy_var_7) -> 
	happyIn51
		 (AbsCoLa.SimStateThreeNH happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6 happy_var_7
	) `HappyStk` happyRest}}}}}}}

happyReduce_76 = happyReduce 6# 12# happyReduction_76
happyReduction_76 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut55 happy_x_1 of { (HappyWrap55 happy_var_1) -> 
	case happyOut57 happy_x_2 of { (HappyWrap57 happy_var_2) -> 
	case happyOut59 happy_x_3 of { (HappyWrap59 happy_var_3) -> 
	case happyOut70 happy_x_4 of { (HappyWrap70 happy_var_4) -> 
	case happyOut77 happy_x_5 of { (HappyWrap77 happy_var_5) -> 
	case happyOut65 happy_x_6 of { (HappyWrap65 happy_var_6) -> 
	happyIn51
		 (AbsCoLa.SimStateFourNH happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6
	) `HappyStk` happyRest}}}}}}

happyReduce_77 = happySpecReduce_1  13# happyReduction_77
happyReduction_77 happy_x_1
	 =  case happyOut53 happy_x_1 of { (HappyWrap53 happy_var_1) -> 
	happyIn52
		 (AbsCoLa.CondiSim happy_var_1
	)}

happyReduce_78 = happySpecReduce_3  13# happyReduction_78
happyReduction_78 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_1 of { (HappyWrap53 happy_var_1) -> 
	case happyOut52 happy_x_3 of { (HappyWrap52 happy_var_3) -> 
	happyIn52
		 (AbsCoLa.CondiOr happy_var_1 happy_var_3
	)}}

happyReduce_79 = happySpecReduce_3  13# happyReduction_79
happyReduction_79 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_1 of { (HappyWrap53 happy_var_1) -> 
	case happyOut52 happy_x_3 of { (HappyWrap52 happy_var_3) -> 
	happyIn52
		 (AbsCoLa.CondiAnd happy_var_1 happy_var_3
	)}}

happyReduce_80 = happyReduce 7# 14# happyReduction_80
happyReduction_80 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut55 happy_x_1 of { (HappyWrap55 happy_var_1) -> 
	case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	case happyOut57 happy_x_3 of { (HappyWrap57 happy_var_3) -> 
	case happyOut59 happy_x_4 of { (HappyWrap59 happy_var_4) -> 
	case happyOut70 happy_x_5 of { (HappyWrap70 happy_var_5) -> 
	case happyOut77 happy_x_6 of { (HappyWrap77 happy_var_6) -> 
	case happyOut65 happy_x_7 of { (HappyWrap65 happy_var_7) -> 
	happyIn53
		 (AbsCoLa.SimConOne happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6 happy_var_7
	) `HappyStk` happyRest}}}}}}}

happyReduce_81 = happyReduce 7# 14# happyReduction_81
happyReduction_81 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut55 happy_x_1 of { (HappyWrap55 happy_var_1) -> 
	case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	case happyOut57 happy_x_3 of { (HappyWrap57 happy_var_3) -> 
	case happyOut65 happy_x_4 of { (HappyWrap65 happy_var_4) -> 
	case happyOut59 happy_x_5 of { (HappyWrap59 happy_var_5) -> 
	case happyOut70 happy_x_6 of { (HappyWrap70 happy_var_6) -> 
	case happyOut77 happy_x_7 of { (HappyWrap77 happy_var_7) -> 
	happyIn53
		 (AbsCoLa.SimConTwo happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6 happy_var_7
	) `HappyStk` happyRest}}}}}}}

happyReduce_82 = happyReduce 7# 14# happyReduction_82
happyReduction_82 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut55 happy_x_1 of { (HappyWrap55 happy_var_1) -> 
	case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	case happyOut65 happy_x_3 of { (HappyWrap65 happy_var_3) -> 
	case happyOut57 happy_x_4 of { (HappyWrap57 happy_var_4) -> 
	case happyOut59 happy_x_5 of { (HappyWrap59 happy_var_5) -> 
	case happyOut70 happy_x_6 of { (HappyWrap70 happy_var_6) -> 
	case happyOut77 happy_x_7 of { (HappyWrap77 happy_var_7) -> 
	happyIn53
		 (AbsCoLa.SimConThree happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6 happy_var_7
	) `HappyStk` happyRest}}}}}}}

happyReduce_83 = happyReduce 8# 14# happyReduction_83
happyReduction_83 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut55 happy_x_1 of { (HappyWrap55 happy_var_1) -> 
	case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	case happyOut57 happy_x_3 of { (HappyWrap57 happy_var_3) -> 
	case happyOut63 happy_x_4 of { (HappyWrap63 happy_var_4) -> 
	case happyOut58 happy_x_5 of { (HappyWrap58 happy_var_5) -> 
	case happyOut70 happy_x_6 of { (HappyWrap70 happy_var_6) -> 
	case happyOut77 happy_x_7 of { (HappyWrap77 happy_var_7) -> 
	case happyOut65 happy_x_8 of { (HappyWrap65 happy_var_8) -> 
	happyIn53
		 (AbsCoLa.SimConFour happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6 happy_var_7 happy_var_8
	) `HappyStk` happyRest}}}}}}}}

happyReduce_84 = happySpecReduce_3  14# happyReduction_84
happyReduction_84 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut55 happy_x_1 of { (HappyWrap55 happy_var_1) -> 
	case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	case happyOut54 happy_x_3 of { (HappyWrap54 happy_var_3) -> 
	happyIn53
		 (AbsCoLa.SimConFive happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_85 = happyReduce 6# 14# happyReduction_85
happyReduction_85 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut55 happy_x_1 of { (HappyWrap55 happy_var_1) -> 
	case happyOut57 happy_x_2 of { (HappyWrap57 happy_var_2) -> 
	case happyOut59 happy_x_3 of { (HappyWrap59 happy_var_3) -> 
	case happyOut70 happy_x_4 of { (HappyWrap70 happy_var_4) -> 
	case happyOut77 happy_x_5 of { (HappyWrap77 happy_var_5) -> 
	case happyOut65 happy_x_6 of { (HappyWrap65 happy_var_6) -> 
	happyIn53
		 (AbsCoLa.SimConOneNH happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6
	) `HappyStk` happyRest}}}}}}

happyReduce_86 = happyReduce 6# 14# happyReduction_86
happyReduction_86 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut55 happy_x_1 of { (HappyWrap55 happy_var_1) -> 
	case happyOut57 happy_x_2 of { (HappyWrap57 happy_var_2) -> 
	case happyOut65 happy_x_3 of { (HappyWrap65 happy_var_3) -> 
	case happyOut59 happy_x_4 of { (HappyWrap59 happy_var_4) -> 
	case happyOut70 happy_x_5 of { (HappyWrap70 happy_var_5) -> 
	case happyOut77 happy_x_6 of { (HappyWrap77 happy_var_6) -> 
	happyIn53
		 (AbsCoLa.SimConTwoNH happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6
	) `HappyStk` happyRest}}}}}}

happyReduce_87 = happyReduce 6# 14# happyReduction_87
happyReduction_87 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut55 happy_x_1 of { (HappyWrap55 happy_var_1) -> 
	case happyOut65 happy_x_2 of { (HappyWrap65 happy_var_2) -> 
	case happyOut57 happy_x_3 of { (HappyWrap57 happy_var_3) -> 
	case happyOut59 happy_x_4 of { (HappyWrap59 happy_var_4) -> 
	case happyOut70 happy_x_5 of { (HappyWrap70 happy_var_5) -> 
	case happyOut77 happy_x_6 of { (HappyWrap77 happy_var_6) -> 
	happyIn53
		 (AbsCoLa.SimConThreeNH happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6
	) `HappyStk` happyRest}}}}}}

happyReduce_88 = happyReduce 7# 14# happyReduction_88
happyReduction_88 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut55 happy_x_1 of { (HappyWrap55 happy_var_1) -> 
	case happyOut57 happy_x_2 of { (HappyWrap57 happy_var_2) -> 
	case happyOut63 happy_x_3 of { (HappyWrap63 happy_var_3) -> 
	case happyOut58 happy_x_4 of { (HappyWrap58 happy_var_4) -> 
	case happyOut70 happy_x_5 of { (HappyWrap70 happy_var_5) -> 
	case happyOut77 happy_x_6 of { (HappyWrap77 happy_var_6) -> 
	case happyOut65 happy_x_7 of { (HappyWrap65 happy_var_7) -> 
	happyIn53
		 (AbsCoLa.SimConFourNH happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6 happy_var_7
	) `HappyStk` happyRest}}}}}}}

happyReduce_89 = happySpecReduce_2  14# happyReduction_89
happyReduction_89 happy_x_2
	happy_x_1
	 =  case happyOut55 happy_x_1 of { (HappyWrap55 happy_var_1) -> 
	case happyOut54 happy_x_2 of { (HappyWrap54 happy_var_2) -> 
	happyIn53
		 (AbsCoLa.SimConFiveNH happy_var_1 happy_var_2
	)}}

happyReduce_90 = happyReduce 4# 15# happyReduction_90
happyReduction_90 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut57 happy_x_1 of { (HappyWrap57 happy_var_1) -> 
	case happyOut59 happy_x_2 of { (HappyWrap59 happy_var_2) -> 
	case happyOut60 happy_x_3 of { (HappyWrap60 happy_var_3) -> 
	case happyOut57 happy_x_4 of { (HappyWrap57 happy_var_4) -> 
	happyIn54
		 (AbsCoLa.BoolEx happy_var_1 happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}}

happyReduce_91 = happySpecReduce_3  16# happyReduction_91
happyReduction_91 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut76 happy_x_2 of { (HappyWrap76 happy_var_2) -> 
	happyIn55
		 (AbsCoLa.IDSim happy_var_2
	)}

happyReduce_92 = happyReduce 6# 16# happyReduction_92
happyReduction_92 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut76 happy_x_2 of { (HappyWrap76 happy_var_2) -> 
	case happyOut76 happy_x_4 of { (HappyWrap76 happy_var_4) -> 
	happyIn55
		 (AbsCoLa.IDRep happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_93 = happyReduce 5# 17# happyReduction_93
happyReduction_93 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = happyIn56
		 (AbsCoLa.HoldYes
	) `HappyStk` happyRest

happyReduce_94 = happyReduce 6# 17# happyReduction_94
happyReduction_94 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = happyIn56
		 (AbsCoLa.HoldNo
	) `HappyStk` happyRest

happyReduce_95 = happySpecReduce_1  18# happyReduction_95
happyReduction_95 happy_x_1
	 =  case happyOut41 happy_x_1 of { (HappyWrap41 happy_var_1) -> 
	happyIn57
		 (AbsCoLa.SubQuoted happy_var_1
	)}

happyReduce_96 = happySpecReduce_1  18# happyReduction_96
happyReduction_96 happy_x_1
	 =  case happyOut39 happy_x_1 of { (HappyWrap39 happy_var_1) -> 
	happyIn57
		 (AbsCoLa.SubUnQuoted happy_var_1
	)}

happyReduce_97 = happySpecReduce_1  19# happyReduction_97
happyReduction_97 happy_x_1
	 =  happyIn58
		 (AbsCoLa.VDel
	)

happyReduce_98 = happySpecReduce_1  19# happyReduction_98
happyReduction_98 happy_x_1
	 =  happyIn58
		 (AbsCoLa.VPay
	)

happyReduce_99 = happySpecReduce_1  19# happyReduction_99
happyReduction_99 happy_x_1
	 =  happyIn58
		 (AbsCoLa.VCharge
	)

happyReduce_100 = happySpecReduce_1  19# happyReduction_100
happyReduction_100 happy_x_1
	 =  happyIn58
		 (AbsCoLa.VRefund
	)

happyReduce_101 = happySpecReduce_1  20# happyReduction_101
happyReduction_101 happy_x_1
	 =  happyIn59
		 (AbsCoLa.VSDel
	)

happyReduce_102 = happySpecReduce_1  20# happyReduction_102
happyReduction_102 happy_x_1
	 =  happyIn59
		 (AbsCoLa.VSPay
	)

happyReduce_103 = happySpecReduce_1  20# happyReduction_103
happyReduction_103 happy_x_1
	 =  happyIn59
		 (AbsCoLa.VSCharge
	)

happyReduce_104 = happySpecReduce_1  20# happyReduction_104
happyReduction_104 happy_x_1
	 =  happyIn59
		 (AbsCoLa.VSRefund
	)

happyReduce_105 = happySpecReduce_2  21# happyReduction_105
happyReduction_105 happy_x_2
	happy_x_1
	 =  happyIn60
		 (AbsCoLa.CompareLess
	)

happyReduce_106 = happySpecReduce_1  21# happyReduction_106
happyReduction_106 happy_x_1
	 =  case happyOut61 happy_x_1 of { (HappyWrap61 happy_var_1) -> 
	happyIn60
		 (AbsCoLa.CompareEq happy_var_1
	)}

happyReduce_107 = happySpecReduce_1  21# happyReduction_107
happyReduction_107 happy_x_1
	 =  case happyOut62 happy_x_1 of { (HappyWrap62 happy_var_1) -> 
	happyIn60
		 (AbsCoLa.CompareMore happy_var_1
	)}

happyReduce_108 = happySpecReduce_1  22# happyReduction_108
happyReduction_108 happy_x_1
	 =  happyIn61
		 (AbsCoLa.EqOne
	)

happyReduce_109 = happySpecReduce_2  22# happyReduction_109
happyReduction_109 happy_x_2
	happy_x_1
	 =  happyIn61
		 (AbsCoLa.EqTwo
	)

happyReduce_110 = happySpecReduce_2  23# happyReduction_110
happyReduction_110 happy_x_2
	happy_x_1
	 =  happyIn62
		 (AbsCoLa.MoreOne
	)

happyReduce_111 = happySpecReduce_2  23# happyReduction_111
happyReduction_111 happy_x_2
	happy_x_1
	 =  happyIn62
		 (AbsCoLa.MoreTwo
	)

happyReduce_112 = happySpecReduce_1  24# happyReduction_112
happyReduction_112 happy_x_1
	 =  case happyOut64 happy_x_1 of { (HappyWrap64 happy_var_1) -> 
	happyIn63
		 (AbsCoLa.ModalObli happy_var_1
	)}

happyReduce_113 = happySpecReduce_1  24# happyReduction_113
happyReduction_113 happy_x_1
	 =  happyIn63
		 (AbsCoLa.ModalPermi
	)

happyReduce_114 = happySpecReduce_3  24# happyReduction_114
happyReduction_114 happy_x_3
	happy_x_2
	happy_x_1
	 =  happyIn63
		 (AbsCoLa.ModalForbi
	)

happyReduce_115 = happySpecReduce_1  25# happyReduction_115
happyReduction_115 happy_x_1
	 =  happyIn64
		 (AbsCoLa.ObliOne
	)

happyReduce_116 = happySpecReduce_1  25# happyReduction_116
happyReduction_116 happy_x_1
	 =  happyIn64
		 (AbsCoLa.ObliTwo
	)

happyReduce_117 = happySpecReduce_1  26# happyReduction_117
happyReduction_117 happy_x_1
	 =  case happyOut66 happy_x_1 of { (HappyWrap66 happy_var_1) -> 
	happyIn65
		 (AbsCoLa.DateSpe happy_var_1
	)}

happyReduce_118 = happySpecReduce_2  26# happyReduction_118
happyReduction_118 happy_x_2
	happy_x_1
	 =  happyIn65
		 (AbsCoLa.DateAny
	)

happyReduce_119 = happySpecReduce_3  26# happyReduction_119
happyReduction_119 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut57 happy_x_3 of { (HappyWrap57 happy_var_3) -> 
	happyIn65
		 (AbsCoLa.DateSome happy_var_3
	)}

happyReduce_120 = happySpecReduce_3  26# happyReduction_120
happyReduction_120 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut57 happy_x_3 of { (HappyWrap57 happy_var_3) -> 
	happyIn65
		 (AbsCoLa.DateThe happy_var_3
	)}

happyReduce_121 = happyReduce 4# 26# happyReduction_121
happyReduction_121 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut67 happy_x_1 of { (HappyWrap67 happy_var_1) -> 
	case happyOut76 happy_x_2 of { (HappyWrap76 happy_var_2) -> 
	case happyOut69 happy_x_3 of { (HappyWrap69 happy_var_3) -> 
	case happyOut76 happy_x_4 of { (HappyWrap76 happy_var_4) -> 
	happyIn65
		 (AbsCoLa.DateQuanSpecific happy_var_1 happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}}

happyReduce_122 = happySpecReduce_3  26# happyReduction_122
happyReduction_122 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut67 happy_x_1 of { (HappyWrap67 happy_var_1) -> 
	case happyOut57 happy_x_3 of { (HappyWrap57 happy_var_3) -> 
	happyIn65
		 (AbsCoLa.DateQuanSome happy_var_1 happy_var_3
	)}}

happyReduce_123 = happySpecReduce_3  26# happyReduction_123
happyReduction_123 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut67 happy_x_1 of { (HappyWrap67 happy_var_1) -> 
	case happyOut57 happy_x_3 of { (HappyWrap57 happy_var_3) -> 
	happyIn65
		 (AbsCoLa.DateQuanThe happy_var_1 happy_var_3
	)}}

happyReduce_124 = happyReduce 4# 26# happyReduction_124
happyReduction_124 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut68 happy_x_1 of { (HappyWrap68 happy_var_1) -> 
	case happyOut67 happy_x_2 of { (HappyWrap67 happy_var_2) -> 
	case happyOut57 happy_x_4 of { (HappyWrap57 happy_var_4) -> 
	happyIn65
		 (AbsCoLa.DateQuanSomeWO happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_125 = happyReduce 4# 26# happyReduction_125
happyReduction_125 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut68 happy_x_1 of { (HappyWrap68 happy_var_1) -> 
	case happyOut67 happy_x_2 of { (HappyWrap67 happy_var_2) -> 
	case happyOut57 happy_x_4 of { (HappyWrap57 happy_var_4) -> 
	happyIn65
		 (AbsCoLa.DateQuanTheWO happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_126 = happyReduce 5# 26# happyReduction_126
happyReduction_126 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut67 happy_x_1 of { (HappyWrap67 happy_var_1) -> 
	case happyOut68 happy_x_2 of { (HappyWrap68 happy_var_2) -> 
	case happyOut67 happy_x_3 of { (HappyWrap67 happy_var_3) -> 
	case happyOut57 happy_x_5 of { (HappyWrap57 happy_var_5) -> 
	happyIn65
		 (AbsCoLa.DateQuanTempSome happy_var_1 happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}}

happyReduce_127 = happyReduce 5# 26# happyReduction_127
happyReduction_127 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut67 happy_x_1 of { (HappyWrap67 happy_var_1) -> 
	case happyOut68 happy_x_2 of { (HappyWrap68 happy_var_2) -> 
	case happyOut67 happy_x_3 of { (HappyWrap67 happy_var_3) -> 
	case happyOut57 happy_x_5 of { (HappyWrap57 happy_var_5) -> 
	happyIn65
		 (AbsCoLa.DateQuanTempThe happy_var_1 happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}}

happyReduce_128 = happyReduce 5# 27# happyReduction_128
happyReduction_128 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut76 happy_x_3 of { (HappyWrap76 happy_var_3) -> 
	case happyOut69 happy_x_4 of { (HappyWrap69 happy_var_4) -> 
	case happyOut76 happy_x_5 of { (HappyWrap76 happy_var_5) -> 
	happyIn66
		 (AbsCoLa.DateSpeOnThe happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_129 = happyReduce 4# 27# happyReduction_129
happyReduction_129 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut76 happy_x_2 of { (HappyWrap76 happy_var_2) -> 
	case happyOut69 happy_x_3 of { (HappyWrap69 happy_var_3) -> 
	case happyOut76 happy_x_4 of { (HappyWrap76 happy_var_4) -> 
	happyIn66
		 (AbsCoLa.DateSpeOn happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_130 = happySpecReduce_1  28# happyReduction_130
happyReduction_130 happy_x_1
	 =  happyIn67
		 (AbsCoLa.TempAfter
	)

happyReduce_131 = happySpecReduce_1  28# happyReduction_131
happyReduction_131 happy_x_1
	 =  happyIn67
		 (AbsCoLa.TempBefore
	)

happyReduce_132 = happySpecReduce_2  29# happyReduction_132
happyReduction_132 happy_x_2
	happy_x_1
	 =  case happyOut76 happy_x_1 of { (HappyWrap76 happy_var_1) -> 
	happyIn68
		 (AbsCoLa.TempOffDay happy_var_1
	)}

happyReduce_133 = happySpecReduce_2  29# happyReduction_133
happyReduction_133 happy_x_2
	happy_x_1
	 =  case happyOut76 happy_x_1 of { (HappyWrap76 happy_var_1) -> 
	happyIn68
		 (AbsCoLa.TempOffYear happy_var_1
	)}

happyReduce_134 = happySpecReduce_2  29# happyReduction_134
happyReduction_134 happy_x_2
	happy_x_1
	 =  case happyOut76 happy_x_1 of { (HappyWrap76 happy_var_1) -> 
	happyIn68
		 (AbsCoLa.TempOffWeek happy_var_1
	)}

happyReduce_135 = happySpecReduce_2  29# happyReduction_135
happyReduction_135 happy_x_2
	happy_x_1
	 =  case happyOut76 happy_x_1 of { (HappyWrap76 happy_var_1) -> 
	happyIn68
		 (AbsCoLa.TempOffDays happy_var_1
	)}

happyReduce_136 = happySpecReduce_2  29# happyReduction_136
happyReduction_136 happy_x_2
	happy_x_1
	 =  case happyOut76 happy_x_1 of { (HappyWrap76 happy_var_1) -> 
	happyIn68
		 (AbsCoLa.TempOffYears happy_var_1
	)}

happyReduce_137 = happySpecReduce_2  29# happyReduction_137
happyReduction_137 happy_x_2
	happy_x_1
	 =  case happyOut76 happy_x_1 of { (HappyWrap76 happy_var_1) -> 
	happyIn68
		 (AbsCoLa.TempOffWeeks happy_var_1
	)}

happyReduce_138 = happySpecReduce_1  30# happyReduction_138
happyReduction_138 happy_x_1
	 =  happyIn69
		 (AbsCoLa.MJan
	)

happyReduce_139 = happySpecReduce_1  30# happyReduction_139
happyReduction_139 happy_x_1
	 =  happyIn69
		 (AbsCoLa.MFeb
	)

happyReduce_140 = happySpecReduce_1  30# happyReduction_140
happyReduction_140 happy_x_1
	 =  happyIn69
		 (AbsCoLa.MMar
	)

happyReduce_141 = happySpecReduce_1  30# happyReduction_141
happyReduction_141 happy_x_1
	 =  happyIn69
		 (AbsCoLa.MApr
	)

happyReduce_142 = happySpecReduce_1  30# happyReduction_142
happyReduction_142 happy_x_1
	 =  happyIn69
		 (AbsCoLa.MMay
	)

happyReduce_143 = happySpecReduce_1  30# happyReduction_143
happyReduction_143 happy_x_1
	 =  happyIn69
		 (AbsCoLa.MJun
	)

happyReduce_144 = happySpecReduce_1  30# happyReduction_144
happyReduction_144 happy_x_1
	 =  happyIn69
		 (AbsCoLa.MJul
	)

happyReduce_145 = happySpecReduce_1  30# happyReduction_145
happyReduction_145 happy_x_1
	 =  happyIn69
		 (AbsCoLa.MAug
	)

happyReduce_146 = happySpecReduce_1  30# happyReduction_146
happyReduction_146 happy_x_1
	 =  happyIn69
		 (AbsCoLa.MSep
	)

happyReduce_147 = happySpecReduce_1  30# happyReduction_147
happyReduction_147 happy_x_1
	 =  happyIn69
		 (AbsCoLa.MOct
	)

happyReduce_148 = happySpecReduce_1  30# happyReduction_148
happyReduction_148 happy_x_1
	 =  happyIn69
		 (AbsCoLa.MNov
	)

happyReduce_149 = happySpecReduce_1  30# happyReduction_149
happyReduction_149 happy_x_1
	 =  happyIn69
		 (AbsCoLa.MDec
	)

happyReduce_150 = happySpecReduce_1  31# happyReduction_150
happyReduction_150 happy_x_1
	 =  case happyOut71 happy_x_1 of { (HappyWrap71 happy_var_1) -> 
	happyIn70
		 (AbsCoLa.ObjNu happy_var_1
	)}

happyReduce_151 = happySpecReduce_1  31# happyReduction_151
happyReduction_151 happy_x_1
	 =  case happyOut75 happy_x_1 of { (HappyWrap75 happy_var_1) -> 
	happyIn70
		 (AbsCoLa.ObjNonNu happy_var_1
	)}

happyReduce_152 = happySpecReduce_2  32# happyReduction_152
happyReduction_152 happy_x_2
	happy_x_1
	 =  case happyOut72 happy_x_1 of { (HappyWrap72 happy_var_1) -> 
	case happyOut76 happy_x_2 of { (HappyWrap76 happy_var_2) -> 
	happyIn71
		 (AbsCoLa.NumPound happy_var_1 happy_var_2
	)}}

happyReduce_153 = happySpecReduce_2  32# happyReduction_153
happyReduction_153 happy_x_2
	happy_x_1
	 =  case happyOut73 happy_x_1 of { (HappyWrap73 happy_var_1) -> 
	case happyOut76 happy_x_2 of { (HappyWrap76 happy_var_2) -> 
	happyIn71
		 (AbsCoLa.NumDol happy_var_1 happy_var_2
	)}}

happyReduce_154 = happySpecReduce_2  32# happyReduction_154
happyReduction_154 happy_x_2
	happy_x_1
	 =  case happyOut74 happy_x_1 of { (HappyWrap74 happy_var_1) -> 
	case happyOut76 happy_x_2 of { (HappyWrap76 happy_var_2) -> 
	happyIn71
		 (AbsCoLa.NumEur happy_var_1 happy_var_2
	)}}

happyReduce_155 = happySpecReduce_2  32# happyReduction_155
happyReduction_155 happy_x_2
	happy_x_1
	 =  case happyOut57 happy_x_2 of { (HappyWrap57 happy_var_2) -> 
	happyIn71
		 (AbsCoLa.NumAmount happy_var_2
	)}

happyReduce_156 = happySpecReduce_1  33# happyReduction_156
happyReduction_156 happy_x_1
	 =  happyIn72
		 (AbsCoLa.PoundOne
	)

happyReduce_157 = happySpecReduce_1  33# happyReduction_157
happyReduction_157 happy_x_1
	 =  happyIn72
		 (AbsCoLa.PoundTwo
	)

happyReduce_158 = happySpecReduce_1  33# happyReduction_158
happyReduction_158 happy_x_1
	 =  happyIn72
		 (AbsCoLa.PoundThree
	)

happyReduce_159 = happySpecReduce_1  34# happyReduction_159
happyReduction_159 happy_x_1
	 =  happyIn73
		 (AbsCoLa.DollarOne
	)

happyReduce_160 = happySpecReduce_1  34# happyReduction_160
happyReduction_160 happy_x_1
	 =  happyIn73
		 (AbsCoLa.DollarTwo
	)

happyReduce_161 = happySpecReduce_1  34# happyReduction_161
happyReduction_161 happy_x_1
	 =  happyIn73
		 (AbsCoLa.DollarThree
	)

happyReduce_162 = happySpecReduce_1  35# happyReduction_162
happyReduction_162 happy_x_1
	 =  happyIn74
		 (AbsCoLa.EuroOne
	)

happyReduce_163 = happySpecReduce_1  35# happyReduction_163
happyReduction_163 happy_x_1
	 =  happyIn74
		 (AbsCoLa.EuroTwo
	)

happyReduce_164 = happySpecReduce_2  36# happyReduction_164
happyReduction_164 happy_x_2
	happy_x_1
	 =  case happyOut57 happy_x_2 of { (HappyWrap57 happy_var_2) -> 
	happyIn75
		 (AbsCoLa.NonNumCurr happy_var_2
	)}

happyReduce_165 = happySpecReduce_2  36# happyReduction_165
happyReduction_165 happy_x_2
	happy_x_1
	 =  case happyOut57 happy_x_2 of { (HappyWrap57 happy_var_2) -> 
	happyIn75
		 (AbsCoLa.NonNumRep happy_var_2
	)}

happyReduce_166 = happySpecReduce_2  36# happyReduction_166
happyReduction_166 happy_x_2
	happy_x_1
	 =  case happyOut57 happy_x_2 of { (HappyWrap57 happy_var_2) -> 
	happyIn75
		 (AbsCoLa.NonNumNamed happy_var_2
	)}

happyReduce_167 = happySpecReduce_2  36# happyReduction_167
happyReduction_167 happy_x_2
	happy_x_1
	 =  case happyOut57 happy_x_2 of { (HappyWrap57 happy_var_2) -> 
	happyIn75
		 (AbsCoLa.NonNumOther happy_var_2
	)}

happyReduce_168 = happySpecReduce_1  37# happyReduction_168
happyReduction_168 happy_x_1
	 =  case happyOut40 happy_x_1 of { (HappyWrap40 happy_var_1) -> 
	happyIn76
		 (AbsCoLa.NumInt happy_var_1
	)}

happyReduce_169 = happySpecReduce_2  38# happyReduction_169
happyReduction_169 happy_x_2
	happy_x_1
	 =  case happyOut57 happy_x_2 of { (HappyWrap57 happy_var_2) -> 
	happyIn77
		 (AbsCoLa.Rec happy_var_2
	)}

happyNewToken action sts stk [] =
	happyDoAction 82# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TS _ 19) -> cont 19#;
	PT _ (TS _ 20) -> cont 20#;
	PT _ (TS _ 21) -> cont 21#;
	PT _ (TS _ 22) -> cont 22#;
	PT _ (TS _ 23) -> cont 23#;
	PT _ (TS _ 24) -> cont 24#;
	PT _ (TS _ 25) -> cont 25#;
	PT _ (TS _ 26) -> cont 26#;
	PT _ (TS _ 27) -> cont 27#;
	PT _ (TS _ 28) -> cont 28#;
	PT _ (TS _ 29) -> cont 29#;
	PT _ (TS _ 30) -> cont 30#;
	PT _ (TS _ 31) -> cont 31#;
	PT _ (TS _ 32) -> cont 32#;
	PT _ (TS _ 33) -> cont 33#;
	PT _ (TS _ 34) -> cont 34#;
	PT _ (TS _ 35) -> cont 35#;
	PT _ (TS _ 36) -> cont 36#;
	PT _ (TS _ 37) -> cont 37#;
	PT _ (TS _ 38) -> cont 38#;
	PT _ (TS _ 39) -> cont 39#;
	PT _ (TS _ 40) -> cont 40#;
	PT _ (TS _ 41) -> cont 41#;
	PT _ (TS _ 42) -> cont 42#;
	PT _ (TS _ 43) -> cont 43#;
	PT _ (TS _ 44) -> cont 44#;
	PT _ (TS _ 45) -> cont 45#;
	PT _ (TS _ 46) -> cont 46#;
	PT _ (TS _ 47) -> cont 47#;
	PT _ (TS _ 48) -> cont 48#;
	PT _ (TS _ 49) -> cont 49#;
	PT _ (TS _ 50) -> cont 50#;
	PT _ (TS _ 51) -> cont 51#;
	PT _ (TS _ 52) -> cont 52#;
	PT _ (TS _ 53) -> cont 53#;
	PT _ (TS _ 54) -> cont 54#;
	PT _ (TS _ 55) -> cont 55#;
	PT _ (TS _ 56) -> cont 56#;
	PT _ (TS _ 57) -> cont 57#;
	PT _ (TS _ 58) -> cont 58#;
	PT _ (TS _ 59) -> cont 59#;
	PT _ (TS _ 60) -> cont 60#;
	PT _ (TS _ 61) -> cont 61#;
	PT _ (TS _ 62) -> cont 62#;
	PT _ (TS _ 63) -> cont 63#;
	PT _ (TS _ 64) -> cont 64#;
	PT _ (TS _ 65) -> cont 65#;
	PT _ (TS _ 66) -> cont 66#;
	PT _ (TS _ 67) -> cont 67#;
	PT _ (TS _ 68) -> cont 68#;
	PT _ (TS _ 69) -> cont 69#;
	PT _ (TS _ 70) -> cont 70#;
	PT _ (TS _ 71) -> cont 71#;
	PT _ (TS _ 72) -> cont 72#;
	PT _ (TS _ 73) -> cont 73#;
	PT _ (TS _ 74) -> cont 74#;
	PT _ (TS _ 75) -> cont 75#;
	PT _ (TS _ 76) -> cont 76#;
	PT _ (TS _ 77) -> cont 77#;
	PT _ (TS _ 78) -> cont 78#;
	PT _ (TV happy_dollar_dollar) -> cont 79#;
	PT _ (TI happy_dollar_dollar) -> cont 80#;
	PT _ (TL happy_dollar_dollar) -> cont 81#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 82# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = ((>>=))
happyReturn :: () => a -> Err a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pContract tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {(HappyWrap42 x') = happyOut42 x} in x'))

pComponent tks = happySomeParser where
 happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (let {(HappyWrap43 x') = happyOut43 x} in x'))

pDefinition tks = happySomeParser where
 happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (let {(HappyWrap44 x') = happyOut44 x} in x'))

pSimpleDefinition tks = happySomeParser where
 happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (let {(HappyWrap45 x') = happyOut45 x} in x'))

pNumericalExpression tks = happySomeParser where
 happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (let {(HappyWrap46 x') = happyOut46 x} in x'))

pOperator tks = happySomeParser where
 happySomeParser = happyThen (happyParse 5# tks) (\x -> happyReturn (let {(HappyWrap47 x') = happyOut47 x} in x'))

pConditionalDefinition tks = happySomeParser where
 happySomeParser = happyThen (happyParse 6# tks) (\x -> happyReturn (let {(HappyWrap48 x') = happyOut48 x} in x'))

pStatement tks = happySomeParser where
 happySomeParser = happyThen (happyParse 7# tks) (\x -> happyReturn (let {(HappyWrap49 x') = happyOut49 x} in x'))

pConditionalStatement tks = happySomeParser where
 happySomeParser = happyThen (happyParse 8# tks) (\x -> happyReturn (let {(HappyWrap50 x') = happyOut50 x} in x'))

pSimpleStatement tks = happySomeParser where
 happySomeParser = happyThen (happyParse 9# tks) (\x -> happyReturn (let {(HappyWrap51 x') = happyOut51 x} in x'))

pCondition tks = happySomeParser where
 happySomeParser = happyThen (happyParse 10# tks) (\x -> happyReturn (let {(HappyWrap52 x') = happyOut52 x} in x'))

pSimpleCondition tks = happySomeParser where
 happySomeParser = happyThen (happyParse 11# tks) (\x -> happyReturn (let {(HappyWrap53 x') = happyOut53 x} in x'))

pBooleanExpression tks = happySomeParser where
 happySomeParser = happyThen (happyParse 12# tks) (\x -> happyReturn (let {(HappyWrap54 x') = happyOut54 x} in x'))

pID tks = happySomeParser where
 happySomeParser = happyThen (happyParse 13# tks) (\x -> happyReturn (let {(HappyWrap55 x') = happyOut55 x} in x'))

pHolds tks = happySomeParser where
 happySomeParser = happyThen (happyParse 14# tks) (\x -> happyReturn (let {(HappyWrap56 x') = happyOut56 x} in x'))

pSubject tks = happySomeParser where
 happySomeParser = happyThen (happyParse 15# tks) (\x -> happyReturn (let {(HappyWrap57 x') = happyOut57 x} in x'))

pVerb tks = happySomeParser where
 happySomeParser = happyThen (happyParse 16# tks) (\x -> happyReturn (let {(HappyWrap58 x') = happyOut58 x} in x'))

pVerbStatus tks = happySomeParser where
 happySomeParser = happyThen (happyParse 17# tks) (\x -> happyReturn (let {(HappyWrap59 x') = happyOut59 x} in x'))

pComparison tks = happySomeParser where
 happySomeParser = happyThen (happyParse 18# tks) (\x -> happyReturn (let {(HappyWrap60 x') = happyOut60 x} in x'))

pEqual tks = happySomeParser where
 happySomeParser = happyThen (happyParse 19# tks) (\x -> happyReturn (let {(HappyWrap61 x') = happyOut61 x} in x'))

pMore tks = happySomeParser where
 happySomeParser = happyThen (happyParse 20# tks) (\x -> happyReturn (let {(HappyWrap62 x') = happyOut62 x} in x'))

pModalVerb tks = happySomeParser where
 happySomeParser = happyThen (happyParse 21# tks) (\x -> happyReturn (let {(HappyWrap63 x') = happyOut63 x} in x'))

pObligation tks = happySomeParser where
 happySomeParser = happyThen (happyParse 22# tks) (\x -> happyReturn (let {(HappyWrap64 x') = happyOut64 x} in x'))

pDate tks = happySomeParser where
 happySomeParser = happyThen (happyParse 23# tks) (\x -> happyReturn (let {(HappyWrap65 x') = happyOut65 x} in x'))

pSpecificDate tks = happySomeParser where
 happySomeParser = happyThen (happyParse 24# tks) (\x -> happyReturn (let {(HappyWrap66 x') = happyOut66 x} in x'))

pTemporalQuantifier tks = happySomeParser where
 happySomeParser = happyThen (happyParse 25# tks) (\x -> happyReturn (let {(HappyWrap67 x') = happyOut67 x} in x'))

pTemporalOffset tks = happySomeParser where
 happySomeParser = happyThen (happyParse 26# tks) (\x -> happyReturn (let {(HappyWrap68 x') = happyOut68 x} in x'))

pMonth tks = happySomeParser where
 happySomeParser = happyThen (happyParse 27# tks) (\x -> happyReturn (let {(HappyWrap69 x') = happyOut69 x} in x'))

pObject tks = happySomeParser where
 happySomeParser = happyThen (happyParse 28# tks) (\x -> happyReturn (let {(HappyWrap70 x') = happyOut70 x} in x'))

pNumericalObject tks = happySomeParser where
 happySomeParser = happyThen (happyParse 29# tks) (\x -> happyReturn (let {(HappyWrap71 x') = happyOut71 x} in x'))

pPounds tks = happySomeParser where
 happySomeParser = happyThen (happyParse 30# tks) (\x -> happyReturn (let {(HappyWrap72 x') = happyOut72 x} in x'))

pDollars tks = happySomeParser where
 happySomeParser = happyThen (happyParse 31# tks) (\x -> happyReturn (let {(HappyWrap73 x') = happyOut73 x} in x'))

pEuros tks = happySomeParser where
 happySomeParser = happyThen (happyParse 32# tks) (\x -> happyReturn (let {(HappyWrap74 x') = happyOut74 x} in x'))

pNonNumericalObject tks = happySomeParser where
 happySomeParser = happyThen (happyParse 33# tks) (\x -> happyReturn (let {(HappyWrap75 x') = happyOut75 x} in x'))

pNum tks = happySomeParser where
 happySomeParser = happyThen (happyParse 34# tks) (\x -> happyReturn (let {(HappyWrap76 x') = happyOut76 x} in x'))

pReceiver tks = happySomeParser where
 happySomeParser = happyThen (happyParse 35# tks) (\x -> happyReturn (let {(HappyWrap77 x') = happyOut77 x} in x'))

happySeq = happyDontSeq


type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Prelude.Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Prelude.Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Prelude.Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif



















data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}
          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Prelude.Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}
                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else Prelude.False
         action
          | check     = indexShortOffAddr happyTable off_i
          | Prelude.otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `Prelude.mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)













-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ((Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
