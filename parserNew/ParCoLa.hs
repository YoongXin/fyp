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
newtype HappyWrap38 = HappyWrap38 (AbsCoLa.Ident)
happyIn38 :: (AbsCoLa.Ident) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap38 x)
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> HappyWrap38
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
newtype HappyWrap39 = HappyWrap39 (Integer)
happyIn39 :: (Integer) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap39 x)
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> HappyWrap39
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
newtype HappyWrap40 = HappyWrap40 (String)
happyIn40 :: (String) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap40 x)
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> HappyWrap40
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
newtype HappyWrap41 = HappyWrap41 (AbsCoLa.Contract)
happyIn41 :: (AbsCoLa.Contract) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap41 x)
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> HappyWrap41
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
newtype HappyWrap42 = HappyWrap42 (AbsCoLa.Component)
happyIn42 :: (AbsCoLa.Component) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap42 x)
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> HappyWrap42
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
newtype HappyWrap43 = HappyWrap43 (AbsCoLa.Definition)
happyIn43 :: (AbsCoLa.Definition) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap43 x)
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> HappyWrap43
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
newtype HappyWrap44 = HappyWrap44 (AbsCoLa.SimpleDefinition)
happyIn44 :: (AbsCoLa.SimpleDefinition) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap44 x)
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> HappyWrap44
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
newtype HappyWrap45 = HappyWrap45 (AbsCoLa.NumericalExpression)
happyIn45 :: (AbsCoLa.NumericalExpression) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap45 x)
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> HappyWrap45
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
newtype HappyWrap46 = HappyWrap46 (AbsCoLa.Operator)
happyIn46 :: (AbsCoLa.Operator) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap46 x)
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> HappyWrap46
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
newtype HappyWrap47 = HappyWrap47 (AbsCoLa.ConditionalDefinition)
happyIn47 :: (AbsCoLa.ConditionalDefinition) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap47 x)
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> HappyWrap47
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
newtype HappyWrap48 = HappyWrap48 (AbsCoLa.Statement)
happyIn48 :: (AbsCoLa.Statement) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap48 x)
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> HappyWrap48
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
newtype HappyWrap49 = HappyWrap49 (AbsCoLa.ConditionalStatement)
happyIn49 :: (AbsCoLa.ConditionalStatement) -> (HappyAbsSyn )
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap49 x)
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> HappyWrap49
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
newtype HappyWrap50 = HappyWrap50 (AbsCoLa.SimpleStatement)
happyIn50 :: (AbsCoLa.SimpleStatement) -> (HappyAbsSyn )
happyIn50 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap50 x)
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> HappyWrap50
happyOut50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut50 #-}
newtype HappyWrap51 = HappyWrap51 (AbsCoLa.Condition)
happyIn51 :: (AbsCoLa.Condition) -> (HappyAbsSyn )
happyIn51 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap51 x)
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> HappyWrap51
happyOut51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut51 #-}
newtype HappyWrap52 = HappyWrap52 (AbsCoLa.SimpleCondition)
happyIn52 :: (AbsCoLa.SimpleCondition) -> (HappyAbsSyn )
happyIn52 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap52 x)
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn ) -> HappyWrap52
happyOut52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut52 #-}
newtype HappyWrap53 = HappyWrap53 (AbsCoLa.BooleanExpression)
happyIn53 :: (AbsCoLa.BooleanExpression) -> (HappyAbsSyn )
happyIn53 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap53 x)
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn ) -> HappyWrap53
happyOut53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut53 #-}
newtype HappyWrap54 = HappyWrap54 (AbsCoLa.ID)
happyIn54 :: (AbsCoLa.ID) -> (HappyAbsSyn )
happyIn54 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap54 x)
{-# INLINE happyIn54 #-}
happyOut54 :: (HappyAbsSyn ) -> HappyWrap54
happyOut54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut54 #-}
newtype HappyWrap55 = HappyWrap55 (AbsCoLa.Holds)
happyIn55 :: (AbsCoLa.Holds) -> (HappyAbsSyn )
happyIn55 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap55 x)
{-# INLINE happyIn55 #-}
happyOut55 :: (HappyAbsSyn ) -> HappyWrap55
happyOut55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut55 #-}
newtype HappyWrap56 = HappyWrap56 (AbsCoLa.Subject)
happyIn56 :: (AbsCoLa.Subject) -> (HappyAbsSyn )
happyIn56 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap56 x)
{-# INLINE happyIn56 #-}
happyOut56 :: (HappyAbsSyn ) -> HappyWrap56
happyOut56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut56 #-}
newtype HappyWrap57 = HappyWrap57 (AbsCoLa.Verb)
happyIn57 :: (AbsCoLa.Verb) -> (HappyAbsSyn )
happyIn57 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap57 x)
{-# INLINE happyIn57 #-}
happyOut57 :: (HappyAbsSyn ) -> HappyWrap57
happyOut57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut57 #-}
newtype HappyWrap58 = HappyWrap58 (AbsCoLa.VerbStatus)
happyIn58 :: (AbsCoLa.VerbStatus) -> (HappyAbsSyn )
happyIn58 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap58 x)
{-# INLINE happyIn58 #-}
happyOut58 :: (HappyAbsSyn ) -> HappyWrap58
happyOut58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut58 #-}
newtype HappyWrap59 = HappyWrap59 (AbsCoLa.Comparison)
happyIn59 :: (AbsCoLa.Comparison) -> (HappyAbsSyn )
happyIn59 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap59 x)
{-# INLINE happyIn59 #-}
happyOut59 :: (HappyAbsSyn ) -> HappyWrap59
happyOut59 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut59 #-}
newtype HappyWrap60 = HappyWrap60 (AbsCoLa.Equal)
happyIn60 :: (AbsCoLa.Equal) -> (HappyAbsSyn )
happyIn60 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap60 x)
{-# INLINE happyIn60 #-}
happyOut60 :: (HappyAbsSyn ) -> HappyWrap60
happyOut60 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut60 #-}
newtype HappyWrap61 = HappyWrap61 (AbsCoLa.More)
happyIn61 :: (AbsCoLa.More) -> (HappyAbsSyn )
happyIn61 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap61 x)
{-# INLINE happyIn61 #-}
happyOut61 :: (HappyAbsSyn ) -> HappyWrap61
happyOut61 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut61 #-}
newtype HappyWrap62 = HappyWrap62 (AbsCoLa.ModalVerb)
happyIn62 :: (AbsCoLa.ModalVerb) -> (HappyAbsSyn )
happyIn62 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap62 x)
{-# INLINE happyIn62 #-}
happyOut62 :: (HappyAbsSyn ) -> HappyWrap62
happyOut62 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut62 #-}
newtype HappyWrap63 = HappyWrap63 (AbsCoLa.Obligation)
happyIn63 :: (AbsCoLa.Obligation) -> (HappyAbsSyn )
happyIn63 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap63 x)
{-# INLINE happyIn63 #-}
happyOut63 :: (HappyAbsSyn ) -> HappyWrap63
happyOut63 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut63 #-}
newtype HappyWrap64 = HappyWrap64 (AbsCoLa.Date)
happyIn64 :: (AbsCoLa.Date) -> (HappyAbsSyn )
happyIn64 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap64 x)
{-# INLINE happyIn64 #-}
happyOut64 :: (HappyAbsSyn ) -> HappyWrap64
happyOut64 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut64 #-}
newtype HappyWrap65 = HappyWrap65 (AbsCoLa.TemporalQuantifier)
happyIn65 :: (AbsCoLa.TemporalQuantifier) -> (HappyAbsSyn )
happyIn65 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap65 x)
{-# INLINE happyIn65 #-}
happyOut65 :: (HappyAbsSyn ) -> HappyWrap65
happyOut65 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut65 #-}
newtype HappyWrap66 = HappyWrap66 (AbsCoLa.TemporalOffset)
happyIn66 :: (AbsCoLa.TemporalOffset) -> (HappyAbsSyn )
happyIn66 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap66 x)
{-# INLINE happyIn66 #-}
happyOut66 :: (HappyAbsSyn ) -> HappyWrap66
happyOut66 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut66 #-}
newtype HappyWrap67 = HappyWrap67 (AbsCoLa.Month)
happyIn67 :: (AbsCoLa.Month) -> (HappyAbsSyn )
happyIn67 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap67 x)
{-# INLINE happyIn67 #-}
happyOut67 :: (HappyAbsSyn ) -> HappyWrap67
happyOut67 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut67 #-}
newtype HappyWrap68 = HappyWrap68 (AbsCoLa.Object)
happyIn68 :: (AbsCoLa.Object) -> (HappyAbsSyn )
happyIn68 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap68 x)
{-# INLINE happyIn68 #-}
happyOut68 :: (HappyAbsSyn ) -> HappyWrap68
happyOut68 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut68 #-}
newtype HappyWrap69 = HappyWrap69 (AbsCoLa.NumericalObject)
happyIn69 :: (AbsCoLa.NumericalObject) -> (HappyAbsSyn )
happyIn69 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap69 x)
{-# INLINE happyIn69 #-}
happyOut69 :: (HappyAbsSyn ) -> HappyWrap69
happyOut69 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut69 #-}
newtype HappyWrap70 = HappyWrap70 (AbsCoLa.Pounds)
happyIn70 :: (AbsCoLa.Pounds) -> (HappyAbsSyn )
happyIn70 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap70 x)
{-# INLINE happyIn70 #-}
happyOut70 :: (HappyAbsSyn ) -> HappyWrap70
happyOut70 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut70 #-}
newtype HappyWrap71 = HappyWrap71 (AbsCoLa.Dollars)
happyIn71 :: (AbsCoLa.Dollars) -> (HappyAbsSyn )
happyIn71 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap71 x)
{-# INLINE happyIn71 #-}
happyOut71 :: (HappyAbsSyn ) -> HappyWrap71
happyOut71 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut71 #-}
newtype HappyWrap72 = HappyWrap72 (AbsCoLa.Euros)
happyIn72 :: (AbsCoLa.Euros) -> (HappyAbsSyn )
happyIn72 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap72 x)
{-# INLINE happyIn72 #-}
happyOut72 :: (HappyAbsSyn ) -> HappyWrap72
happyOut72 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut72 #-}
newtype HappyWrap73 = HappyWrap73 (AbsCoLa.NonNumericalObject)
happyIn73 :: (AbsCoLa.NonNumericalObject) -> (HappyAbsSyn )
happyIn73 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap73 x)
{-# INLINE happyIn73 #-}
happyOut73 :: (HappyAbsSyn ) -> HappyWrap73
happyOut73 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut73 #-}
newtype HappyWrap74 = HappyWrap74 (AbsCoLa.Num)
happyIn74 :: (AbsCoLa.Num) -> (HappyAbsSyn )
happyIn74 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap74 x)
{-# INLINE happyIn74 #-}
happyOut74 :: (HappyAbsSyn ) -> HappyWrap74
happyOut74 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut74 #-}
newtype HappyWrap75 = HappyWrap75 (AbsCoLa.Receiver)
happyIn75 :: (AbsCoLa.Receiver) -> (HappyAbsSyn )
happyIn75 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap75 x)
{-# INLINE happyIn75 #-}
happyOut75 :: (HappyAbsSyn ) -> HappyWrap75
happyOut75 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut75 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x90\x05\x00\x02\x42\x00\x00\x10\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x10\x10\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x11\x00\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x02\x40\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x52\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x42\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x0a\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x23\xe2\x96\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x59\x80\xe4\x20\x04\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x90\x05\x00\x02\x42\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x20\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x42\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x48\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x00\xf0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x50\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x02\x40\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x80\x20\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x80\x20\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x10\x10\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x80\x20\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x20\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x10\x00\x00\x30\x22\xa4\xc6\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x90\x05\x00\x02\x42\x00\x00\x10\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x23\x42\x6a\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x20\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x22\xa4\xc6\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x52\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x22\x6e\x09\x02\xc0\x00\x00\x00\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x22\x6e\x09\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x22\x00\x44\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x59\x80\xe4\x20\x04\x2c\x05\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x11\x00\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x02\x40\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x23\x42\x6a\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x59\x80\xe4\x20\x04\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x11\x00\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x0a\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x90\x05\x00\x02\x42\x00\x00\x10\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa4\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x23\x42\x6a\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x0a\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x90\x05\x48\x0e\x42\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x01\x80\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa4\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x01\x80\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x10\x10\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x11\x00\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x59\x80\xe4\x20\x04\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x02\x40\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x90\x05\x48\x0e\x42\xc0\x52\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x01\x80\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x22\x00\x44\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x59\x80\xe4\x20\x04\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x90\x05\x48\x0e\x42\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x90\x05\x48\x0e\x42\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x59\x80\xe4\x20\x04\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x90\x05\x48\x0e\x42\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x90\x05\x48\x0e\x42\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x59\x80\xe4\x20\x04\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x90\x05\x48\x0e\x42\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x01\x80\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x90\x05\x48\x0e\x42\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x11\x00\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x59\x80\xe4\x20\x04\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x90\x05\x48\x0e\x42\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pContract","%start_pComponent","%start_pDefinition","%start_pSimpleDefinition","%start_pNumericalExpression","%start_pOperator","%start_pConditionalDefinition","%start_pStatement","%start_pConditionalStatement","%start_pSimpleStatement","%start_pCondition","%start_pSimpleCondition","%start_pBooleanExpression","%start_pID","%start_pHolds","%start_pSubject","%start_pVerb","%start_pVerbStatus","%start_pComparison","%start_pEqual","%start_pMore","%start_pModalVerb","%start_pObligation","%start_pDate","%start_pTemporalQuantifier","%start_pTemporalOffset","%start_pMonth","%start_pObject","%start_pNumericalObject","%start_pPounds","%start_pDollars","%start_pEuros","%start_pNonNumericalObject","%start_pNum","%start_pReceiver","Ident","Integer","String","Contract","Component","Definition","SimpleDefinition","NumericalExpression","Operator","ConditionalDefinition","Statement","ConditionalStatement","SimpleStatement","Condition","SimpleCondition","BooleanExpression","ID","Holds","Subject","Verb","VerbStatus","Comparison","Equal","More","ModalVerb","Obligation","Date","TemporalQuantifier","TemporalOffset","Month","Object","NumericalObject","Pounds","Dollars","Euros","NonNumericalObject","Num","Receiver","'('","')'","'AMOUNT'","'AND'","'ANYDATE'","'April'","'August'","'C-AND'","'DIVIDE'","'DOLLARS'","'December'","'EQUALS'","'EUR'","'EUROS'","'February'","'GBP'","'IF'","'IS'","'January'","'July'","'June'","'MINUS'","'March'","'May'","'NAMEDOBJECT'","'November'","'OR'","'OTHEROBJECT'","'October'","'PLUS'","'POUNDS'","'REPORT'","'SOMECURRENCY'","'SOMEDATE'","'September'","'THEDATE'","'THEN'","'TIMES'","'USD'","'['","']'","'after'","'before'","'buck'","'case'","'charge'","'charged'","'day'","'days'","'deliver'","'delivered'","'equal'","'equals'","'forbidden'","'greater'","'is'","'it'","'less'","'may'","'more'","'must'","'not'","'on'","'paid'","'pay'","'quid'","'refund'","'refunded'","'shall'","'than'","'that'","'the'","'to'","'week'","'weeks'","'year'","'years'","L_Ident","L_integ","L_quoted","%eof"]
        bit_start = st Prelude.* 156
        bit_end = (st Prelude.+ 1) Prelude.* 156
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..155]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x3e\x00\x3e\x00\xec\xff\xec\xff\xfe\xff\x82\x00\x52\x00\xec\xff\x57\x00\xec\xff\xec\xff\xec\xff\xaf\x00\xec\xff\xe2\xff\xaf\x00\xab\x00\xf8\x00\xb5\x01\xf1\x00\x72\x00\x5d\x01\xd0\xff\xa9\x00\x16\x01\x03\x00\xde\x00\x6e\x00\x86\x00\xf7\xff\x27\x00\x51\x01\x3d\x00\x03\x00\x0f\x00\x13\x00\x00\x00\x22\x00\xaf\x00\x00\x00\x22\x00\x00\x00\x22\x00\xaf\x00\xaf\x00\xaf\x00\xaf\x00\x22\x00\x00\x00\x00\x00\x22\x00\x00\x00\x00\x00\x00\x00\x22\x00\x00\x00\x00\x00\x00\x00\x22\x00\x15\x00\x15\x00\x15\x00\xaf\x00\x25\x00\x00\x00\x00\x00\x25\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x25\x00\xba\x00\x25\x00\x00\x00\x00\x00\x25\x00\xe1\xff\xff\xff\x25\x00\x00\x00\x00\x00\x25\x00\x00\x00\x43\x00\x00\x00\x30\x00\x65\x00\x70\x00\x4c\x00\x75\x00\x00\x00\x85\x00\x00\x00\x00\x00\x7b\x00\x89\x00\x00\x00\x00\x00\x00\x00\x00\x00\x89\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x89\x00\x00\x00\x89\x00\x95\x00\xa6\x00\xa5\x00\xb1\x00\xf8\x00\xb1\x00\x35\x00\xb1\x00\x0d\x00\xb1\x00\x35\x00\xf7\x00\xc5\x00\x7e\x00\x06\x01\xd8\x00\x10\x01\x2d\x01\xe6\x00\xaf\x00\x3b\x01\x15\x01\x00\x00\x00\x00\x00\x00\x00\x00\xf9\xff\x00\x00\x00\x00\x15\x01\x15\x01\x15\x01\x37\x01\x00\x00\x75\x01\x00\x00\x35\x00\x4f\x01\x42\x01\x93\x01\x73\x00\xb1\x01\x67\x00\x74\x00\xaf\x00\x7b\x01\x7b\x01\xfe\xff\xb8\x01\x0e\x00\xc0\x01\xe6\x01\xd3\x01\xd3\x01\xdf\x00\xd3\x01\xd3\x01\x00\x00\x67\x00\xdf\x00\xaf\x00\xb5\x01\x05\x00\x59\x00\x00\x00\x00\x00\x00\x00\x00\x00\xba\x01\x00\x00\xaf\x00\xaf\x00\xc8\x01\x50\x01\x20\x00\xaf\x00\xaf\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc8\x01\xef\x00\xde\x00\x00\x00\x00\x00\x00\x00\xcc\x01\x55\x02\xcd\x01\x00\x00\x5a\x01\xf8\x00\x3b\x00\x01\x01\xf9\x00\x00\x00\xdf\x00\x5a\x01\x00\x00\x00\x00\x6e\x00\x02\x01\x5d\x01\x00\x00\x00\x00\x5e\x02\x00\x00\xfe\xff\x5a\x01\x5e\x02\x82\x00\x00\x00\x00\x00\x5d\x01\xdf\x00\x5a\x01\x5e\x02\x00\x00\x00\x00\x00\x00\x5d\x01\x6e\x00\x02\x01\x5d\x01\x02\x01\x00\x00\x82\x00\x02\x01\x6e\x00\x41\x02\x17\x01\x3b\x00\x19\x01\x1d\x01\x6e\x00\x6e\x00\x41\x02\x6e\x00\x00\x00\x90\x02\x47\x02\x69\x02\x4b\x02\x5a\x01\x5a\x01\x00\x00\x00\x00\x00\x00\x00\x00\x5b\x02\x00\x00\x7c\x02\x53\x02\xa9\x00\x53\x02\x53\x02\x6e\x00\x6e\x00\x53\x02\x6e\x00\xa9\x00\x53\x02\x6e\x00\x6e\x00\x1f\x01\x6e\x00\x53\x02\x1f\x01\x6e\x00\xa9\x00\x53\x02\x6e\x00\x53\x02\x53\x02\xa9\x00\x00\x00\x53\x02\xa9\x00\x53\x02\x53\x02\x00\x00\xa9\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa9\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x53\x02\xa9\x00\x00\x00\x53\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xb7\x01\xce\x01\x39\x01\xf3\x00\x9b\x00\xa2\x02\xde\x01\xd5\x01\x06\x02\x7c\x00\xc1\x00\xdd\x01\xc9\x00\x9b\x02\x9c\x02\x10\x00\x99\x02\x9a\x02\xaf\x01\x9d\x02\x98\x02\x66\x01\x97\x02\x68\x01\x96\x02\x08\x00\x95\x02\x04\x02\x5d\x02\x94\x02\x9e\x02\x93\x02\x9f\x02\x09\x00\x91\x02\x00\x00\x00\x00\x00\x00\xca\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd0\x00\x52\x01\x53\x01\x56\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x12\x00\x17\x00\x18\x00\x67\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1b\x00\x00\x00\xa3\x02\x00\x00\xfe\x00\x00\x00\x00\x00\x00\x00\xd5\x00\x00\x00\x00\x00\x00\x00\x73\x02\x00\x00\x00\x00\x00\x00\x00\x00\x6a\x01\x77\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb0\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfa\x00\x7b\x02\x00\x00\x00\x00\xc5\x01\x00\x00\x20\x01\xdc\x01\x6b\x01\x7f\x02\x83\x02\xa3\x00\x00\x00\x00\x00\x49\x01\x00\x00\x09\x02\x0e\x02\xdc\x01\x87\x02\x8b\x02\x00\x00\x1b\x01\xe4\x01\x6e\x01\x88\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6f\x01\x72\x01\x1d\x00\xa0\x02\xa1\x02\x73\x01\x76\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x31\x00\x00\x00\xa4\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x41\x00\x00\x00\x87\x01\xa5\x02\x90\x01\xa7\x02\xa8\x02\x00\x00\xec\x01\x8a\x01\x00\x00\x00\x00\x0a\x02\xaa\x02\x7e\x01\x00\x00\x00\x00\x11\x02\x00\x00\xbf\x00\x8b\x01\x99\x01\xb8\x02\x00\x00\x00\x00\xa6\x01\xf4\x01\x8e\x01\xd6\x01\x00\x00\x00\x00\x00\x00\xb4\x01\x10\x02\xb1\x02\xda\x01\xb2\x02\x00\x00\xbb\x02\xb3\x02\x16\x02\xa6\x02\xb4\x02\x96\x01\xb6\x02\xb9\x02\x1c\x02\x22\x02\xa9\x02\x28\x02\x00\x00\x00\x00\x00\x00\x00\x00\x45\x00\x8f\x01\x92\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xab\x02\xdf\x01\xac\x02\xad\x02\x2e\x02\x34\x02\xae\x02\x3a\x02\xe7\x01\xaf\x02\x40\x02\x46\x02\xb7\x02\x4c\x02\xb5\x02\xbc\x02\x52\x02\xef\x01\xba\x02\x58\x02\xbd\x02\xbe\x02\x05\x02\x00\x00\xbf\x02\x86\x02\xc0\x02\xc1\x02\x00\x00\x89\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8c\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc2\x02\x8e\x02\x00\x00\xc3\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xd9\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdc\xff\x00\x00\x00\x00\x61\xff\x00\x00\xdb\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x67\xff\x66\xff\x00\x00\x69\xff\x6a\xff\x68\xff\x00\x00\x6d\xff\x6c\xff\x6b\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x73\xff\x72\xff\x00\x00\x7c\xff\x78\xff\x74\xff\x7e\xff\x7f\xff\x79\xff\x7a\xff\x7d\xff\x7b\xff\x75\xff\x76\xff\x77\xff\x00\x00\x00\x00\x00\x00\x87\xff\x86\xff\x00\x00\x00\x00\x00\x00\x00\x00\x91\xff\x92\xff\x00\x00\x95\xff\x00\x00\x94\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x99\xff\x00\x00\x9b\xff\x9a\xff\x00\x00\x00\x00\x9e\xff\xa0\xff\x9f\xff\x9d\xff\x00\x00\xa2\xff\xa4\xff\xa3\xff\xa1\xff\xa5\xff\xa6\xff\x00\x00\xda\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb8\xff\x00\x00\x00\x00\x00\x00\x00\x00\xc5\xff\x00\x00\x00\x00\x00\x00\xd2\xff\x00\x00\x00\x00\x00\x00\x00\x00\xc8\xff\xca\xff\xcb\xff\xc9\xff\x00\x00\xcd\xff\xce\xff\x00\x00\x00\x00\x00\x00\xd6\xff\xd5\xff\xd4\xff\xd3\xff\x00\x00\x00\x00\x00\x00\xd8\xff\xd9\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xac\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9c\xff\x98\xff\x97\xff\x96\xff\x00\x00\x8f\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x85\xff\x82\xff\x83\xff\x80\xff\x84\xff\x81\xff\x6e\xff\x6f\xff\x70\xff\x71\xff\x65\xff\x64\xff\x62\xff\x63\xff\x60\xff\x8a\xff\x8b\xff\x00\x00\x00\x00\x00\x00\x8d\xff\x8e\xff\x93\xff\x00\x00\x00\x00\x00\x00\xaa\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb1\xff\x00\x00\x00\x00\xb7\xff\xb6\xff\x00\x00\x00\x00\x00\x00\xc4\xff\xc3\xff\x00\x00\xd1\xff\x00\x00\x00\x00\x00\x00\xcc\xff\xc7\xff\xc2\xff\x00\x00\x00\x00\x00\x00\x00\x00\xd7\xff\xc6\xff\xc1\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd0\xff\xcf\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xab\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8c\xff\x88\xff\x89\xff\x90\xff\x00\x00\xa8\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb9\xff\x00\x00\x00\x00\x00\x00\x00\x00\xaf\xff\x00\x00\xb0\xff\xae\xff\xa9\xff\xa7\xff\xad\xff\xb4\xff\x00\x00\xb5\xff\xb3\xff\xbc\xff\xbb\xff\xba\xff\x00\x00\x00\x00\xbd\xff\x00\x00\xbe\xff\xc0\xff\xbf\xff\xb2\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x03\x00\x09\x00\x22\x00\x05\x00\x24\x00\x01\x00\x10\x00\x0a\x00\x01\x00\x01\x00\x0d\x00\x0e\x00\x3d\x00\x10\x00\x16\x00\x00\x00\x04\x00\x02\x00\x01\x00\x28\x00\x45\x00\x1f\x00\x1e\x00\x01\x00\x01\x00\x0c\x00\x39\x00\x01\x00\x1f\x00\x01\x00\x26\x00\x12\x00\x22\x00\x12\x00\x24\x00\x1c\x00\x27\x00\x06\x00\x07\x00\x1b\x00\x01\x00\x2c\x00\x0b\x00\x24\x00\x24\x00\x29\x00\x0f\x00\x4f\x00\x0a\x00\x01\x00\x13\x00\x14\x00\x15\x00\x24\x00\x17\x00\x18\x00\x42\x00\x1a\x00\x24\x00\x24\x00\x1d\x00\x03\x00\x24\x00\x42\x00\x24\x00\x01\x00\x23\x00\x1c\x00\x0a\x00\x01\x00\x48\x00\x0d\x00\x0e\x00\x51\x00\x10\x00\x24\x00\x4f\x00\x27\x00\x11\x00\x30\x00\x31\x00\x4f\x00\x2c\x00\x19\x00\x24\x00\x19\x00\x1c\x00\x49\x00\x1c\x00\x1f\x00\x20\x00\x21\x00\x20\x00\x21\x00\x2a\x00\x2b\x00\x4e\x00\x27\x00\x11\x00\x4f\x00\x24\x00\x28\x00\x2c\x00\x11\x00\x24\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x39\x00\x34\x00\x35\x00\x03\x00\x37\x00\x51\x00\x3f\x00\x3a\x00\x51\x00\x3c\x00\x0a\x00\x36\x00\x28\x00\x0d\x00\x0e\x00\x42\x00\x10\x00\x28\x00\x0c\x00\x51\x00\x04\x00\x4e\x00\x11\x00\x50\x00\x12\x00\x19\x00\x0c\x00\x03\x00\x1c\x00\x09\x00\x10\x00\x1f\x00\x20\x00\x21\x00\x0a\x00\x2a\x00\x2b\x00\x0d\x00\x0e\x00\x27\x00\x10\x00\x3e\x00\x16\x00\x1b\x00\x2c\x00\x28\x00\x01\x00\x51\x00\x2a\x00\x2b\x00\x1e\x00\x48\x00\x07\x00\x2f\x00\x01\x00\x1f\x00\x3f\x00\x33\x00\x26\x00\x37\x00\x07\x00\x46\x00\x38\x00\x27\x00\x3c\x00\x3b\x00\x42\x00\x3d\x00\x2c\x00\x3f\x00\x40\x00\x4e\x00\x46\x00\x50\x00\x44\x00\x45\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x49\x00\x24\x00\x01\x00\x46\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x07\x00\x24\x00\x42\x00\x00\x00\x00\x00\x02\x00\x02\x00\x38\x00\x0d\x00\x0e\x00\x00\x00\x10\x00\x02\x00\x2a\x00\x2b\x00\x00\x00\x51\x00\x02\x00\x0f\x00\x2e\x00\x51\x00\x12\x00\x12\x00\x32\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x12\x00\x24\x00\x06\x00\x07\x00\x11\x00\x12\x00\x3f\x00\x0b\x00\x30\x00\x31\x00\x41\x00\x0f\x00\x43\x00\x1a\x00\x1b\x00\x13\x00\x14\x00\x15\x00\x4f\x00\x17\x00\x18\x00\x51\x00\x1a\x00\x06\x00\x00\x00\x1d\x00\x02\x00\x4e\x00\x00\x00\x50\x00\x02\x00\x23\x00\x51\x00\x10\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x11\x00\x2a\x00\x2b\x00\x11\x00\x12\x00\x0f\x00\x2f\x00\x11\x00\x12\x00\x22\x00\x33\x00\x24\x00\x1a\x00\x1b\x00\x51\x00\x38\x00\x1a\x00\x1b\x00\x3b\x00\x00\x00\x3d\x00\x02\x00\x3f\x00\x40\x00\x00\x00\x11\x00\x02\x00\x44\x00\x45\x00\x34\x00\x35\x00\x2f\x00\x2f\x00\x51\x00\x0f\x00\x33\x00\x33\x00\x12\x00\x28\x00\x2e\x00\x2e\x00\x04\x00\x12\x00\x32\x00\x32\x00\x1a\x00\x1b\x00\x51\x00\x40\x00\x40\x00\x1a\x00\x1b\x00\x44\x00\x44\x00\x05\x00\x06\x00\x2a\x00\x2b\x00\x41\x00\x41\x00\x43\x00\x43\x00\x2f\x00\x2e\x00\x11\x00\x10\x00\x33\x00\x32\x00\x2f\x00\x2e\x00\x05\x00\x06\x00\x33\x00\x32\x00\x00\x00\x00\x00\x02\x00\x02\x00\x00\x00\x40\x00\x02\x00\x10\x00\x41\x00\x44\x00\x43\x00\x40\x00\x0d\x00\x0e\x00\x41\x00\x44\x00\x43\x00\x28\x00\x12\x00\x12\x00\x51\x00\x00\x00\x12\x00\x02\x00\x00\x00\x00\x00\x02\x00\x02\x00\x00\x00\x00\x00\x02\x00\x02\x00\x00\x00\x00\x00\x02\x00\x02\x00\x00\x00\x28\x00\x02\x00\x12\x00\x2a\x00\x2b\x00\x12\x00\x12\x00\x18\x00\x19\x00\x12\x00\x12\x00\x1a\x00\x1b\x00\x12\x00\x12\x00\x11\x00\x00\x00\x12\x00\x02\x00\x00\x00\x00\x00\x02\x00\x02\x00\x00\x00\x00\x00\x02\x00\x02\x00\x00\x00\x51\x00\x02\x00\x38\x00\x18\x00\x19\x00\x3b\x00\x12\x00\x3d\x00\x08\x00\x12\x00\x12\x00\x05\x00\x06\x00\x12\x00\x12\x00\x45\x00\x28\x00\x12\x00\x15\x00\x16\x00\x17\x00\x4e\x00\x10\x00\x50\x00\x15\x00\x16\x00\x17\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x03\x00\x04\x00\x05\x00\x06\x00\x18\x00\x19\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x15\x00\x16\x00\x17\x00\x10\x00\x03\x00\x04\x00\x05\x00\x06\x00\x18\x00\x19\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x04\x00\x05\x00\x06\x00\x10\x00\x25\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x05\x00\x06\x00\x25\x00\x10\x00\x0a\x00\x0a\x00\x0c\x00\x0c\x00\x05\x00\x06\x00\x10\x00\x10\x00\x09\x00\x28\x00\x34\x00\x35\x00\x0e\x00\x37\x00\x10\x00\x10\x00\x3a\x00\x14\x00\x3c\x00\x18\x00\x19\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x14\x00\x1a\x00\x1b\x00\x28\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x14\x00\x1a\x00\x1b\x00\x49\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x14\x00\x1a\x00\x1b\x00\x25\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x0a\x00\x0b\x00\x0c\x00\x0a\x00\x48\x00\x0c\x00\x10\x00\x4f\x00\x0a\x00\x10\x00\x0c\x00\x0a\x00\x4f\x00\x0c\x00\x10\x00\x1a\x00\x1b\x00\x10\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x1e\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x1f\x00\x20\x00\x21\x00\x22\x00\x0d\x00\x0e\x00\x2d\x00\x10\x00\x0d\x00\x0e\x00\x28\x00\x10\x00\x0d\x00\x0e\x00\x49\x00\x10\x00\x0d\x00\x0e\x00\x47\x00\x10\x00\x0d\x00\x0e\x00\x02\x00\x10\x00\x0d\x00\x0e\x00\x2d\x00\x10\x00\x0d\x00\x0e\x00\x4f\x00\x10\x00\x49\x00\x15\x00\x16\x00\x17\x00\x1a\x00\x1b\x00\x47\x00\x1a\x00\x1b\x00\x29\x00\x1a\x00\x1b\x00\x1a\x00\x1b\x00\x08\x00\x10\x00\x13\x00\x11\x00\x14\x00\x17\x00\x19\x00\x1b\x00\x1d\x00\x16\x00\x20\x00\x22\x00\x25\x00\x14\x00\x08\x00\x14\x00\x13\x00\x1b\x00\x14\x00\x13\x00\x1d\x00\x21\x00\x08\x00\x1d\x00\x23\x00\x08\x00\x13\x00\x13\x00\x13\x00\xff\xff\x14\x00\x13\x00\x13\x00\x25\x00\xff\xff\x14\x00\x25\x00\x13\x00\x25\x00\x25\x00\x25\x00\x25\x00\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\xff\xff\x25\x00\x25\x00\x25\x00\x25\x00\x25\x00\x25\x00\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x3f\x00\x8e\x00\xc3\x00\xbd\x00\xc4\x00\xde\x00\x38\x00\x34\x00\x27\x00\x27\x00\x31\x00\x32\x00\x59\x00\x38\x00\x8f\x00\x72\x00\xaf\x00\x73\x00\x27\x00\x7a\x00\x5a\x00\x39\x00\x90\x00\x27\x00\x27\x00\xf1\x00\x78\x00\x27\x00\x39\x00\x27\x00\x91\x00\xf2\x00\xbe\x00\x74\x00\xbf\x00\x4f\x00\x35\x00\x44\x00\x45\x00\xb0\x00\x27\x00\x36\x00\x46\x00\x50\x00\x28\x00\xdf\x00\x47\x00\x2a\x00\x34\x00\x27\x00\x48\x00\x49\x00\x4a\x00\xcd\x00\x4b\x00\x4c\x00\x3a\x00\x4d\x00\xcc\x00\xcb\x00\x4e\x00\x3f\x00\xb5\x00\x3a\x00\xd7\x00\x27\x00\x4f\x00\xc0\x00\x34\x00\x27\x00\xc0\x00\x31\x00\x32\x00\xff\xff\x38\x00\xc1\x00\x2a\x00\x35\x00\x9d\x00\xc5\x00\xc6\x00\x2a\x00\x36\x00\x2c\x00\x16\x01\x2c\x00\x2d\x00\x27\x00\x2d\x00\x39\x00\x2e\x00\x2f\x00\x2e\x00\x2f\x00\x53\x00\x54\x00\x25\x00\x35\x00\x8c\x00\x2a\x00\x10\x01\x7a\x00\x36\x00\x86\x00\x19\x01\xc7\x00\xc8\x00\xc9\x00\xca\x00\x78\x00\x63\x00\x64\x00\x3f\x00\x60\x00\xff\xff\x57\x00\x68\x00\xff\xff\x61\x00\x34\x00\xbc\x00\x7a\x00\x31\x00\x32\x00\x3a\x00\x38\x00\x7a\x00\xf1\x00\xff\xff\xac\x00\x25\x00\x9d\x00\x76\x00\xf2\x00\x2c\x00\x80\x00\x3f\x00\x2d\x00\x8e\x00\x81\x00\x39\x00\x2e\x00\x2f\x00\x34\x00\x53\x00\x54\x00\x31\x00\x32\x00\x35\x00\x38\x00\xdc\x00\x8f\x00\xad\x00\x36\x00\x7a\x00\x27\x00\xff\xff\x53\x00\x54\x00\x90\x00\xdd\x00\x91\x00\x6a\x00\x27\x00\x39\x00\x57\x00\x6b\x00\x91\x00\x60\x00\xf3\x00\xbb\x00\x5d\x00\x35\x00\x61\x00\x5e\x00\x3a\x00\x59\x00\x36\x00\x57\x00\x6c\x00\x25\x00\xba\x00\x76\x00\x6d\x00\x5a\x00\x92\x00\x3b\x00\x3c\x00\x3d\x00\xb9\x00\x93\x00\x27\x00\xb8\x00\x92\x00\x3b\x00\x3c\x00\x3d\x00\x03\x01\x93\x00\x3a\x00\x72\x00\x72\x00\x73\x00\x73\x00\xb7\x00\x7e\x00\x7f\x00\x72\x00\x7d\x00\x73\x00\x53\x00\x54\x00\x72\x00\xff\xff\x73\x00\x7a\x00\x6f\x00\xff\xff\x7b\x00\xd2\x00\x70\x00\x92\x00\x3b\x00\x3c\x00\x3d\x00\xd1\x00\x93\x00\x44\x00\x45\x00\xa1\x00\xad\x00\x57\x00\x46\x00\xc5\x00\xc6\x00\x71\x00\x47\x00\x72\x00\xa3\x00\x55\x00\x48\x00\x49\x00\x4a\x00\x2a\x00\x4b\x00\x4c\x00\xff\xff\x4d\x00\x94\x00\x72\x00\x4e\x00\x73\x00\x25\x00\x72\x00\x76\x00\x73\x00\x4f\x00\xff\xff\x8a\x00\xc7\x00\xc8\x00\xc9\x00\xca\x00\xa5\x00\x53\x00\x54\x00\xa1\x00\xa2\x00\xb0\x00\x6a\x00\xb1\x00\xb2\x00\x15\x01\x6b\x00\x16\x01\xa3\x00\x55\x00\xff\xff\x5d\x00\xb3\x00\x55\x00\x5e\x00\x72\x00\x59\x00\x73\x00\x57\x00\x6c\x00\x72\x00\xa6\x00\x73\x00\x6d\x00\x5a\x00\x63\x00\x64\x00\x6a\x00\x6a\x00\xff\xff\xe4\x00\x6b\x00\x6b\x00\xe5\x00\x7a\x00\x6f\x00\x6f\x00\xaa\x00\xf7\x00\x70\x00\x70\x00\xe6\x00\x55\x00\xff\xff\x6c\x00\x6c\x00\xf8\x00\x55\x00\x6d\x00\x6d\x00\x95\x00\x88\x00\x53\x00\x54\x00\x71\x00\x71\x00\x72\x00\x72\x00\x6a\x00\x6f\x00\xa6\x00\x8a\x00\x6b\x00\x70\x00\x6a\x00\x6f\x00\xef\x00\x88\x00\x6b\x00\x70\x00\x72\x00\x72\x00\x73\x00\x73\x00\x72\x00\x6c\x00\x73\x00\x8a\x00\x71\x00\x6d\x00\x72\x00\x6c\x00\x31\x00\x32\x00\x71\x00\x6d\x00\x72\x00\x7a\x00\xd0\x00\xcf\x00\xff\xff\x72\x00\xce\x00\x73\x00\x72\x00\x72\x00\x73\x00\x73\x00\x72\x00\x72\x00\x73\x00\x73\x00\x72\x00\x72\x00\x73\x00\x73\x00\x72\x00\x7a\x00\x73\x00\xca\x00\x53\x00\x54\x00\xa8\x00\xf6\x00\x5a\x00\x5b\x00\xe0\x00\xd9\x00\x54\x00\x55\x00\xd8\x00\xd4\x00\xa5\x00\x72\x00\xd3\x00\x73\x00\x72\x00\x72\x00\x73\x00\x73\x00\x72\x00\x72\x00\x73\x00\x73\x00\x72\x00\xff\xff\x73\x00\x5d\x00\x04\x01\x5b\x00\x5e\x00\x0f\x01\x59\x00\xa0\x00\x07\x01\x02\x01\xfb\x00\x88\x00\xfd\x00\x18\x01\x5a\x00\x7a\x00\x17\x01\xdf\x00\x65\x00\x66\x00\x25\x00\x8a\x00\x76\x00\xdf\x00\x65\x00\x66\x00\x0d\x01\x40\x00\x3b\x00\x3c\x00\x3d\x00\x41\x00\x23\x01\x40\x00\x3b\x00\x3c\x00\x3d\x00\x41\x00\x9d\x00\x9e\x00\x97\x00\x88\x00\x01\x01\x5b\x00\x98\x00\x99\x00\x9a\x00\x84\x00\x64\x00\x65\x00\x66\x00\x9b\x00\xfa\x00\x9e\x00\x97\x00\x88\x00\x2c\x01\x5b\x00\x98\x00\x99\x00\x9a\x00\x84\x00\x96\x00\x97\x00\x88\x00\x9b\x00\xfa\x00\x98\x00\x99\x00\x9a\x00\x84\x00\xfb\x00\x88\x00\xf3\x00\x9b\x00\x86\x00\xfc\x00\x84\x00\x84\x00\x87\x00\x88\x00\x81\x00\x9b\x00\x89\x00\x7a\x00\x63\x00\x64\x00\x7c\x00\x60\x00\x7d\x00\x8a\x00\x68\x00\xe9\x00\x61\x00\x29\x01\x5b\x00\xea\x00\x5b\x00\xeb\x00\x55\x00\xe1\x00\x3b\x01\x55\x00\x7a\x00\xe2\x00\x5b\x00\xe3\x00\x55\x00\x08\x01\x34\x01\x55\x00\xdb\x00\x09\x01\x5b\x00\x0a\x01\x55\x00\xfe\x00\x49\x01\x55\x00\xef\x00\xff\x00\x5b\x00\x00\x01\x55\x00\x82\x00\x83\x00\x84\x00\xed\x00\x13\x01\x84\x00\x81\x00\x2a\x00\xec\x00\x81\x00\x84\x00\xfc\x00\x2a\x00\x84\x00\x81\x00\x44\x01\x55\x00\x81\x00\x3f\x00\x40\x00\x3b\x00\x3c\x00\x3d\x00\x41\x00\x06\x01\x40\x00\x3b\x00\x3c\x00\x3d\x00\x41\x00\x2b\x01\x40\x00\x3b\x00\x3c\x00\x3d\x00\x41\x00\x26\x01\x40\x00\x3b\x00\x3c\x00\x3d\x00\x41\x00\x20\x01\x40\x00\x3b\x00\x3c\x00\x3d\x00\x41\x00\x1f\x01\x40\x00\x3b\x00\x3c\x00\x3d\x00\x41\x00\x1d\x01\x40\x00\x3b\x00\x3c\x00\x3d\x00\x41\x00\x38\x01\x40\x00\x3b\x00\x3c\x00\x3d\x00\x41\x00\x37\x01\x40\x00\x3b\x00\x3c\x00\x3d\x00\x41\x00\x35\x01\x40\x00\x3b\x00\x3c\x00\x3d\x00\x41\x00\x32\x01\x40\x00\x3b\x00\x3c\x00\x3d\x00\x41\x00\x31\x01\x40\x00\x3b\x00\x3c\x00\x3d\x00\x41\x00\x2f\x01\x40\x00\x3b\x00\x3c\x00\x3d\x00\x41\x00\x4a\x01\x40\x00\x3b\x00\x3c\x00\x3d\x00\x41\x00\x47\x01\x40\x00\x3b\x00\x3c\x00\x3d\x00\x41\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\xaa\x00\x7f\x00\x12\x01\x7d\x00\xa7\x00\x7f\x00\x7a\x00\x7d\x00\xa0\x00\x7f\x00\x27\x00\x7d\x00\xf5\x00\x7f\x00\x1c\x01\x7d\x00\xf4\x00\x7f\x00\x1d\x01\x7d\x00\xe8\x00\x7f\x00\x1b\x01\x7d\x00\xe7\x00\x7f\x00\x2a\x00\x7d\x00\x27\x00\xdf\x00\x65\x00\x66\x00\x42\x01\x55\x00\x3f\x01\x3f\x01\x55\x00\x3e\x01\x4e\x01\x55\x00\x4c\x01\x55\x00\x8c\x00\x78\x00\x6d\x00\x76\x00\x68\x00\x5e\x00\x57\x00\x51\x00\x42\x00\x61\x00\x36\x00\x2f\x00\x25\x00\xb4\x00\xa6\x00\x0e\x01\x0c\x01\xd6\x00\x0b\x01\x05\x01\xd5\x00\x32\x00\xa6\x00\x13\x01\x2a\x00\xa6\x00\x2a\x01\x28\x01\x27\x01\x00\x00\x24\x01\x22\x01\x30\x01\x25\x01\x00\x00\x21\x01\x1e\x01\x2d\x01\x3c\x01\x3a\x01\x39\x01\x36\x01\x33\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2e\x01\x00\x00\x00\x00\x00\x00\x00\x00\x48\x01\x00\x00\x00\x00\x46\x01\x45\x01\x43\x01\x41\x01\x40\x01\x4d\x01\x4b\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (35, 159) [
	(35 , happyReduce_35),
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
	(159 , happyReduce_159)
	]

happy_n_terms = 82 :: Prelude.Int
happy_n_nonterms = 38 :: Prelude.Int

happyReduce_35 = happySpecReduce_1  0# happyReduction_35
happyReduction_35 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TV happy_var_1)) -> 
	happyIn38
		 (AbsCoLa.Ident happy_var_1
	)}

happyReduce_36 = happySpecReduce_1  1# happyReduction_36
happyReduction_36 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TI happy_var_1)) -> 
	happyIn39
		 ((read happy_var_1) :: Integer
	)}

happyReduce_37 = happySpecReduce_1  2# happyReduction_37
happyReduction_37 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TL happy_var_1)) -> 
	happyIn40
		 (happy_var_1
	)}

happyReduce_38 = happySpecReduce_0  3# happyReduction_38
happyReduction_38  =  happyIn41
		 (AbsCoLa.ConEmpty
	)

happyReduce_39 = happySpecReduce_1  3# happyReduction_39
happyReduction_39 happy_x_1
	 =  case happyOut42 happy_x_1 of { (HappyWrap42 happy_var_1) -> 
	happyIn41
		 (AbsCoLa.ConComp happy_var_1
	)}

happyReduce_40 = happySpecReduce_3  3# happyReduction_40
happyReduction_40 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_1 of { (HappyWrap42 happy_var_1) -> 
	case happyOut41 happy_x_3 of { (HappyWrap41 happy_var_3) -> 
	happyIn41
		 (AbsCoLa.ConAnd happy_var_1 happy_var_3
	)}}

happyReduce_41 = happySpecReduce_1  4# happyReduction_41
happyReduction_41 happy_x_1
	 =  case happyOut43 happy_x_1 of { (HappyWrap43 happy_var_1) -> 
	happyIn42
		 (AbsCoLa.ComDef happy_var_1
	)}

happyReduce_42 = happySpecReduce_1  4# happyReduction_42
happyReduction_42 happy_x_1
	 =  case happyOut47 happy_x_1 of { (HappyWrap47 happy_var_1) -> 
	happyIn42
		 (AbsCoLa.ComConDef happy_var_1
	)}

happyReduce_43 = happySpecReduce_1  4# happyReduction_43
happyReduction_43 happy_x_1
	 =  case happyOut48 happy_x_1 of { (HappyWrap48 happy_var_1) -> 
	happyIn42
		 (AbsCoLa.ComState happy_var_1
	)}

happyReduce_44 = happySpecReduce_1  4# happyReduction_44
happyReduction_44 happy_x_1
	 =  case happyOut49 happy_x_1 of { (HappyWrap49 happy_var_1) -> 
	happyIn42
		 (AbsCoLa.ComConState happy_var_1
	)}

happyReduce_45 = happySpecReduce_1  5# happyReduction_45
happyReduction_45 happy_x_1
	 =  case happyOut44 happy_x_1 of { (HappyWrap44 happy_var_1) -> 
	happyIn43
		 (AbsCoLa.DefSim happy_var_1
	)}

happyReduce_46 = happySpecReduce_3  5# happyReduction_46
happyReduction_46 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut44 happy_x_1 of { (HappyWrap44 happy_var_1) -> 
	case happyOut43 happy_x_3 of { (HappyWrap43 happy_var_3) -> 
	happyIn43
		 (AbsCoLa.DefAnd happy_var_1 happy_var_3
	)}}

happyReduce_47 = happyReduce 4# 6# happyReduction_47
happyReduction_47 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	case happyOut56 happy_x_4 of { (HappyWrap56 happy_var_4) -> 
	happyIn44
		 (AbsCoLa.SimDefIs happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_48 = happyReduce 4# 6# happyReduction_48
happyReduction_48 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	case happyOut45 happy_x_4 of { (HappyWrap45 happy_var_4) -> 
	happyIn44
		 (AbsCoLa.SimDefEq happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_49 = happySpecReduce_1  7# happyReduction_49
happyReduction_49 happy_x_1
	 =  case happyOut74 happy_x_1 of { (HappyWrap74 happy_var_1) -> 
	happyIn45
		 (AbsCoLa.NumExpNum happy_var_1
	)}

happyReduce_50 = happySpecReduce_1  7# happyReduction_50
happyReduction_50 happy_x_1
	 =  case happyOut69 happy_x_1 of { (HappyWrap69 happy_var_1) -> 
	happyIn45
		 (AbsCoLa.NumExpObj happy_var_1
	)}

happyReduce_51 = happySpecReduce_3  7# happyReduction_51
happyReduction_51 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_1 of { (HappyWrap45 happy_var_1) -> 
	case happyOut46 happy_x_2 of { (HappyWrap46 happy_var_2) -> 
	case happyOut45 happy_x_3 of { (HappyWrap45 happy_var_3) -> 
	happyIn45
		 (AbsCoLa.NumExpOp happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_52 = happySpecReduce_1  8# happyReduction_52
happyReduction_52 happy_x_1
	 =  happyIn46
		 (AbsCoLa.OpPlus
	)

happyReduce_53 = happySpecReduce_1  8# happyReduction_53
happyReduction_53 happy_x_1
	 =  happyIn46
		 (AbsCoLa.OpMin
	)

happyReduce_54 = happySpecReduce_1  8# happyReduction_54
happyReduction_54 happy_x_1
	 =  happyIn46
		 (AbsCoLa.OpMult
	)

happyReduce_55 = happySpecReduce_1  8# happyReduction_55
happyReduction_55 happy_x_1
	 =  happyIn46
		 (AbsCoLa.OpDiv
	)

happyReduce_56 = happySpecReduce_3  9# happyReduction_56
happyReduction_56 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { (HappyWrap43 happy_var_1) -> 
	case happyOut51 happy_x_3 of { (HappyWrap51 happy_var_3) -> 
	happyIn47
		 (AbsCoLa.ConDefIf happy_var_1 happy_var_3
	)}}

happyReduce_57 = happyReduce 4# 9# happyReduction_57
happyReduction_57 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut51 happy_x_2 of { (HappyWrap51 happy_var_2) -> 
	case happyOut43 happy_x_4 of { (HappyWrap43 happy_var_4) -> 
	happyIn47
		 (AbsCoLa.ConDefIfThen happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_58 = happySpecReduce_1  10# happyReduction_58
happyReduction_58 happy_x_1
	 =  case happyOut50 happy_x_1 of { (HappyWrap50 happy_var_1) -> 
	happyIn48
		 (AbsCoLa.StateSim happy_var_1
	)}

happyReduce_59 = happySpecReduce_3  10# happyReduction_59
happyReduction_59 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut50 happy_x_1 of { (HappyWrap50 happy_var_1) -> 
	case happyOut48 happy_x_3 of { (HappyWrap48 happy_var_3) -> 
	happyIn48
		 (AbsCoLa.StateOr happy_var_1 happy_var_3
	)}}

happyReduce_60 = happySpecReduce_3  10# happyReduction_60
happyReduction_60 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut50 happy_x_1 of { (HappyWrap50 happy_var_1) -> 
	case happyOut48 happy_x_3 of { (HappyWrap48 happy_var_3) -> 
	happyIn48
		 (AbsCoLa.StateAnd happy_var_1 happy_var_3
	)}}

happyReduce_61 = happySpecReduce_3  11# happyReduction_61
happyReduction_61 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_1 of { (HappyWrap48 happy_var_1) -> 
	case happyOut51 happy_x_3 of { (HappyWrap51 happy_var_3) -> 
	happyIn49
		 (AbsCoLa.ConStateIf happy_var_1 happy_var_3
	)}}

happyReduce_62 = happyReduce 4# 11# happyReduction_62
happyReduction_62 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut51 happy_x_2 of { (HappyWrap51 happy_var_2) -> 
	case happyOut48 happy_x_4 of { (HappyWrap48 happy_var_4) -> 
	happyIn49
		 (AbsCoLa.ConStateIfThen happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_63 = happyReduce 8# 12# happyReduction_63
happyReduction_63 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	case happyOut55 happy_x_2 of { (HappyWrap55 happy_var_2) -> 
	case happyOut56 happy_x_3 of { (HappyWrap56 happy_var_3) -> 
	case happyOut62 happy_x_4 of { (HappyWrap62 happy_var_4) -> 
	case happyOut57 happy_x_5 of { (HappyWrap57 happy_var_5) -> 
	case happyOut68 happy_x_6 of { (HappyWrap68 happy_var_6) -> 
	case happyOut75 happy_x_7 of { (HappyWrap75 happy_var_7) -> 
	case happyOut64 happy_x_8 of { (HappyWrap64 happy_var_8) -> 
	happyIn50
		 (AbsCoLa.SimStateOne happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6 happy_var_7 happy_var_8
	) `HappyStk` happyRest}}}}}}}}

happyReduce_64 = happyReduce 8# 12# happyReduction_64
happyReduction_64 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	case happyOut55 happy_x_2 of { (HappyWrap55 happy_var_2) -> 
	case happyOut56 happy_x_3 of { (HappyWrap56 happy_var_3) -> 
	case happyOut64 happy_x_4 of { (HappyWrap64 happy_var_4) -> 
	case happyOut62 happy_x_5 of { (HappyWrap62 happy_var_5) -> 
	case happyOut57 happy_x_6 of { (HappyWrap57 happy_var_6) -> 
	case happyOut68 happy_x_7 of { (HappyWrap68 happy_var_7) -> 
	case happyOut75 happy_x_8 of { (HappyWrap75 happy_var_8) -> 
	happyIn50
		 (AbsCoLa.SimStateTwo happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6 happy_var_7 happy_var_8
	) `HappyStk` happyRest}}}}}}}}

happyReduce_65 = happyReduce 8# 12# happyReduction_65
happyReduction_65 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	case happyOut55 happy_x_2 of { (HappyWrap55 happy_var_2) -> 
	case happyOut64 happy_x_3 of { (HappyWrap64 happy_var_3) -> 
	case happyOut56 happy_x_4 of { (HappyWrap56 happy_var_4) -> 
	case happyOut62 happy_x_5 of { (HappyWrap62 happy_var_5) -> 
	case happyOut57 happy_x_6 of { (HappyWrap57 happy_var_6) -> 
	case happyOut68 happy_x_7 of { (HappyWrap68 happy_var_7) -> 
	case happyOut75 happy_x_8 of { (HappyWrap75 happy_var_8) -> 
	happyIn50
		 (AbsCoLa.SimStateThree happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6 happy_var_7 happy_var_8
	) `HappyStk` happyRest}}}}}}}}

happyReduce_66 = happyReduce 7# 12# happyReduction_66
happyReduction_66 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	case happyOut55 happy_x_2 of { (HappyWrap55 happy_var_2) -> 
	case happyOut56 happy_x_3 of { (HappyWrap56 happy_var_3) -> 
	case happyOut58 happy_x_4 of { (HappyWrap58 happy_var_4) -> 
	case happyOut68 happy_x_5 of { (HappyWrap68 happy_var_5) -> 
	case happyOut75 happy_x_6 of { (HappyWrap75 happy_var_6) -> 
	case happyOut64 happy_x_7 of { (HappyWrap64 happy_var_7) -> 
	happyIn50
		 (AbsCoLa.SimStateFour happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6 happy_var_7
	) `HappyStk` happyRest}}}}}}}

happyReduce_67 = happyReduce 7# 12# happyReduction_67
happyReduction_67 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	case happyOut62 happy_x_3 of { (HappyWrap62 happy_var_3) -> 
	case happyOut57 happy_x_4 of { (HappyWrap57 happy_var_4) -> 
	case happyOut68 happy_x_5 of { (HappyWrap68 happy_var_5) -> 
	case happyOut75 happy_x_6 of { (HappyWrap75 happy_var_6) -> 
	case happyOut64 happy_x_7 of { (HappyWrap64 happy_var_7) -> 
	happyIn50
		 (AbsCoLa.SimStateOneNH happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6 happy_var_7
	) `HappyStk` happyRest}}}}}}}

happyReduce_68 = happyReduce 7# 12# happyReduction_68
happyReduction_68 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	case happyOut64 happy_x_3 of { (HappyWrap64 happy_var_3) -> 
	case happyOut62 happy_x_4 of { (HappyWrap62 happy_var_4) -> 
	case happyOut57 happy_x_5 of { (HappyWrap57 happy_var_5) -> 
	case happyOut68 happy_x_6 of { (HappyWrap68 happy_var_6) -> 
	case happyOut75 happy_x_7 of { (HappyWrap75 happy_var_7) -> 
	happyIn50
		 (AbsCoLa.SimStateTwoNH happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6 happy_var_7
	) `HappyStk` happyRest}}}}}}}

happyReduce_69 = happyReduce 7# 12# happyReduction_69
happyReduction_69 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	case happyOut64 happy_x_2 of { (HappyWrap64 happy_var_2) -> 
	case happyOut56 happy_x_3 of { (HappyWrap56 happy_var_3) -> 
	case happyOut62 happy_x_4 of { (HappyWrap62 happy_var_4) -> 
	case happyOut57 happy_x_5 of { (HappyWrap57 happy_var_5) -> 
	case happyOut68 happy_x_6 of { (HappyWrap68 happy_var_6) -> 
	case happyOut75 happy_x_7 of { (HappyWrap75 happy_var_7) -> 
	happyIn50
		 (AbsCoLa.SimStateThreeNH happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6 happy_var_7
	) `HappyStk` happyRest}}}}}}}

happyReduce_70 = happyReduce 6# 12# happyReduction_70
happyReduction_70 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	case happyOut58 happy_x_3 of { (HappyWrap58 happy_var_3) -> 
	case happyOut68 happy_x_4 of { (HappyWrap68 happy_var_4) -> 
	case happyOut75 happy_x_5 of { (HappyWrap75 happy_var_5) -> 
	case happyOut64 happy_x_6 of { (HappyWrap64 happy_var_6) -> 
	happyIn50
		 (AbsCoLa.SimStateFourNH happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6
	) `HappyStk` happyRest}}}}}}

happyReduce_71 = happySpecReduce_1  13# happyReduction_71
happyReduction_71 happy_x_1
	 =  case happyOut52 happy_x_1 of { (HappyWrap52 happy_var_1) -> 
	happyIn51
		 (AbsCoLa.CondiSim happy_var_1
	)}

happyReduce_72 = happySpecReduce_3  13# happyReduction_72
happyReduction_72 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut52 happy_x_1 of { (HappyWrap52 happy_var_1) -> 
	case happyOut51 happy_x_3 of { (HappyWrap51 happy_var_3) -> 
	happyIn51
		 (AbsCoLa.CondiOr happy_var_1 happy_var_3
	)}}

happyReduce_73 = happySpecReduce_3  13# happyReduction_73
happyReduction_73 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut52 happy_x_1 of { (HappyWrap52 happy_var_1) -> 
	case happyOut51 happy_x_3 of { (HappyWrap51 happy_var_3) -> 
	happyIn51
		 (AbsCoLa.CondiAnd happy_var_1 happy_var_3
	)}}

happyReduce_74 = happyReduce 7# 14# happyReduction_74
happyReduction_74 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	case happyOut55 happy_x_2 of { (HappyWrap55 happy_var_2) -> 
	case happyOut56 happy_x_3 of { (HappyWrap56 happy_var_3) -> 
	case happyOut58 happy_x_4 of { (HappyWrap58 happy_var_4) -> 
	case happyOut68 happy_x_5 of { (HappyWrap68 happy_var_5) -> 
	case happyOut75 happy_x_6 of { (HappyWrap75 happy_var_6) -> 
	case happyOut64 happy_x_7 of { (HappyWrap64 happy_var_7) -> 
	happyIn52
		 (AbsCoLa.SimConOne happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6 happy_var_7
	) `HappyStk` happyRest}}}}}}}

happyReduce_75 = happyReduce 7# 14# happyReduction_75
happyReduction_75 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	case happyOut55 happy_x_2 of { (HappyWrap55 happy_var_2) -> 
	case happyOut56 happy_x_3 of { (HappyWrap56 happy_var_3) -> 
	case happyOut64 happy_x_4 of { (HappyWrap64 happy_var_4) -> 
	case happyOut58 happy_x_5 of { (HappyWrap58 happy_var_5) -> 
	case happyOut68 happy_x_6 of { (HappyWrap68 happy_var_6) -> 
	case happyOut75 happy_x_7 of { (HappyWrap75 happy_var_7) -> 
	happyIn52
		 (AbsCoLa.SimConTwo happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6 happy_var_7
	) `HappyStk` happyRest}}}}}}}

happyReduce_76 = happyReduce 7# 14# happyReduction_76
happyReduction_76 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	case happyOut55 happy_x_2 of { (HappyWrap55 happy_var_2) -> 
	case happyOut64 happy_x_3 of { (HappyWrap64 happy_var_3) -> 
	case happyOut56 happy_x_4 of { (HappyWrap56 happy_var_4) -> 
	case happyOut58 happy_x_5 of { (HappyWrap58 happy_var_5) -> 
	case happyOut68 happy_x_6 of { (HappyWrap68 happy_var_6) -> 
	case happyOut75 happy_x_7 of { (HappyWrap75 happy_var_7) -> 
	happyIn52
		 (AbsCoLa.SimConThree happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6 happy_var_7
	) `HappyStk` happyRest}}}}}}}

happyReduce_77 = happyReduce 8# 14# happyReduction_77
happyReduction_77 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	case happyOut55 happy_x_2 of { (HappyWrap55 happy_var_2) -> 
	case happyOut56 happy_x_3 of { (HappyWrap56 happy_var_3) -> 
	case happyOut62 happy_x_4 of { (HappyWrap62 happy_var_4) -> 
	case happyOut57 happy_x_5 of { (HappyWrap57 happy_var_5) -> 
	case happyOut68 happy_x_6 of { (HappyWrap68 happy_var_6) -> 
	case happyOut75 happy_x_7 of { (HappyWrap75 happy_var_7) -> 
	case happyOut64 happy_x_8 of { (HappyWrap64 happy_var_8) -> 
	happyIn52
		 (AbsCoLa.SimConFour happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6 happy_var_7 happy_var_8
	) `HappyStk` happyRest}}}}}}}}

happyReduce_78 = happySpecReduce_3  14# happyReduction_78
happyReduction_78 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	case happyOut55 happy_x_2 of { (HappyWrap55 happy_var_2) -> 
	case happyOut53 happy_x_3 of { (HappyWrap53 happy_var_3) -> 
	happyIn52
		 (AbsCoLa.SimConFive happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_79 = happyReduce 6# 14# happyReduction_79
happyReduction_79 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	case happyOut58 happy_x_3 of { (HappyWrap58 happy_var_3) -> 
	case happyOut68 happy_x_4 of { (HappyWrap68 happy_var_4) -> 
	case happyOut75 happy_x_5 of { (HappyWrap75 happy_var_5) -> 
	case happyOut64 happy_x_6 of { (HappyWrap64 happy_var_6) -> 
	happyIn52
		 (AbsCoLa.SimConOneNH happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6
	) `HappyStk` happyRest}}}}}}

happyReduce_80 = happyReduce 6# 14# happyReduction_80
happyReduction_80 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	case happyOut64 happy_x_3 of { (HappyWrap64 happy_var_3) -> 
	case happyOut58 happy_x_4 of { (HappyWrap58 happy_var_4) -> 
	case happyOut68 happy_x_5 of { (HappyWrap68 happy_var_5) -> 
	case happyOut75 happy_x_6 of { (HappyWrap75 happy_var_6) -> 
	happyIn52
		 (AbsCoLa.SimConTwoNH happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6
	) `HappyStk` happyRest}}}}}}

happyReduce_81 = happyReduce 6# 14# happyReduction_81
happyReduction_81 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	case happyOut64 happy_x_2 of { (HappyWrap64 happy_var_2) -> 
	case happyOut56 happy_x_3 of { (HappyWrap56 happy_var_3) -> 
	case happyOut58 happy_x_4 of { (HappyWrap58 happy_var_4) -> 
	case happyOut68 happy_x_5 of { (HappyWrap68 happy_var_5) -> 
	case happyOut75 happy_x_6 of { (HappyWrap75 happy_var_6) -> 
	happyIn52
		 (AbsCoLa.SimConThreeNH happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6
	) `HappyStk` happyRest}}}}}}

happyReduce_82 = happyReduce 7# 14# happyReduction_82
happyReduction_82 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	case happyOut62 happy_x_3 of { (HappyWrap62 happy_var_3) -> 
	case happyOut57 happy_x_4 of { (HappyWrap57 happy_var_4) -> 
	case happyOut68 happy_x_5 of { (HappyWrap68 happy_var_5) -> 
	case happyOut75 happy_x_6 of { (HappyWrap75 happy_var_6) -> 
	case happyOut64 happy_x_7 of { (HappyWrap64 happy_var_7) -> 
	happyIn52
		 (AbsCoLa.SimConFourNH happy_var_1 happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6 happy_var_7
	) `HappyStk` happyRest}}}}}}}

happyReduce_83 = happySpecReduce_2  14# happyReduction_83
happyReduction_83 happy_x_2
	happy_x_1
	 =  case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	case happyOut53 happy_x_2 of { (HappyWrap53 happy_var_2) -> 
	happyIn52
		 (AbsCoLa.SimConFiveNH happy_var_1 happy_var_2
	)}}

happyReduce_84 = happyReduce 4# 15# happyReduction_84
happyReduction_84 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut56 happy_x_1 of { (HappyWrap56 happy_var_1) -> 
	case happyOut58 happy_x_2 of { (HappyWrap58 happy_var_2) -> 
	case happyOut59 happy_x_3 of { (HappyWrap59 happy_var_3) -> 
	case happyOut56 happy_x_4 of { (HappyWrap56 happy_var_4) -> 
	happyIn53
		 (AbsCoLa.BoolEx happy_var_1 happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}}

happyReduce_85 = happySpecReduce_3  16# happyReduction_85
happyReduction_85 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut74 happy_x_2 of { (HappyWrap74 happy_var_2) -> 
	happyIn54
		 (AbsCoLa.IDSim happy_var_2
	)}

happyReduce_86 = happyReduce 6# 16# happyReduction_86
happyReduction_86 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut74 happy_x_2 of { (HappyWrap74 happy_var_2) -> 
	case happyOut74 happy_x_4 of { (HappyWrap74 happy_var_4) -> 
	happyIn54
		 (AbsCoLa.IDRep happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_87 = happyReduce 5# 17# happyReduction_87
happyReduction_87 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = happyIn55
		 (AbsCoLa.HoldYes
	) `HappyStk` happyRest

happyReduce_88 = happyReduce 6# 17# happyReduction_88
happyReduction_88 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = happyIn55
		 (AbsCoLa.HoldNo
	) `HappyStk` happyRest

happyReduce_89 = happySpecReduce_1  18# happyReduction_89
happyReduction_89 happy_x_1
	 =  case happyOut40 happy_x_1 of { (HappyWrap40 happy_var_1) -> 
	happyIn56
		 (AbsCoLa.SubQuoted happy_var_1
	)}

happyReduce_90 = happySpecReduce_1  18# happyReduction_90
happyReduction_90 happy_x_1
	 =  case happyOut38 happy_x_1 of { (HappyWrap38 happy_var_1) -> 
	happyIn56
		 (AbsCoLa.SubUnQuoted happy_var_1
	)}

happyReduce_91 = happySpecReduce_1  19# happyReduction_91
happyReduction_91 happy_x_1
	 =  happyIn57
		 (AbsCoLa.VDel
	)

happyReduce_92 = happySpecReduce_1  19# happyReduction_92
happyReduction_92 happy_x_1
	 =  happyIn57
		 (AbsCoLa.VPay
	)

happyReduce_93 = happySpecReduce_1  19# happyReduction_93
happyReduction_93 happy_x_1
	 =  happyIn57
		 (AbsCoLa.VCharge
	)

happyReduce_94 = happySpecReduce_1  19# happyReduction_94
happyReduction_94 happy_x_1
	 =  happyIn57
		 (AbsCoLa.VRefund
	)

happyReduce_95 = happySpecReduce_1  20# happyReduction_95
happyReduction_95 happy_x_1
	 =  happyIn58
		 (AbsCoLa.VSDel
	)

happyReduce_96 = happySpecReduce_1  20# happyReduction_96
happyReduction_96 happy_x_1
	 =  happyIn58
		 (AbsCoLa.VSPay
	)

happyReduce_97 = happySpecReduce_1  20# happyReduction_97
happyReduction_97 happy_x_1
	 =  happyIn58
		 (AbsCoLa.VSCharge
	)

happyReduce_98 = happySpecReduce_1  20# happyReduction_98
happyReduction_98 happy_x_1
	 =  happyIn58
		 (AbsCoLa.VSRefund
	)

happyReduce_99 = happySpecReduce_2  21# happyReduction_99
happyReduction_99 happy_x_2
	happy_x_1
	 =  happyIn59
		 (AbsCoLa.CompareLess
	)

happyReduce_100 = happySpecReduce_1  21# happyReduction_100
happyReduction_100 happy_x_1
	 =  case happyOut60 happy_x_1 of { (HappyWrap60 happy_var_1) -> 
	happyIn59
		 (AbsCoLa.CompareEq happy_var_1
	)}

happyReduce_101 = happySpecReduce_1  21# happyReduction_101
happyReduction_101 happy_x_1
	 =  case happyOut61 happy_x_1 of { (HappyWrap61 happy_var_1) -> 
	happyIn59
		 (AbsCoLa.CompareMore happy_var_1
	)}

happyReduce_102 = happySpecReduce_1  22# happyReduction_102
happyReduction_102 happy_x_1
	 =  happyIn60
		 (AbsCoLa.EqOne
	)

happyReduce_103 = happySpecReduce_2  22# happyReduction_103
happyReduction_103 happy_x_2
	happy_x_1
	 =  happyIn60
		 (AbsCoLa.EqTwo
	)

happyReduce_104 = happySpecReduce_2  23# happyReduction_104
happyReduction_104 happy_x_2
	happy_x_1
	 =  happyIn61
		 (AbsCoLa.MoreOne
	)

happyReduce_105 = happySpecReduce_2  23# happyReduction_105
happyReduction_105 happy_x_2
	happy_x_1
	 =  happyIn61
		 (AbsCoLa.MoreTwo
	)

happyReduce_106 = happySpecReduce_1  24# happyReduction_106
happyReduction_106 happy_x_1
	 =  case happyOut63 happy_x_1 of { (HappyWrap63 happy_var_1) -> 
	happyIn62
		 (AbsCoLa.ModalObli happy_var_1
	)}

happyReduce_107 = happySpecReduce_1  24# happyReduction_107
happyReduction_107 happy_x_1
	 =  happyIn62
		 (AbsCoLa.ModalPermi
	)

happyReduce_108 = happySpecReduce_3  24# happyReduction_108
happyReduction_108 happy_x_3
	happy_x_2
	happy_x_1
	 =  happyIn62
		 (AbsCoLa.ModalForbi
	)

happyReduce_109 = happySpecReduce_1  25# happyReduction_109
happyReduction_109 happy_x_1
	 =  happyIn63
		 (AbsCoLa.ObliOne
	)

happyReduce_110 = happySpecReduce_1  25# happyReduction_110
happyReduction_110 happy_x_1
	 =  happyIn63
		 (AbsCoLa.ObliTwo
	)

happyReduce_111 = happyReduce 5# 26# happyReduction_111
happyReduction_111 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut74 happy_x_3 of { (HappyWrap74 happy_var_3) -> 
	case happyOut67 happy_x_4 of { (HappyWrap67 happy_var_4) -> 
	case happyOut74 happy_x_5 of { (HappyWrap74 happy_var_5) -> 
	happyIn64
		 (AbsCoLa.DateSpe happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_112 = happySpecReduce_2  26# happyReduction_112
happyReduction_112 happy_x_2
	happy_x_1
	 =  happyIn64
		 (AbsCoLa.DateAny
	)

happyReduce_113 = happySpecReduce_3  26# happyReduction_113
happyReduction_113 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut56 happy_x_3 of { (HappyWrap56 happy_var_3) -> 
	happyIn64
		 (AbsCoLa.DateSome happy_var_3
	)}

happyReduce_114 = happySpecReduce_3  26# happyReduction_114
happyReduction_114 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut56 happy_x_3 of { (HappyWrap56 happy_var_3) -> 
	happyIn64
		 (AbsCoLa.DateThe happy_var_3
	)}

happyReduce_115 = happyReduce 4# 26# happyReduction_115
happyReduction_115 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut65 happy_x_1 of { (HappyWrap65 happy_var_1) -> 
	case happyOut74 happy_x_2 of { (HappyWrap74 happy_var_2) -> 
	case happyOut67 happy_x_3 of { (HappyWrap67 happy_var_3) -> 
	case happyOut74 happy_x_4 of { (HappyWrap74 happy_var_4) -> 
	happyIn64
		 (AbsCoLa.DateQuanSpecific happy_var_1 happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}}

happyReduce_116 = happySpecReduce_3  26# happyReduction_116
happyReduction_116 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut65 happy_x_1 of { (HappyWrap65 happy_var_1) -> 
	case happyOut56 happy_x_3 of { (HappyWrap56 happy_var_3) -> 
	happyIn64
		 (AbsCoLa.DateQuanSome happy_var_1 happy_var_3
	)}}

happyReduce_117 = happySpecReduce_3  26# happyReduction_117
happyReduction_117 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut65 happy_x_1 of { (HappyWrap65 happy_var_1) -> 
	case happyOut56 happy_x_3 of { (HappyWrap56 happy_var_3) -> 
	happyIn64
		 (AbsCoLa.DateQuanThe happy_var_1 happy_var_3
	)}}

happyReduce_118 = happyReduce 5# 26# happyReduction_118
happyReduction_118 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut65 happy_x_1 of { (HappyWrap65 happy_var_1) -> 
	case happyOut66 happy_x_2 of { (HappyWrap66 happy_var_2) -> 
	case happyOut65 happy_x_3 of { (HappyWrap65 happy_var_3) -> 
	case happyOut56 happy_x_5 of { (HappyWrap56 happy_var_5) -> 
	happyIn64
		 (AbsCoLa.DateQuanTempSome happy_var_1 happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}}

happyReduce_119 = happyReduce 5# 26# happyReduction_119
happyReduction_119 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut65 happy_x_1 of { (HappyWrap65 happy_var_1) -> 
	case happyOut66 happy_x_2 of { (HappyWrap66 happy_var_2) -> 
	case happyOut65 happy_x_3 of { (HappyWrap65 happy_var_3) -> 
	case happyOut56 happy_x_5 of { (HappyWrap56 happy_var_5) -> 
	happyIn64
		 (AbsCoLa.DateQuanTempThe happy_var_1 happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}}

happyReduce_120 = happySpecReduce_1  27# happyReduction_120
happyReduction_120 happy_x_1
	 =  happyIn65
		 (AbsCoLa.TempAfter
	)

happyReduce_121 = happySpecReduce_1  27# happyReduction_121
happyReduction_121 happy_x_1
	 =  happyIn65
		 (AbsCoLa.TempBefore
	)

happyReduce_122 = happySpecReduce_2  28# happyReduction_122
happyReduction_122 happy_x_2
	happy_x_1
	 =  case happyOut74 happy_x_1 of { (HappyWrap74 happy_var_1) -> 
	happyIn66
		 (AbsCoLa.TempOffDay happy_var_1
	)}

happyReduce_123 = happySpecReduce_2  28# happyReduction_123
happyReduction_123 happy_x_2
	happy_x_1
	 =  case happyOut74 happy_x_1 of { (HappyWrap74 happy_var_1) -> 
	happyIn66
		 (AbsCoLa.TempOffYear happy_var_1
	)}

happyReduce_124 = happySpecReduce_2  28# happyReduction_124
happyReduction_124 happy_x_2
	happy_x_1
	 =  case happyOut74 happy_x_1 of { (HappyWrap74 happy_var_1) -> 
	happyIn66
		 (AbsCoLa.TempOffWeek happy_var_1
	)}

happyReduce_125 = happySpecReduce_2  28# happyReduction_125
happyReduction_125 happy_x_2
	happy_x_1
	 =  case happyOut74 happy_x_1 of { (HappyWrap74 happy_var_1) -> 
	happyIn66
		 (AbsCoLa.TempOffDays happy_var_1
	)}

happyReduce_126 = happySpecReduce_2  28# happyReduction_126
happyReduction_126 happy_x_2
	happy_x_1
	 =  case happyOut74 happy_x_1 of { (HappyWrap74 happy_var_1) -> 
	happyIn66
		 (AbsCoLa.TempOffYears happy_var_1
	)}

happyReduce_127 = happySpecReduce_2  28# happyReduction_127
happyReduction_127 happy_x_2
	happy_x_1
	 =  case happyOut74 happy_x_1 of { (HappyWrap74 happy_var_1) -> 
	happyIn66
		 (AbsCoLa.TempOffWeeks happy_var_1
	)}

happyReduce_128 = happySpecReduce_1  29# happyReduction_128
happyReduction_128 happy_x_1
	 =  happyIn67
		 (AbsCoLa.MJan
	)

happyReduce_129 = happySpecReduce_1  29# happyReduction_129
happyReduction_129 happy_x_1
	 =  happyIn67
		 (AbsCoLa.MFeb
	)

happyReduce_130 = happySpecReduce_1  29# happyReduction_130
happyReduction_130 happy_x_1
	 =  happyIn67
		 (AbsCoLa.MMar
	)

happyReduce_131 = happySpecReduce_1  29# happyReduction_131
happyReduction_131 happy_x_1
	 =  happyIn67
		 (AbsCoLa.MApr
	)

happyReduce_132 = happySpecReduce_1  29# happyReduction_132
happyReduction_132 happy_x_1
	 =  happyIn67
		 (AbsCoLa.MMay
	)

happyReduce_133 = happySpecReduce_1  29# happyReduction_133
happyReduction_133 happy_x_1
	 =  happyIn67
		 (AbsCoLa.MJun
	)

happyReduce_134 = happySpecReduce_1  29# happyReduction_134
happyReduction_134 happy_x_1
	 =  happyIn67
		 (AbsCoLa.MJul
	)

happyReduce_135 = happySpecReduce_1  29# happyReduction_135
happyReduction_135 happy_x_1
	 =  happyIn67
		 (AbsCoLa.MAug
	)

happyReduce_136 = happySpecReduce_1  29# happyReduction_136
happyReduction_136 happy_x_1
	 =  happyIn67
		 (AbsCoLa.MSep
	)

happyReduce_137 = happySpecReduce_1  29# happyReduction_137
happyReduction_137 happy_x_1
	 =  happyIn67
		 (AbsCoLa.MOct
	)

happyReduce_138 = happySpecReduce_1  29# happyReduction_138
happyReduction_138 happy_x_1
	 =  happyIn67
		 (AbsCoLa.MNov
	)

happyReduce_139 = happySpecReduce_1  29# happyReduction_139
happyReduction_139 happy_x_1
	 =  happyIn67
		 (AbsCoLa.MDec
	)

happyReduce_140 = happySpecReduce_1  30# happyReduction_140
happyReduction_140 happy_x_1
	 =  case happyOut69 happy_x_1 of { (HappyWrap69 happy_var_1) -> 
	happyIn68
		 (AbsCoLa.ObjNu happy_var_1
	)}

happyReduce_141 = happySpecReduce_1  30# happyReduction_141
happyReduction_141 happy_x_1
	 =  case happyOut73 happy_x_1 of { (HappyWrap73 happy_var_1) -> 
	happyIn68
		 (AbsCoLa.ObjNonNu happy_var_1
	)}

happyReduce_142 = happySpecReduce_2  31# happyReduction_142
happyReduction_142 happy_x_2
	happy_x_1
	 =  case happyOut70 happy_x_1 of { (HappyWrap70 happy_var_1) -> 
	case happyOut74 happy_x_2 of { (HappyWrap74 happy_var_2) -> 
	happyIn69
		 (AbsCoLa.NumPound happy_var_1 happy_var_2
	)}}

happyReduce_143 = happySpecReduce_2  31# happyReduction_143
happyReduction_143 happy_x_2
	happy_x_1
	 =  case happyOut71 happy_x_1 of { (HappyWrap71 happy_var_1) -> 
	case happyOut74 happy_x_2 of { (HappyWrap74 happy_var_2) -> 
	happyIn69
		 (AbsCoLa.NumDol happy_var_1 happy_var_2
	)}}

happyReduce_144 = happySpecReduce_2  31# happyReduction_144
happyReduction_144 happy_x_2
	happy_x_1
	 =  case happyOut72 happy_x_1 of { (HappyWrap72 happy_var_1) -> 
	case happyOut74 happy_x_2 of { (HappyWrap74 happy_var_2) -> 
	happyIn69
		 (AbsCoLa.NumEur happy_var_1 happy_var_2
	)}}

happyReduce_145 = happySpecReduce_2  31# happyReduction_145
happyReduction_145 happy_x_2
	happy_x_1
	 =  case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	happyIn69
		 (AbsCoLa.NumAmount happy_var_2
	)}

happyReduce_146 = happySpecReduce_1  32# happyReduction_146
happyReduction_146 happy_x_1
	 =  happyIn70
		 (AbsCoLa.PoundOne
	)

happyReduce_147 = happySpecReduce_1  32# happyReduction_147
happyReduction_147 happy_x_1
	 =  happyIn70
		 (AbsCoLa.PoundTwo
	)

happyReduce_148 = happySpecReduce_1  32# happyReduction_148
happyReduction_148 happy_x_1
	 =  happyIn70
		 (AbsCoLa.PoundThree
	)

happyReduce_149 = happySpecReduce_1  33# happyReduction_149
happyReduction_149 happy_x_1
	 =  happyIn71
		 (AbsCoLa.DollarOne
	)

happyReduce_150 = happySpecReduce_1  33# happyReduction_150
happyReduction_150 happy_x_1
	 =  happyIn71
		 (AbsCoLa.DollarTwo
	)

happyReduce_151 = happySpecReduce_1  33# happyReduction_151
happyReduction_151 happy_x_1
	 =  happyIn71
		 (AbsCoLa.DollarThree
	)

happyReduce_152 = happySpecReduce_1  34# happyReduction_152
happyReduction_152 happy_x_1
	 =  happyIn72
		 (AbsCoLa.EuroOne
	)

happyReduce_153 = happySpecReduce_1  34# happyReduction_153
happyReduction_153 happy_x_1
	 =  happyIn72
		 (AbsCoLa.EuroTwo
	)

happyReduce_154 = happySpecReduce_2  35# happyReduction_154
happyReduction_154 happy_x_2
	happy_x_1
	 =  case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	happyIn73
		 (AbsCoLa.NonNumCurr happy_var_2
	)}

happyReduce_155 = happySpecReduce_2  35# happyReduction_155
happyReduction_155 happy_x_2
	happy_x_1
	 =  case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	happyIn73
		 (AbsCoLa.NonNumRep happy_var_2
	)}

happyReduce_156 = happySpecReduce_2  35# happyReduction_156
happyReduction_156 happy_x_2
	happy_x_1
	 =  case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	happyIn73
		 (AbsCoLa.NonNumNamed happy_var_2
	)}

happyReduce_157 = happySpecReduce_2  35# happyReduction_157
happyReduction_157 happy_x_2
	happy_x_1
	 =  case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	happyIn73
		 (AbsCoLa.NonNumOther happy_var_2
	)}

happyReduce_158 = happySpecReduce_1  36# happyReduction_158
happyReduction_158 happy_x_1
	 =  case happyOut39 happy_x_1 of { (HappyWrap39 happy_var_1) -> 
	happyIn74
		 (AbsCoLa.NumInt happy_var_1
	)}

happyReduce_159 = happySpecReduce_2  37# happyReduction_159
happyReduction_159 happy_x_2
	happy_x_1
	 =  case happyOut56 happy_x_2 of { (HappyWrap56 happy_var_2) -> 
	happyIn75
		 (AbsCoLa.Rec happy_var_2
	)}

happyNewToken action sts stk [] =
	happyDoAction 81# notHappyAtAll action sts stk []

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
	PT _ (TV happy_dollar_dollar) -> cont 78#;
	PT _ (TI happy_dollar_dollar) -> cont 79#;
	PT _ (TL happy_dollar_dollar) -> cont 80#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 81# tk tks = happyError' (tks, explist)
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
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {(HappyWrap41 x') = happyOut41 x} in x'))

pComponent tks = happySomeParser where
 happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (let {(HappyWrap42 x') = happyOut42 x} in x'))

pDefinition tks = happySomeParser where
 happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (let {(HappyWrap43 x') = happyOut43 x} in x'))

pSimpleDefinition tks = happySomeParser where
 happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (let {(HappyWrap44 x') = happyOut44 x} in x'))

pNumericalExpression tks = happySomeParser where
 happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (let {(HappyWrap45 x') = happyOut45 x} in x'))

pOperator tks = happySomeParser where
 happySomeParser = happyThen (happyParse 5# tks) (\x -> happyReturn (let {(HappyWrap46 x') = happyOut46 x} in x'))

pConditionalDefinition tks = happySomeParser where
 happySomeParser = happyThen (happyParse 6# tks) (\x -> happyReturn (let {(HappyWrap47 x') = happyOut47 x} in x'))

pStatement tks = happySomeParser where
 happySomeParser = happyThen (happyParse 7# tks) (\x -> happyReturn (let {(HappyWrap48 x') = happyOut48 x} in x'))

pConditionalStatement tks = happySomeParser where
 happySomeParser = happyThen (happyParse 8# tks) (\x -> happyReturn (let {(HappyWrap49 x') = happyOut49 x} in x'))

pSimpleStatement tks = happySomeParser where
 happySomeParser = happyThen (happyParse 9# tks) (\x -> happyReturn (let {(HappyWrap50 x') = happyOut50 x} in x'))

pCondition tks = happySomeParser where
 happySomeParser = happyThen (happyParse 10# tks) (\x -> happyReturn (let {(HappyWrap51 x') = happyOut51 x} in x'))

pSimpleCondition tks = happySomeParser where
 happySomeParser = happyThen (happyParse 11# tks) (\x -> happyReturn (let {(HappyWrap52 x') = happyOut52 x} in x'))

pBooleanExpression tks = happySomeParser where
 happySomeParser = happyThen (happyParse 12# tks) (\x -> happyReturn (let {(HappyWrap53 x') = happyOut53 x} in x'))

pID tks = happySomeParser where
 happySomeParser = happyThen (happyParse 13# tks) (\x -> happyReturn (let {(HappyWrap54 x') = happyOut54 x} in x'))

pHolds tks = happySomeParser where
 happySomeParser = happyThen (happyParse 14# tks) (\x -> happyReturn (let {(HappyWrap55 x') = happyOut55 x} in x'))

pSubject tks = happySomeParser where
 happySomeParser = happyThen (happyParse 15# tks) (\x -> happyReturn (let {(HappyWrap56 x') = happyOut56 x} in x'))

pVerb tks = happySomeParser where
 happySomeParser = happyThen (happyParse 16# tks) (\x -> happyReturn (let {(HappyWrap57 x') = happyOut57 x} in x'))

pVerbStatus tks = happySomeParser where
 happySomeParser = happyThen (happyParse 17# tks) (\x -> happyReturn (let {(HappyWrap58 x') = happyOut58 x} in x'))

pComparison tks = happySomeParser where
 happySomeParser = happyThen (happyParse 18# tks) (\x -> happyReturn (let {(HappyWrap59 x') = happyOut59 x} in x'))

pEqual tks = happySomeParser where
 happySomeParser = happyThen (happyParse 19# tks) (\x -> happyReturn (let {(HappyWrap60 x') = happyOut60 x} in x'))

pMore tks = happySomeParser where
 happySomeParser = happyThen (happyParse 20# tks) (\x -> happyReturn (let {(HappyWrap61 x') = happyOut61 x} in x'))

pModalVerb tks = happySomeParser where
 happySomeParser = happyThen (happyParse 21# tks) (\x -> happyReturn (let {(HappyWrap62 x') = happyOut62 x} in x'))

pObligation tks = happySomeParser where
 happySomeParser = happyThen (happyParse 22# tks) (\x -> happyReturn (let {(HappyWrap63 x') = happyOut63 x} in x'))

pDate tks = happySomeParser where
 happySomeParser = happyThen (happyParse 23# tks) (\x -> happyReturn (let {(HappyWrap64 x') = happyOut64 x} in x'))

pTemporalQuantifier tks = happySomeParser where
 happySomeParser = happyThen (happyParse 24# tks) (\x -> happyReturn (let {(HappyWrap65 x') = happyOut65 x} in x'))

pTemporalOffset tks = happySomeParser where
 happySomeParser = happyThen (happyParse 25# tks) (\x -> happyReturn (let {(HappyWrap66 x') = happyOut66 x} in x'))

pMonth tks = happySomeParser where
 happySomeParser = happyThen (happyParse 26# tks) (\x -> happyReturn (let {(HappyWrap67 x') = happyOut67 x} in x'))

pObject tks = happySomeParser where
 happySomeParser = happyThen (happyParse 27# tks) (\x -> happyReturn (let {(HappyWrap68 x') = happyOut68 x} in x'))

pNumericalObject tks = happySomeParser where
 happySomeParser = happyThen (happyParse 28# tks) (\x -> happyReturn (let {(HappyWrap69 x') = happyOut69 x} in x'))

pPounds tks = happySomeParser where
 happySomeParser = happyThen (happyParse 29# tks) (\x -> happyReturn (let {(HappyWrap70 x') = happyOut70 x} in x'))

pDollars tks = happySomeParser where
 happySomeParser = happyThen (happyParse 30# tks) (\x -> happyReturn (let {(HappyWrap71 x') = happyOut71 x} in x'))

pEuros tks = happySomeParser where
 happySomeParser = happyThen (happyParse 31# tks) (\x -> happyReturn (let {(HappyWrap72 x') = happyOut72 x} in x'))

pNonNumericalObject tks = happySomeParser where
 happySomeParser = happyThen (happyParse 32# tks) (\x -> happyReturn (let {(HappyWrap73 x') = happyOut73 x} in x'))

pNum tks = happySomeParser where
 happySomeParser = happyThen (happyParse 33# tks) (\x -> happyReturn (let {(HappyWrap74 x') = happyOut74 x} in x'))

pReceiver tks = happySomeParser where
 happySomeParser = happyThen (happyParse 34# tks) (\x -> happyReturn (let {(HappyWrap75 x') = happyOut75 x} in x'))

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
