module Helper.ToStringFunctions where

import Prelude
  ( ($), (++)
  , Int, String
  , show
  )

import Parser.AbsCoLa 

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
temporalOffsetToString (TempOffDay (NumInt num)) = show num ++ " day"
temporalOffsetToString (TempOffYear (NumInt num)) = show num ++ " year"
temporalOffsetToString (TempOffWeek (NumInt num)) = show num ++ " week"
temporalOffsetToString (TempOffDays (NumInt num)) = show num ++ " days"
temporalOffsetToString (TempOffYears (NumInt num)) = show num ++ " years"
temporalOffsetToString (TempOffWeeks (NumInt num)) = show num ++ " weeks"

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

dateToString' :: Date -> String
dateToString' (DateSpe (DateSpeOnThe day month year)) = dateSpeToString day month year
dateToString' (DateSpe (DateSpeOn day month year)) = dateSpeToString day month year
dateToString' (DateAny) = "ANYDATE"
dateToString' (DateSome subject) = "SOMEDATE " ++ subjectToString subject
dateToString' (DateThe subject) = "THEDATE " ++ subjectToString subject
dateToString' (DateQuanSpecific tq day month year) = temporalQuantifierToString tq ++ dateSpeToString day month year  
dateToString' (DateQuanSome tq subject) = temporalQuantifierToString tq ++ "SOMEDATE " ++ subjectToString subject
dateToString' (DateQuanThe tq subject) = temporalQuantifierToString tq ++ "THEDATE " ++ subjectToString subject
dateToString' (DateQuanSomeWO to tq subject) = temporalOffsetToString to ++ temporalQuantifierToString tq ++ "SOMEDATE " ++ subjectToString subject
dateToString' (DateQuanTheWO to tq subject) = temporalOffsetToString to ++ temporalQuantifierToString tq ++ "THEDATE " ++ subjectToString subject
dateToString' (DateQuanTempSome tq1 to tq2 subject) = temporalQuantifierToString tq1 ++ temporalOffsetToString to ++ temporalQuantifierToString tq2 ++ "SOMEDATE " ++ subjectToString subject
dateToString' (DateQuanTempThe tq1 to tq2 subject) = temporalQuantifierToString tq1 ++ temporalOffsetToString to ++ temporalQuantifierToString tq2 ++ "THEDATE " ++ subjectToString subject

verbStatusToVerb :: VerbStatus -> Verb
verbStatusToVerb VSDel = VDel
verbStatusToVerb VSPay = VPay
verbStatusToVerb VSCharge = VCharge
verbStatusToVerb VSRefund = VRefund

yesBooleanExpressionToString :: BooleanExpression -> String
yesBooleanExpressionToString (BoolEx subject1 verbStatus comparison subject2) =
    subjectToString subject1 ++ " " ++ verbStatusToString verbStatus ++ " " ++ comparisonToString comparison ++ " " ++ subjectToString subject2

noBooleanExpressionToString :: BooleanExpression -> String
noBooleanExpressionToString (BoolEx subject1 verbStatus comparison subject2) =
    subjectToString subject1 ++ " DIDN'T " ++ verbStatusToVerbString verbStatus ++ " " ++ comparisonToString comparison ++ " " ++ subjectToString subject2