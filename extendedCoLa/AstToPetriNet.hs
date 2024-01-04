module AstToPetriNet where

import Prelude
  ( ($), (++), (.)
  , Int
  , String
  , Show, show
  , Eq
  , Read
  , Ord
  , Bool(..)
  , IO, putStrLn
  , error
  )

import AbsCoLa   

import Data.List (map, intercalate, concatMap)

data PNContract
    = PNContract ([PNStatement], [PNConditionalStatement], [PNDefinition], [PNConditionalDefinition])
  deriving (Eq, Read, Show)

data PNTest = TRUE | FALSE
  deriving (Eq, Read, Show)

data PNModalVerb = PNSHALL | PNSHANT | PNMAY
  deriving (Eq, Read, Show)

data PNStatement 
    = PNTemporalActionStatement PNTest Subject PNModalVerb Verb Object Receiver Date
  deriving (Eq, Read, Show)

data PNConditionalStatement 
    = PNConditionalStatement PNCondition PNStatement
  deriving (Eq, Read, Show)

data PNCondition 
    = PNStatementCondition PNStatement
    | PNTemporalActionCondition PNTest Subject VerbStatus Object Receiver Date
    | PNExpressionCondition PNTest Subject VerbStatus Comparison Subject
    | PNAndCondition [PNCondition]
    | PNOrCondition [PNCondition]
  deriving (Eq, Read, Show)

data PNDefinition
    = PNIsDefinition Subject Subject
    | PNEqualsDefinition Subject NumericalExpression
    | PNDefAnd [PNDefinition] 
  deriving (Eq, Read, Show)

data PNConditionalDefinition 
    = PNConditionalDefinition PNCondition PNDefinition
  deriving (Eq, Read, Show)

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
componentToPN (ComConDef conditionalDefinition) =  ([], [], [], (conditionalDefinitionToPN conditionalDefinition))
componentToPN (ComState statement) = ((statementToPN statement), [], [], [])
componentToPN (ComConState conditionalStatement) = ([], (conditionalStatementToPN conditionalStatement), [], []) 

definitionToPN :: Definition -> PNDefinition
definitionToPN (DefSim simpleDefinition) = simpleDefinitionToPN simpleDefinition
definitionToPN (DefAnd simpleDefinition definition) = 
    case definitionToPN definition of
        PNDefAnd ds -> PNDefAnd (simpleDefinitionToPN simpleDefinition : ds)
        d -> PNDefAnd [simpleDefinitionToPN simpleDefinition, d]

conditionalDefinitionToPN :: ConditionalDefinition -> [PNConditionalDefinition]
conditionalDefinitionToPN (ConDefIf definition condition) = [PNConditionalDefinition (conditionToPN condition) (definitionToPN definition)]
conditionalDefinitionToPN (ConDefIfThen condition definition) = [PNConditionalDefinition (conditionToPN condition) (definitionToPN definition)]
conditionalDefinitionToPN (ConDefIfElse definition1 condition definition2) = 
    [PNConditionalDefinition (conditionToPN condition) (definitionToPN definition1)] ++ [PNConditionalDefinition (notConditionToPN condition) (definitionToPN definition2)] 
conditionalDefinitionToPN (ConDefIfThenElse condition definition1 definition2) = 
    [PNConditionalDefinition (conditionToPN condition) (definitionToPN definition1)] ++ [PNConditionalDefinition (notConditionToPN condition) (definitionToPN definition2)] 

statementToPN :: Statement -> [PNStatement]
statementToPN (StateSim simpleStatement) = simpleStatementToPN simpleStatement
statementToPN (StateOr simpleStatement statement) = simpleStatementToPN simpleStatement ++ statementToPN statement
statementToPN (StateAnd simpleStatement statement) = simpleStatementToPN simpleStatement ++ statementToPN statement

conditionalStatementToPN :: ConditionalStatement -> [PNConditionalStatement]
conditionalStatementToPN (ConStateIf statement condition) = xConditionalStatementToPN (conditionToPN condition) (statementToPN statement)
conditionalStatementToPN (ConStateIfThen condition statement) = xConditionalStatementToPN (conditionToPN condition) (statementToPN statement)
conditionalStatementToPN (ConStateIfElse statement1 condition statement2) =
    (xConditionalStatementToPN (conditionToPN condition) (statementToPN statement1)) ++ (xConditionalStatementToPN (notConditionToPN condition) (statementToPN statement2))
conditionalStatementToPN (ConStateIfThenElse condition statement1 statement2) =
    (xConditionalStatementToPN (conditionToPN condition) (statementToPN statement1)) ++ (xConditionalStatementToPN (notConditionToPN condition) (statementToPN statement2))

xConditionalStatementToPN :: PNCondition -> [PNStatement] -> [PNConditionalStatement]
xConditionalStatementToPN pnCondition statementList = map (pnConditionalStatement pnCondition) statementList

simpleDefinitionToPN :: SimpleDefinition -> PNDefinition
simpleDefinitionToPN (SimDefIs id subject1 subject2) = PNIsDefinition subject1 subject2
simpleDefinitionToPN (SimDefEq id subject numericalExpression) = PNEqualsDefinition subject numericalExpression
simpleDefinitionToPN (SimDefDate id subject day month year) = PNIsDefinition subject (dateToSubject day month year)

conditionToPN :: Condition -> PNCondition
conditionToPN (CondiSim simpleCondition) = simpleConditionToPN simpleCondition
conditionToPN (CondiOr simpleCondition condition) =
    case conditionToPN condition of
        PNOrCondition ds -> PNOrCondition (simpleConditionToPN simpleCondition : ds)
        d -> PNOrCondition [simpleConditionToPN simpleCondition, d]
conditionToPN (CondiAnd simpleCondition condition) =
    case conditionToPN condition of
        PNAndCondition ds -> PNAndCondition (simpleConditionToPN simpleCondition : ds)
        d -> PNAndCondition [simpleConditionToPN simpleCondition, d]

notConditionToPN :: Condition -> PNCondition
notConditionToPN (CondiSim simpleCondition) = notSimpleConditionToPN simpleCondition
notConditionToPN (CondiOr simpleCondition condition) =
    case conditionToPN condition of
        PNAndCondition ds -> PNAndCondition (notSimpleConditionToPN simpleCondition : ds)
        d -> PNAndCondition [notSimpleConditionToPN simpleCondition, d]
notConditionToPN (CondiAnd simpleCondition condition) =
    case conditionToPN condition of
        PNOrCondition ds -> PNOrCondition (notSimpleConditionToPN simpleCondition : ds)
        d -> PNOrCondition [notSimpleConditionToPN simpleCondition, d]

simpleStatementToPN :: SimpleStatement -> [PNStatement]
simpleStatementToPN (SimStateOne id HoldYes subject modalVerb verb object receiver date) =
    [createPNSimpleStatementYes subject modalVerb verb object receiver date]
simpleStatementToPN (SimStateOne id HoldNo subject modalVerb verb object receiver date) =
    [createPNSimpleStatementNo subject modalVerb verb object receiver date]
simpleStatementToPN (SimStateTwo id HoldYes subject date modalVerb verb object receiver) =
    [createPNSimpleStatementYes subject modalVerb verb object receiver date]
simpleStatementToPN (SimStateTwo id HoldNo subject date modalVerb verb object receiver) =
    [createPNSimpleStatementNo subject modalVerb verb object receiver date]
simpleStatementToPN (SimStateThree id HoldYes date subject modalVerb verb object receiver) =
    [createPNSimpleStatementYes subject modalVerb verb object receiver date]
simpleStatementToPN (SimStateThree id HoldNo date subject modalVerb verb object receiver) =
    [createPNSimpleStatementNo subject modalVerb verb object receiver date]
simpleStatementToPN (SimStateFour _ _ _ _ _ _ _) = error "Statement doesn't consist modal verb"
simpleStatementToPN (SimStateOneNH id subject modalVerb verb object receiver date) =
    [createPNSimpleStatementYes subject modalVerb verb object receiver date]
simpleStatementToPN (SimStateTwoNH id subject date modalVerb verb object receiver) =
    [createPNSimpleStatementYes subject modalVerb verb object receiver date]
simpleStatementToPN (SimStateThreeNH id date subject modalVerb verb object receiver) =
    [createPNSimpleStatementYes subject modalVerb verb object receiver date]
simpleStatementToPN (SimStateFourNH _ _ _ _ _ _) = error "Statement doesn't consist modal verb"

simpleConditionToPN :: SimpleCondition -> PNCondition
simpleConditionToPN (SimConOne id HoldYes subject verbStatus object receiver date) =
    createPNSimpleConditionYes subject verbStatus object receiver date
simpleConditionToPN (SimConOne id HoldNo subject verbStatus object receiver date) =
    createPNSimpleConditionNo subject verbStatus object receiver date
simpleConditionToPN (SimConTwo id HoldYes subject date verbStatus object receiver) =
    createPNSimpleConditionYes subject verbStatus object receiver date
simpleConditionToPN (SimConTwo id HoldNo subject date verbStatus object receiver) =
    createPNSimpleConditionNo subject verbStatus object receiver date
simpleConditionToPN (SimConThree id HoldYes date subject verbStatus object receiver) =
    createPNSimpleConditionYes subject verbStatus object receiver date
simpleConditionToPN (SimConThree id HoldNo date subject verbStatus object receiver) =
    createPNSimpleConditionNo subject verbStatus object receiver date
simpleConditionToPN (SimConFour id HoldYes subject modalVerb verb object receiver date) = 
    PNStatementCondition (createPNSimpleStatementYes subject modalVerb verb object receiver date)
simpleConditionToPN (SimConFour id HoldNo subject modalVerb verb object receiver date) = 
    PNStatementCondition (createPNSimpleStatementNo subject modalVerb verb object receiver date)
simpleConditionToPN (SimConFive id HoldYes (BoolEx subject1 verbStatus comparison subject2)) =
    booleanExpressionToPNYes subject1 verbStatus comparison subject2
simpleConditionToPN (SimConFive id HoldNo (BoolEx subject1 verbStatus comparison subject2)) =
    booleanExpressionToPNNo subject1 verbStatus comparison subject2
simpleConditionToPN (SimConOneNH id subject verbStatus object receiver date) =
        createPNSimpleConditionYes subject verbStatus object receiver date 
simpleConditionToPN (SimConTwoNH id subject date verbStatus object receiver) =
    createPNSimpleConditionYes subject verbStatus object receiver date
simpleConditionToPN (SimConThreeNH id date subject verbStatus object receiver) =
    createPNSimpleConditionYes subject verbStatus object receiver date
simpleConditionToPN (SimConFourNH id subject modalVerb verb object receiver date) = 
    PNStatementCondition (createPNSimpleStatementYes subject modalVerb verb object receiver date)
simpleConditionToPN (SimConFiveNH id (BoolEx subject1 verbStatus comparison subject2)) =
    booleanExpressionToPNYes subject1 verbStatus comparison subject2

notSimpleConditionToPN :: SimpleCondition -> PNCondition
notSimpleConditionToPN (SimConOne id HoldYes subject verbStatus object receiver date) =
    createPNSimpleConditionNo subject verbStatus object receiver date
notSimpleConditionToPN (SimConOne id HoldNo subject verbStatus object receiver date) =
    createPNSimpleConditionYes subject verbStatus object receiver date
notSimpleConditionToPN (SimConTwo id HoldYes subject date verbStatus object receiver) =
    createPNSimpleConditionNo subject verbStatus object receiver date
notSimpleConditionToPN (SimConTwo id HoldNo subject date verbStatus object receiver) =
    createPNSimpleConditionYes subject verbStatus object receiver date
notSimpleConditionToPN (SimConThree id HoldYes date subject verbStatus object receiver) =
    createPNSimpleConditionNo subject verbStatus object receiver date
notSimpleConditionToPN (SimConThree id HoldNo date subject verbStatus object receiver) =
    createPNSimpleConditionYes subject verbStatus object receiver date
notSimpleConditionToPN (SimConFour id HoldYes subject modalVerb verb object receiver date) = 
    PNStatementCondition (createPNSimpleStatementNo subject modalVerb verb object receiver date)
notSimpleConditionToPN (SimConFour id HoldNo subject modalVerb verb object receiver date) = 
    PNStatementCondition (createPNSimpleStatementYes subject modalVerb verb object receiver date)
notSimpleConditionToPN (SimConFive id HoldYes (BoolEx subject1 verbStatus comparison subject2)) =
    booleanExpressionToPNNo subject1 verbStatus comparison subject2
notSimpleConditionToPN (SimConFive id HoldNo (BoolEx subject1 verbStatus comparison subject2)) =
    booleanExpressionToPNYes subject1 verbStatus comparison subject2
notSimpleConditionToPN (SimConOneNH id subject verbStatus object receiver date) =
    createPNSimpleConditionNo subject verbStatus object receiver date 
notSimpleConditionToPN (SimConTwoNH id subject date verbStatus object receiver) =
    createPNSimpleConditionNo subject verbStatus object receiver date
notSimpleConditionToPN (SimConThreeNH id date subject verbStatus object receiver) =
    createPNSimpleConditionNo subject verbStatus object receiver date
notSimpleConditionToPN (SimConFourNH id subject modalVerb verb object receiver date) = 
    PNStatementCondition (createPNSimpleStatementNo subject modalVerb verb object receiver date)
notSimpleConditionToPN (SimConFiveNH id (BoolEx subject1 verbStatus comparison subject2)) =
    booleanExpressionToPNNo subject1 verbStatus comparison subject2

createPNSimpleStatementYes :: Subject -> ModalVerb -> Verb -> Object -> Receiver -> Date -> PNStatement
createPNSimpleStatementYes subject modalVerb verb object receiver date
    = PNTemporalActionStatement TRUE subject (modalVerbToPN modalVerb) verb object receiver date

createPNSimpleStatementNo :: Subject -> ModalVerb -> Verb -> Object -> Receiver -> Date -> PNStatement
createPNSimpleStatementNo subject modalVerb verb object receiver date
    = PNTemporalActionStatement FALSE subject (modalVerbToPN modalVerb) verb object receiver date

modalVerbToPN :: ModalVerb -> PNModalVerb
modalVerbToPN (ModalObli _) = PNSHALL
modalVerbToPN (ModalPermi) = PNMAY
modalVerbToPN (ModalForbi) = PNSHANT

createPNSimpleConditionYes :: Subject -> VerbStatus -> Object -> Receiver -> Date -> PNCondition
createPNSimpleConditionYes subject verbStatus object receiver date
    = PNTemporalActionCondition TRUE subject verbStatus object receiver date

createPNSimpleConditionNo :: Subject -> VerbStatus -> Object -> Receiver -> Date -> PNCondition
createPNSimpleConditionNo subject verbStatus object receiver date
    = PNTemporalActionCondition FALSE subject verbStatus object receiver date

booleanExpressionToPNYes :: Subject -> VerbStatus -> Comparison -> Subject -> PNCondition
booleanExpressionToPNYes subject1 verbStatus comparison subject2
    = PNExpressionCondition TRUE subject1 verbStatus comparison subject2

booleanExpressionToPNNo :: Subject -> VerbStatus -> Comparison -> Subject -> PNCondition
booleanExpressionToPNNo subject1 verbStatus comparison subject2
    = PNExpressionCondition FALSE subject1 verbStatus comparison subject2

printPNContract :: ([PNStatement], [PNConditionalStatement], [PNDefinition], [PNConditionalDefinition]) -> String
printPNContract (pnStatements, pnConditionalStatements, pnDefinitions, pnConditionalDefinitions)
    = "con = Contract()\n\n" ++ 
      concatMap (printStatementCall . printPNStatement) pnStatements ++
      concatMap (printStatementCall . printPNConditionalStatement) pnConditionalStatements ++ 
      concatMap (printDefinitionCall . printPNDefinition) pnDefinitions ++
      concatMap (printDefinitionCall . printPNConditionalDefinition) pnConditionalDefinitions ++
      "con.interactiveSimulation()"

printStatementCall :: String -> String
printStatementCall statement = "con.statement(" ++ statement ++ ")\n"

printDefinitionCall :: String -> String
printDefinitionCall definition = "con.definition(" ++ definition ++ ")\n"

printSubject :: Subject -> String
printSubject (SubQuoted str) = "\"" ++ str ++ "\"" 
printSubject (SubUnQuoted (Ident str)) = "\"" ++ str ++ "\"" 

printModalVerb :: PNModalVerb -> String
printModalVerb PNSHALL = "SHALL"
printModalVerb PNSHANT = "SHANT"
printModalVerb PNMAY = "MAY"

printVerb :: Verb -> String
printVerb VDel = "Deliver"
printVerb VPay = "Pay"
printVerb VCharge = "Charge"
printVerb VRefund = "Refund"

printObject :: Object -> String
printObject (ObjNu (NumPound _ (NumInt num))) = "£" ++ show num
printObject (ObjNu (NumDol _ (NumInt num))) = "$" ++ show num
printObject (ObjNu (NumEur _ (NumInt num))) = "€" ++ show num
printObject (ObjNu (NumAmount subject)) = "Amount \"" ++ printSubject subject ++ "\""
printObject (ObjNonNu (NonNumCurr subject)) = "SomeCurrency \"" ++ printSubject subject ++ "\""
printObject (ObjNonNu (NonNumRep subject)) = "Report \"" ++ printSubject subject ++ "\""
printObject (ObjNonNu (NonNumNamed subject)) = "NamedObject \"" ++ printSubject subject ++ "\""
printObject (ObjNonNu (NonNumOther subject)) = "OtherObject \"" ++ printSubject subject ++ "\""

printDate :: Date -> String
printDate (DateSpe specificDate) = "TemporalExpression('ON', '" ++ printSpecificDate specificDate ++ "')"
printDate (DateAny) = "TemporalExpression('On', 'ANYDATE')"
printDate (DateSome subject) = "TemporalExpression('ON', 'SOMEDATE " ++ printSubject subject ++ "')"
printDate (DateThe subject) = "TemporalExpression('ON', 'THEDATE " ++ printSubject subject ++ "')"
printDate (DateQuanSpecific temporalQuantifier day month year)
    = "TemporalExpression('" ++ printTemporalQuantifier temporalQuantifier ++ "', '" ++ dateSpeToString day month year ++ "')"
printDate (DateQuanSome temporalQuantifier subject)
    = "TemporalExpression('" ++ printTemporalQuantifier temporalQuantifier ++ "', 'SOMEDATE " ++ printSubject subject ++ "')"
printDate (DateQuanThe temporalQuantifier subject)
    = "TemporalExpression('" ++ printTemporalQuantifier temporalQuantifier ++ "', 'THEDATE " ++ printSubject subject ++ "')"
printDate (DateQuanSomeWO temporalOffset temporalQuantifier subject)
    = "TemporalExpression('ON', '" ++ printTemporalOffset temporalOffset ++ " " 
      ++ printTemporalQuantifier temporalQuantifier ++ " SOMEDATE " ++ printSubject subject ++ "')"
printDate (DateQuanTheWO temporalOffset temporalQuantifier subject)
    = "TemporalExpression('ON', '" ++ printTemporalOffset temporalOffset ++ " " 
      ++ printTemporalQuantifier temporalQuantifier ++ " THEDATE " ++ printSubject subject ++ "')"
printDate (DateQuanTempSome tq temporalOffset temporalQuantifier subject)
    = "TemporalExpression('" ++ printTemporalQuantifier tq ++ "', '" ++ printTemporalOffset temporalOffset ++ " " 
      ++ printTemporalQuantifier temporalQuantifier ++ " SOMEDATE " ++ printSubject subject ++ "')"
printDate (DateQuanTempThe tq temporalOffset temporalQuantifier subject)
    = "TemporalExpression('" ++ printTemporalQuantifier tq ++ "', '" ++ printTemporalOffset temporalOffset ++ " " 
      ++ printTemporalQuantifier temporalQuantifier ++ " THEDATE " ++ printSubject subject ++ "')"

printTemporalOffset :: TemporalOffset -> String
printTemporalOffset (TempOffDay num) = show num ++ " day"
printTemporalOffset (TempOffYear num) = show num ++ " year"
printTemporalOffset (TempOffWeek num) = show num ++ " week"
printTemporalOffset (TempOffDays num) = show num ++ " days"
printTemporalOffset (TempOffYears num) = show num ++ " years"
printTemporalOffset (TempOffWeeks num) = show num ++ " weeks"

printSpecificDate :: SpecificDate -> String
printSpecificDate (DateSpeOnThe day month year) = dateSpeToString day month year
printSpecificDate (DateSpeOn day month year) = dateSpeToString day month year

printTemporalQuantifier :: TemporalQuantifier -> String
printTemporalQuantifier (TempBefore) = "BEFORE"
printTemporalQuantifier (TempAfter) = "AFTER"

printReceiver :: Receiver -> String
printReceiver (Rec subject) = "to " ++ printSubject subject

printTest :: PNTest -> String
printTest (TRUE) = "True"
printTest (FALSE) = "False"

printVerbStatus :: VerbStatus -> String
printVerbStatus (VSDel) = "Delivered"
printVerbStatus (VSPay) = "Paid"
printVerbStatus (VSCharge) = "Charged"
printVerbStatus (VSRefund) = "Refunded"

printComparison :: Comparison -> String
printComparison (CompareLess) = "LessThan"
printComparison (CompareEq _) = "EqualTo"
printComparison (CompareMore _) = "MoreThan"

printNumericalExpression :: NumericalExpression -> String
printNumericalExpression (NumExpNum (NumInt n)) = show n 
printNumericalExpression (NumExpObj numObj) = numericalObjectToString numObj
printNumericalExpression (NumExpOp expr1 operator expr2) =
    let str1 = printNumericalExpression expr1
        str2 = printNumericalExpression expr2
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
numericalObjectToString (NumAmount subject) = printSubject subject

printPNStatement :: PNStatement -> String
printPNStatement (PNTemporalActionStatement pnTest subject pnModalVerb verb object receiver date) 
    = "TemporalStatement('" ++ printSubject subject ++ "', '" ++ printModalVerb pnModalVerb ++ "', '" ++
      printVerb verb ++ "', '" ++ printObject object ++ "', '" ++ printReceiver receiver ++ "', " ++ printDate date
      ++ ", valid=" ++ printTest pnTest ++ ")"

printPNConditionalStatement :: PNConditionalStatement -> String
printPNConditionalStatement (PNConditionalStatement pnCondition pnStatement)
    = "ConditionalStatement(condition=" ++ printPNCondition pnCondition ++ ", statement=" ++ printPNStatement pnStatement ++ ")"

printPNCondition :: PNCondition -> String
printPNCondition (PNStatementCondition pnStatement) = 
      "StatementCondition(statement=" ++ printPNStatement pnStatement ++ ", test=" ++ printTest pnTest ++ ")"
    where
      PNTemporalActionStatement pnTest subject pnModalVerb verb object receiver date = pnStatement
printPNCondition (PNTemporalActionCondition pnTest subject verbStatus object receiver date) =
    "TemporalActionCondition('" ++ printSubject subject ++ "', '" ++ printVerbStatus verbStatus ++ "', '" ++
    printObject object ++ "', '" ++ printReceiver receiver ++ "', " ++ printDate date ++ ", test=" ++
    printTest pnTest ++ ")"
printPNCondition (PNExpressionCondition pnTest subject1 verbStatus comparison subject2) =
    "ExpressionCondition(BooleanExpression('" ++ printSubject subject1 ++ "', '" ++ printVerbStatus verbStatus ++
    "', '" ++ printComparison comparison ++ "', '" ++ printSubject subject2 ++ "'), test=" ++ printTest pnTest ++ ")"
printPNCondition (PNAndCondition pnConditions) =
    "AndCondition(conditions=[" ++ intercalate ",\n\t\t" (map printPNCondition pnConditions) ++ "])"
printPNCondition (PNOrCondition pnConditions) =
    "OrCondition(conditions=[" ++ intercalate ",\n\t\t" (map printPNCondition pnConditions) ++ "])"

printPNDefinition :: PNDefinition -> String
printPNDefinition (PNIsDefinition subject1 subject2) =
    "IsDefinition('" ++ printSubject subject1 ++ "', '" ++ printSubject subject2 ++ "')"
printPNDefinition (PNEqualsDefinition subject numericalExpression) =
    "EqualsDefinition('" ++ printSubject subject ++ "', '" ++ printNumericalExpression numericalExpression ++ "')"
printPNDefinition (PNDefAnd pnDefinitions) =
    "[" ++ intercalate ",\n\t\t" (map printPNDefinition pnDefinitions) ++ "]"

printPNConditionalDefinition :: PNConditionalDefinition -> String
printPNConditionalDefinition (PNConditionalDefinition pnCondition pnDefinition)
    = "ConditionalDefinition(condition=" ++ printPNCondition pnCondition ++ ", definitions=" ++ 
      printPNDefinition pnDefinition ++ ")\n"

runPetriNetConversion :: Contract -> IO()
runPetriNetConversion contract = putStrLn $ printPNContract $ contractToPN contract


