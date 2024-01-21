module Cola.Test.CoLaParserTest where

import Test.QuickCheck
import qualified Main as CoLaParser
import AbsCoLa 
import ExampleContracts

prop_parseSentence :: String -> Contract -> Property
prop_parseSentence input expectedContract =
    CoLaParser.parseSentence input === expectedContract

bikeDeliveryAST = ConAnd (ComConState (ConStateIfThen (CondiOr (SimConOne (IDSim (NumInt 1)) HoldYes (SubUnQuoted (Ident "Alice")) VSPay (ObjNu (NumPound PoundTwo (NumInt 100))) (Rec (SubUnQuoted (Ident "Bob"))) (DateSpe (DateSpeOnThe (NumInt 1) MApr (NumInt 2021)))) (CondiSim (SimConOne (IDSim (NumInt 2)) HoldYes (SubUnQuoted (Ident "Alice")) VSPay (ObjNu (NumDol DollarTwo (NumInt 120))) (Rec (SubUnQuoted (Ident "Bob"))) (DateSpe (DateSpeOnThe (NumInt 1) MApr (NumInt 2021)))))) (StateSim (SimStateOne (IDSim (NumInt 3)) HoldYes (SubUnQuoted (Ident "Bob")) (ModalObli ObliTwo) VDel (ObjNonNu (NonNumOther (SubUnQuoted (Ident "bicycle")))) (Rec (SubUnQuoted (Ident "Alice"))) (DateSpe (DateSpeOnThe (NumInt 5) MApr (NumInt 2021))))))) (ConComp (ComState (StateAnd (SimStateOne (IDSim (NumInt 4)) HoldYes (SubUnQuoted (Ident "Bob")) ModalPermi VDel (ObjNonNu (NonNumRep (SubUnQuoted (Ident "receipt")))) (Rec (SubUnQuoted (Ident "Alice"))) DateAny) (StateSim (SimStateOne (IDSim (NumInt 5)) HoldYes (SubUnQuoted (Ident "Bob")) ModalForbi VCharge (ObjNu (NumAmount (SubQuoted "delivery fee"))) (Rec (SubUnQuoted (Ident "Alice"))) DateAny)))))

isdaOriginalAST = ConAnd (ComConState (ConStateIfThen (CondiAnd (SimConFour (IDSim (NumInt 1)) HoldYes (SubUnQuoted (Ident "PartyA")) (ModalObli ObliOne) VPay (ObjNu (NumAmount (SubUnQuoted (Ident "AmountA")))) (Rec (SubUnQuoted (Ident "PartyB"))) (DateSome (SubUnQuoted (Ident "unknownOne")))) (CondiSim (SimConFour (IDSim (NumInt 2)) HoldYes (SubUnQuoted (Ident "PartyB")) (ModalObli ObliOne) VPay (ObjNu (NumAmount (SubUnQuoted (Ident "AmountB")))) (Rec (SubUnQuoted (Ident "PartyA"))) (DateThe (SubUnQuoted (Ident "unknownOne")))))) (StateAnd (SimStateOne (IDSim (NumInt 3)) HoldNo (SubUnQuoted (Ident "PartyA")) (ModalObli ObliOne) VPay (ObjNu (NumAmount (SubUnQuoted (Ident "AmountA")))) (Rec (SubUnQuoted (Ident "PartyB"))) (DateThe (SubUnQuoted (Ident "unknownOne")))) (StateSim (SimStateOne (IDSim (NumInt 4)) HoldNo (SubUnQuoted (Ident "PartyB")) (ModalObli ObliOne) VPay (ObjNu (NumAmount (SubUnQuoted (Ident "AmountB")))) (Rec (SubUnQuoted (Ident "PartyA"))) (DateThe (SubUnQuoted (Ident "unknownOne")))))))) (ConAnd (ComState (StateSim (SimStateOne (IDSim (NumInt 5)) HoldYes (SubUnQuoted (Ident "ExcessParty")) (ModalObli ObliOne) VPay (ObjNu (NumAmount (SubUnQuoted (Ident "ExcessAmount")))) (Rec (SubUnQuoted (Ident "AnotherParty"))) (DateThe (SubUnQuoted (Ident "unknownOne")))))) (ConAnd (ComConDef (ConDefIfThen (CondiSim (SimConFive (IDSim (NumInt 6)) HoldYes (BoolEx (SubUnQuoted (Ident "PartyA")) VSPay (CompareMore MoreOne) (SubUnQuoted (Ident "PartyB"))))) (DefAnd (SimDefIs (IDSim (NumInt 7)) (SubUnQuoted (Ident "ExcessParty")) (SubUnQuoted (Ident "PartyA"))) (DefSim (SimDefEq (IDSim (NumInt 8)) (SubUnQuoted (Ident "ExcessAmount")) (NumExpOp (NumExpObj (NumAmount (SubUnQuoted (Ident "AmountA")))) OpMin (NumExpObj (NumAmount (SubUnQuoted (Ident "AmountB")))))))))) (ConComp (ComConDef (ConDefIfThen (CondiSim (SimConFive (IDSim (NumInt 9)) HoldYes (BoolEx (SubUnQuoted (Ident "PartyB")) VSPay (CompareMore MoreOne) (SubUnQuoted (Ident "PartyA"))))) (DefAnd (SimDefIs (IDSim (NumInt 10)) (SubUnQuoted (Ident "ExcessParty")) (SubUnQuoted (Ident "PartyB"))) (DefSim (SimDefEq (IDSim (NumInt 11)) (SubUnQuoted (Ident "ExcessAmount")) (NumExpOp (NumExpObj (NumAmount (SubUnQuoted (Ident "AmountB")))) OpMin (NumExpObj (NumAmount (SubUnQuoted (Ident "AmountA")))))))))))))

sampleTests :: [(String, Contract)]
sampleTests =
    [ (bikeDeliveryOriginal, bikeDeliveryAST)
    , (isdaOriginal, isdaOriginalAST)
    ]

main :: IO ()
main = do
    putStrLn "Running QuickCheck tests..."
    quickCheck $ forAll (elements sampleTests) (\(input, expected) -> prop_parseSentence input expected)
    putStrLn "Done."