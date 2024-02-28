module Test.FolToTptpTest where

import Test.QuickCheck
import qualified Main as CoLaParser
import FOLToTPTP
import AbsCoLa 
import ExampleContracts

prop_convertToTPTP :: String -> String -> Property
prop_convertToTPTP input expectedTPTPFormula =
    runTptpConversionContract (CoLaParser.parseContract input) === expectedTPTPFormula

emptyContractTPTP = "fof(contract, axiom, (Empty(Empty)))."
simpleDefinitionTPTP = "fof(contract, axiom, (PartyA = Alice))."
andDefinitionTPTP = "fof(contract, axiom, ((PartyA = Alice) & ((AmountA = Pound100) & (DateA = 60338))))."
conditionalDefinitionTPTP = "fof(contract, axiom, ((! [X,Y,O,D] : (((((name(X,Kelly)) & (name(Y,Carol))) & (date(D,60310))) & (objectAmount(O,rental))) & (mustChargeBefore(X,Y,O,D)))) => (PartyA = Carol)))."
simpleStatementTPTP = "fof(contract, axiom, (~ (! [X,Y,O] : (? [D] : ((((name(X,Eddie)) & (name(Y,Frank))) & (objectAmount(O,extra))) & (mayCharge(X,Y,O,D)))))))."
andStatementTPTP = "fof(contract, axiom, ((~ (! [X,Y,O] : (? [D] : ((((name(X,Dennis)) & (name(Y,Katherine))) & (objectPound(O,20))) & (mayCharge(X,Y,O,D)))))) & (! [X,Y,O,D] : (((((name(X,Matthew)) & (name(Y,Eric))) & (date(D,60160))) & (objectOtherObject(O,scarf))) & (mustDeliver(X,Y,O,D))))))."
orStatementTPTP = "fof(contract, axiom, ((! [X,Y,O,D] : (((((name(X,Alice)) & (name(Y,Bob))) & (date(D,60196))) & (objectPound(O,20))) & (mustPay(X,Y,O,D)))) | (! [X,Y,O,D] : (((((name(X,Alice)) & (name(Y,Bob))) & (date(D,60196))) & (objectDollar(O,30))) & (mustPay(X,Y,O,D))))))."
conditionalStatementTPTP = "fof(contract, axiom, (! [D1,D2] : (((! [X,Y,O] : ((((name(X,Helen)) & (name(Y,George))) & (objectPound(O,12))) & (paid(X,Y,O,D1)))) => (! [X,Y,O] : (((((name(X,George)) & (name(Y,Helen))) & (isDate(D1))) & (objectOtherObject(O,charger))) & (mustDeliverAfter(X,Y,O,D1))))) & ((~ (! [X,Y,O] : ((((name(X,Helen)) & (name(Y,George))) & (objectPound(O,12))) & (paid(X,Y,O,D1))))) => (! [X,Y,O] : (((((name(X,George)) & (name(Y,Helen))) & (isDate(D2))) & (objectPound(O,2))) & (mayCharge(X,Y,O,D2))))))))."

consistencyContract1TPTP = "fof(contract, axiom, ((! [X,Y,O,D] : (((((name(X,Carol)) & (name(Y,David))) & (date(D,60207))) & (objectEuro(O,30))) & (paidBefore(X,Y,O,D)))) => (! [X,Y,O,D] : (((((name(X,David)) & (name(Y,Carol))) & (date(D,60214))) & (objectOtherObject(O,orange))) & (mustDeliverBefore(X,Y,O,D))))))."
consistencyContract2TPTP = "fof(contract, axiom, ((! [X,Y,O,D] : (((((name(X,Emily)) & (name(Y,Frank))) & (date(D,60278))) & (objectDollar(O,45))) & (paid(X,Y,O,D)))) => (! [X,Y,O] : (? [D] : ((((name(X,Frank)) & (name(Y,Emily))) & (objectOtherObject(O,notebook))) & (mustDeliver(X,Y,O,D)))))))."
consistencyContract3TPTP = "fof(contract, axiom, (! [D1] : ((! [X,Y,O,D] : (((((name(X,Grace)) & (name(Y,Helena))) & (date(D,59401))) & (objectPound(O,20))) & (refunded(X,Y,O,D)))) => (! [X,Y,O] : (((((name(X,Helena)) & (name(Y,Grace))) & (isDate(D1))) & (objectReport(O,financialreport))) & (mustDeliver(X,Y,O,D1)))))))."
consistencyContract4TPTP = "fof(contract, axiom, (! [D1] : ((! [X,Y,O] : (((((name(X,Ivana)) & (name(Y,Jess))) & (isDate(D1))) & (objectDollar(O,13))) & (chargedBefore(X,Y,O,D1)))) => (! [X,Y,O] : (((((name(X,Ivana)) & (name(Y,Jess))) & (isDate(D1))) & (objectOtherObject(O,dress))) & (mustDeliverAfter(X,Y,O,D1)))))))."
consistencyContract5TPTP = "fof(contract, axiom, (! [D1] : ((! [X,Y,O] : (((((name(X,Kathy)) & (name(Y,Lily))) & (isDate(D1))) & (objectEuro(O,3))) & (paid(X,Y,O,D1)))) => (! [X,Y,O,D] : ((((((name(X,Lily)) & (name(Y,Kathy))) & (isDate(D1))) & (dateAfter(D,D1,7))) & (objectOtherObject(O,bicycle))) & (mustDeliverBefore(X,Y,O,D)))))))."
consistencyContract6TPTP = "fof(contract, axiom, (! [D1] : (((! [X,Y,O,D] : (((((name(X,Mandy)) & (name(Y,Nancy))) & (date(D,59311))) & (objectPound(O,17))) & (paid(X,Y,O,D)))) => (! [X,Y,O] : (((((name(X,Nancy)) & (name(Y,Mandy))) & (isDate(D1))) & (objectOtherObject(O,bag))) & (mustDeliver(X,Y,O,D1))))) & (unknownOne = 59313))))."
consistencyContract7TPTP = "fof(contract, axiom, ((((! [X,Y,O,D] : (((((name(X,Alice)) & (name(Y,Bob))) & (date(D,59305))) & (objectPound(O,100))) & (paid(X,Y,O,D)))) | (! [X,Y,O,D] : (((((name(X,Alice)) & (name(Y,Bob))) & (date(D,59305))) & (objectDollar(O,120))) & (paid(X,Y,O,D))))) => (! [X,Y,O,D] : (((((name(X,Bob)) & (name(Y,Alice))) & (date(D,59309))) & (objectOtherObject(O,bicycle))) & (mustDeliver(X,Y,O,D))))) & ((~ ((! [X,Y,O,D] : (((((name(X,Alice)) & (name(Y,Bob))) & (date(D,59305))) & (objectPound(O,100))) & (paid(X,Y,O,D)))) | (! [X,Y,O,D] : (((((name(X,Alice)) & (name(Y,Bob))) & (date(D,59305))) & (objectDollar(O,120))) & (paid(X,Y,O,D)))))) => (! [X,Y,O,D] : (((((name(X,Bob)) & (name(Y,Alice))) & (date(D,59309))) & (objectOtherObject(O,orange))) & (mustDeliver(X,Y,O,D)))))))."

bikeDeliveryOriginalTPTP = "fof(contract, axiom, ((((! [X,Y,O,D] : (((((name(X,Alice)) & (name(Y,Bob))) & (date(D,59305))) & (objectPound(O,100))) & (paid(X,Y,O,D)))) | (! [X,Y,O,D] : (((((name(X,Alice)) & (name(Y,Bob))) & (date(D,59305))) & (objectDollar(O,120))) & (paid(X,Y,O,D))))) => (! [X,Y,O,D] : (((((name(X,Bob)) & (name(Y,Alice))) & (date(D,59309))) & (objectOtherObject(O,bicycle))) & (mustDeliver(X,Y,O,D))))) & ((! [X,Y,O] : (? [D] : ((((name(X,Bob)) & (name(Y,Alice))) & (objectReport(O,receipt))) & (mayDeliver(X,Y,O,D))))) & (~ (! [X,Y,O] : (? [D] : ((((name(X,Bob)) & (name(Y,Alice))) & (objectAmount(O,deliveryfee))) & (mayCharge(X,Y,O,D)))))))))."
bikeDeliveryModifiedTPTP = "fof(contract, axiom, (((! [X,Y,O,D] : (((((name(X,Alice)) & (name(Y,Bob))) & (date(D,59305))) & (objectPound(O,100))) & (paid(X,Y,O,D)))) | (! [X,Y,O,D] : (((((name(X,Alice)) & (name(Y,Bob))) & (date(D,59305))) & (objectDollar(O,120))) & (paid(X,Y,O,D))))) => ((! [X,Y,O,D] : (((((name(X,Bob)) & (name(Y,Alice))) & (date(D,59309))) & (objectOtherObject(O,bicycle))) & (mustDeliver(X,Y,O,D)))) & ((! [X,Y,O] : (? [D] : ((((name(X,Bob)) & (name(Y,Alice))) & (objectReport(O,receipt))) & (mayDeliver(X,Y,O,D))))) & (~ (! [X,Y,O] : (? [D] : ((((name(X,Bob)) & (name(Y,Alice))) & (objectAmount(O,deliveryfee))) & (mayCharge(X,Y,O,D))))))))))."
bikeDeliverySanctionTPTP = "fof(contract, axiom, ((((! [X,Y,O,D] : (((((name(X,Alice)) & (name(Y,Bob))) & (date(D,59305))) & (objectPound(O,100))) & (paid(X,Y,O,D)))) | (! [X,Y,O,D] : (((((name(X,Alice)) & (name(Y,Bob))) & (date(D,59305))) & (objectDollar(O,120))) & (paid(X,Y,O,D))))) => ((! [X,Y,O,D] : (((((name(X,Bob)) & (name(Y,Alice))) & (date(D,59309))) & (objectOtherObject(O,bicycle))) & (mustDeliver(X,Y,O,D)))) & (~ (! [X,Y,O] : (? [D] : ((((name(X,Bob)) & (name(Y,Alice))) & (objectAmount(O,deliveryfee))) & (mayCharge(X,Y,O,D)))))))) & ((((! [X,Y,O,D] : (((((name(X,Bob)) & (name(Y,Alice))) & (date(D,59309))) & (objectOtherObject(O,bicycle))) & (delivered(X,Y,O,D)))) & (~ (! [X,Y,O] : (? [D] : ((((name(X,Bob)) & (name(Y,Alice))) & (objectAmount(O,deliveryfee))) & (charged(X,Y,O,D))))))) => (! [X,Y,O] : (? [D] : ((((name(X,Bob)) & (name(Y,Alice))) & (objectReport(O,receipt))) & (mayDeliver(X,Y,O,D)))))) & ((~ (! [X,Y,O,D] : (((((name(X,Bob)) & (name(Y,Alice))) & (date(D,59309))) & (objectOtherObject(O,bicycle))) & (delivered(X,Y,O,D))))) => (! [X,Y,O,D] : (((((name(X,Alice)) & (name(Y,Bob))) & (date(D,59312))) & (objectPound(O,100))) & (mayCharge(X,Y,O,D))))))))."
isdaOriginalTPTP = "fof(contract, axiom, (! [D1] : ((((! [X,Y,O] : (((((name(X,PartyA)) & (name(Y,PartyB))) & (isDate(D1))) & (objectAmount(O,AmountA))) & (mustPay(X,Y,O,D1)))) & (! [X,Y,O] : ((((name(X,PartyB)) & (name(Y,PartyA))) & (objectAmount(O,AmountB))) & (mustPay(X,Y,O,D1))))) => ((~ (! [X,Y,O] : ((((name(X,PartyA)) & (name(Y,PartyB))) & (objectAmount(O,AmountA))) & (mustPay(X,Y,O,D1))))) & (~ (! [X,Y,O] : ((((name(X,PartyB)) & (name(Y,PartyA))) & (objectAmount(O,AmountB))) & (mustPay(X,Y,O,D1))))))) & ((! [X,Y,O] : ((((name(X,ExcessParty)) & (name(Y,AnotherParty))) & (objectAmount(O,ExcessAmount))) & (mustPay(X,Y,O,D1)))) & (((! [X,Y] : (((name(X,PartyA)) & (name(Y,PartyB))) & (paidMore(X,Y)))) => ((ExcessParty = PartyA) & (ExcessAmount = minus(AmountA,AmountB)))) & ((! [X,Y] : (((name(X,PartyB)) & (name(Y,PartyA))) & (paidMore(X,Y)))) => ((ExcessParty = PartyB) & (ExcessAmount = minus(AmountB,AmountA)))))))))."
isdaModifiedTPTP = "fof(contract, axiom, (! [D1] : ((((! [X,Y,O,D] : (((((name(X,PartyA)) & (name(Y,PartyB))) & (date(D,40587))) & (objectAmount(O,AmountA))) & (mustPay(X,Y,O,D)))) & (! [X,Y,O,D] : (((((name(X,PartyB)) & (name(Y,PartyA))) & (date(D,40587))) & (objectAmount(O,AmountB))) & (mustPay(X,Y,O,D))))) => ((~ (! [X,Y,O,D] : (((((name(X,PartyA)) & (name(Y,PartyB))) & (date(D,40587))) & (objectAmount(O,AmountA))) & (mustPay(X,Y,O,D))))) & ((~ (! [X,Y,O,D] : (((((name(X,PartyB)) & (name(Y,PartyA))) & (date(D,40587))) & (objectAmount(O,AmountB))) & (mustPay(X,Y,O,D))))) & (! [X,Y,O] : ((((name(X,Excessparty)) & (name(Y,AnotherParty))) & (objectAmount(O,ExcessAmount))) & (mustPay(X,Y,O,D1))))))) & (((! [X,Y] : (((name(X,PartyA)) & (name(Y,PartyB))) & (paidMore(X,Y)))) => ((ExcessParty = PartyA) & (ExcessAmount = minus(AmountA,AmountB)))) & ((! [X,Y] : (((name(X,PartyB)) & (name(Y,PartyA))) & (paidMore(X,Y)))) => ((ExcessParty = PartyA) & (ExcessAmount = minus(AmountB,AmountA))))))))."
guarantorTPTP = "fof(contract, axiom, ((! [X,Y,O,D] : (((((name(X,Landlord)) & (name(Y,Tenant))) & (date(D,60036))) & (objectOtherObject(O,property))) & (mustDeliver(X,Y,O,D)))) & ((((! [X,Y,O,D] : (((((name(X,Landlord)) & (name(Y,Tenant))) & (date(D,60036))) & (objectOtherObject(O,demandOfTenantPayment))) & (delivered(X,Y,O,D)))) & (~ (! [X,Y,O,D] : (((((name(X,Tenant)) & (name(Y,Landlord))) & (date(D,60039))) & (objectAmount(O,AmountA))) & (paidBefore(X,Y,O,D)))))) => (! [X,Y,O,D] : (((((name(X,Landlord)) & (name(Y,Tenant))) & (date(D,60040))) & (objectOtherObject(O,demandOfGuarantorPayment))) & (mayDeliver(X,Y,O,D))))) & ((((! [X,Y,O,D] : (((((name(X,Landlord)) & (name(Y,Tenant))) & (date(D,60036))) & (objectOtherObject(O,demandOfTenantPayment))) & (delivered(X,Y,O,D)))) & ((~ (! [X,Y,O,D] : (((((name(X,Tenant)) & (name(Y,Landlord))) & (date(D,60044))) & (objectAmount(O,AmountA))) & (paidBefore(X,Y,O,D))))) & (! [X,Y,O,D] : (((((name(X,Landlord)) & (name(Y,Tenant))) & (date(D,60040))) & (objectOtherObject(O,demandOfGuarantorPayment))) & (delivered(X,Y,O,D)))))) => (! [X,Y,O,D] : (((((name(X,Guarantor)) & (name(Y,Landlord))) & (date(D,60045))) & (objectAmount(O,AmountA))) & (mustPay(X,Y,O,D))))) & (((~ (! [X,Y,O,D] : (((((name(X,Tenant)) & (name(Y,Landlord))) & (date(D,60198))) & (objectAmount(O,AmountB))) & (paidBefore(X,Y,O,D))))) => (! [X,Y,O,D] : (((((name(X,Guarantor)) & (name(Y,Landlord))) & (date(D,60199))) & (objectAmount(O,AmountB))) & (mustPay(X,Y,O,D))))) & (((! [X,Y,O,D] : (((((name(X,HousingBenefitScheme)) & (name(Y,Tenant))) & (date(D,60310))) & (objectAmount(O,AmountC))) & (paid(X,Y,O,D)))) & (! [X,Y,O,D] : (((((name(X,LocalAuthority)) & (name(Y,Tenant))) & (date(D,60316))) & (objectOtherObject(O,overpaymentClaim))) & (delivered(X,Y,O,D))))) => (! [X,Y,O,D] : (((((name(X,Guarantor)) & (name(Y,Landlord))) & (date(D,60319))) & (objectAmount(O,AmountC))) & (mustPayBefore(X,Y,O,D))))))))))."

tptpSampleTests :: [(String, String)]
tptpSampleTests = 
    [(emptyContract, emptyContractTPTP)
    , (simpleDefinition1, simpleDefinitionTPTP)
    , (andDefinition4, andDefinitionTPTP)
    , (conditionalDefinition8, conditionalDefinitionTPTP)
    , (simpleStatement3, simpleStatementTPTP)
    , (andStatement3, andStatementTPTP)
    , (orStatement1, orStatementTPTP)
    , (conditionalStatement8, conditionalStatementTPTP)
    , (consistencyContract1, consistencyContract1TPTP)
    , (consistencyContract2, consistencyContract2TPTP)
    , (consistencyContract3, consistencyContract3TPTP)
    , (consistencyContract4, consistencyContract4TPTP)
    , (consistencyContract5, consistencyContract5TPTP)
    , (consistencyContract6, consistencyContract6TPTP)
    , (consistencyContract7, consistencyContract7TPTP)
    , (bikeDeliveryOriginal, bikeDeliveryOriginalTPTP)
    , (bikeDeliveryModified, bikeDeliveryModifiedTPTP)
    , (bikeDeliverySanction, bikeDeliverySanctionTPTP)
    , (isdaOriginal, isdaOriginalTPTP)
    , (isdaModified, isdaModifiedTPTP)
    , (guarantor, guarantorTPTP)]

tptpConversionTest :: IO ()
tptpConversionTest = do
    putStrLn "Running QuickCheck tests for FOL to TPTP converter ..."
    quickCheck $ forAll (elements tptpSampleTests) (\(input, expected) -> prop_convertToTPTP input expected)
    putStrLn "Done."