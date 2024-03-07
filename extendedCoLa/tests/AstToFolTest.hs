module Test.AstToFolTest where
    
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Main as CoLaParser
import ContractAnalysis.AstToFol
import Parser.AbsCoLa 
import Helper.ExampleContracts

prop_convertToFOL :: String -> FOLFormula -> Property
prop_convertToFOL input expectedFOLFormula =
    runFOLConversion (CoLaParser.parseContract input) === expectedFOLFormula

prop_correctTempQuanDictionary :: String -> TempQuanDictionary -> Property
prop_correctTempQuanDictionary input expectedDict =
    getTempQuanDictAfterConversion (CoLaParser.parseContract input) === expectedDict

emptyContractFOL = Brackets (Pred "Empty" [Var "Empty"])
simpleDefinitionFOL = Equal (Var "PartyA") (Var "Alice")
andDefinitionFOL = And (Equal (Var "PartyA") (Var "Alice")) (And (Equal (Var "AmountA") (Var "Pound100")) (Equal (Var "DateA") (Var "60338")))
conditionalDefinitionFOL = Implies (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Kelly"]) (Pred "name" [Var "Y",Var "Carol"])) (Pred "date" [Var "D",Var "60310"])) (Pred "objectAmount" [Var "O",Var "rental"])) (Pred "mustChargeBefore" [Var "X",Var "Y",Var "O",Var "D"])))) (Equal (Var "PartyA") (Var "Carol"))
simpleStatementFOL = Not (ForAll [Var "X",Var "Y",Var "O"] (Brackets (Exists [Var "D"] (Brackets (And (And (And (Pred "name" [Var "X",Var "Eddie"]) (Pred "name" [Var "Y",Var "Frank"])) (Pred "objectAmount" [Var "O",Var "extra"])) (Pred "mayCharge" [Var "X",Var "Y",Var "O",Var "D"]))))))
andStatementFOL = And (Not (ForAll [Var "X",Var "Y",Var "O"] (Brackets (Exists [Var "D"] (Brackets (And (And (And (Pred "name" [Var "X",Var "Dennis"]) (Pred "name" [Var "Y",Var "Katherine"])) (Pred "objectPound" [Var "O",Var "20"])) (Pred "mayCharge" [Var "X",Var "Y",Var "O",Var "D"]))))))) (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Matthew"]) (Pred "name" [Var "Y",Var "Eric"])) (Pred "date" [Var "D",Var "60160"])) (Pred "objectOtherObject" [Var "O",Var "scarf"])) (Pred "mustDeliver" [Var "X",Var "Y",Var "O",Var "D"]))))
orStatementFOL = Or (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Alice"]) (Pred "name" [Var "Y",Var "Bob"])) (Pred "date" [Var "D",Var "60196"])) (Pred "objectPound" [Var "O",Var "20"])) (Pred "mustPay" [Var "X",Var "Y",Var "O",Var "D"])))) (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Alice"]) (Pred "name" [Var "Y",Var "Bob"])) (Pred "date" [Var "D",Var "60196"])) (Pred "objectDollar" [Var "O",Var "30"])) (Pred "mustPay" [Var "X",Var "Y",Var "O",Var "D"]))))
conditionalStatementFOL = ForAll [Var "D1",Var "D2"] (Brackets (And (Implies (ForAll [Var "X",Var "Y",Var "O"] (Brackets (And (And (And (Pred "name" [Var "X",Var "Helen"]) (Pred "name" [Var "Y",Var "George"])) (Pred "objectPound" [Var "O",Var "12"])) (Pred "paid" [Var "X",Var "Y",Var "O",Var "D1"])))) (ForAll [Var "X",Var "Y",Var "O"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "George"]) (Pred "name" [Var "Y",Var "Helen"])) (Pred "isDate" [Var "D1"])) (Pred "objectOtherObject" [Var "O",Var "charger"])) (Pred "mustDeliverAfter" [Var "X",Var "Y",Var "O",Var "D1"]))))) (Implies (Not (ForAll [Var "X",Var "Y",Var "O"] (Brackets (And (And (And (Pred "name" [Var "X",Var "Helen"]) (Pred "name" [Var "Y",Var "George"])) (Pred "objectPound" [Var "O",Var "12"])) (Pred "paid" [Var "X",Var "Y",Var "O",Var "D1"]))))) (ForAll [Var "X",Var "Y",Var "O"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "George"]) (Pred "name" [Var "Y",Var "Helen"])) (Pred "isDate" [Var "D2"])) (Pred "objectPound" [Var "O",Var "2"])) (Pred "mayCharge" [Var "X",Var "Y",Var "O",Var "D2"])))))))

consistencyContract1FOL = Implies (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Carol"]) (Pred "name" [Var "Y",Var "David"])) (Pred "date" [Var "D",Var "60207"])) (Pred "objectEuro" [Var "O",Var "30"])) (Pred "paidBefore" [Var "X",Var "Y",Var "O",Var "D"])))) (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "David"]) (Pred "name" [Var "Y",Var "Carol"])) (Pred "date" [Var "D",Var "60214"])) (Pred "objectOtherObject" [Var "O",Var "orange"])) (Pred "mustDeliverBefore" [Var "X",Var "Y",Var "O",Var "D"]))))
consistencyContract2FOL = Implies (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Emily"]) (Pred "name" [Var "Y",Var "Frank"])) (Pred "date" [Var "D",Var "60278"])) (Pred "objectDollar" [Var "O",Var "45"])) (Pred "paid" [Var "X",Var "Y",Var "O",Var "D"])))) (ForAll [Var "X",Var "Y",Var "O"] (Brackets (Exists [Var "D"] (Brackets (And (And (And (Pred "name" [Var "X",Var "Frank"]) (Pred "name" [Var "Y",Var "Emily"])) (Pred "objectOtherObject" [Var "O",Var "notebook"])) (Pred "mustDeliver" [Var "X",Var "Y",Var "O",Var "D"]))))))
consistencyContract3FOL = ForAll [Var "D1"] (Brackets (Implies (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Grace"]) (Pred "name" [Var "Y",Var "Helena"])) (Pred "date" [Var "D",Var "59401"])) (Pred "objectPound" [Var "O",Var "20"])) (Pred "refunded" [Var "X",Var "Y",Var "O",Var "D"])))) (ForAll [Var "X",Var "Y",Var "O"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Helena"]) (Pred "name" [Var "Y",Var "Grace"])) (Pred "isDate" [Var "D1"])) (Pred "objectReport" [Var "O",Var "financial report"])) (Pred "mustDeliver" [Var "X",Var "Y",Var "O",Var "D1"]))))))
consistencyContract4FOL = ForAll [Var "D1"] (Brackets (Implies (ForAll [Var "X",Var "Y",Var "O"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Ivana"]) (Pred "name" [Var "Y",Var "Jess"])) (Pred "isDate" [Var "D1"])) (Pred "objectDollar" [Var "O",Var "13"])) (Pred "chargedBefore" [Var "X",Var "Y",Var "O",Var "D1"])))) (ForAll [Var "X",Var "Y",Var "O"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Ivana"]) (Pred "name" [Var "Y",Var "Jess"])) (Pred "isDate" [Var "D1"])) (Pred "objectOtherObject" [Var "O",Var "dress"])) (Pred "mustDeliverAfter" [Var "X",Var "Y",Var "O",Var "D1"]))))))
consistencyContract5FOL = ForAll [Var "D1"] (Brackets (Implies (ForAll [Var "X",Var "Y",Var "O"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Kathy"]) (Pred "name" [Var "Y",Var "Lily"])) (Pred "isDate" [Var "D1"])) (Pred "objectEuro" [Var "O",Var "3"])) (Pred "paid" [Var "X",Var "Y",Var "O",Var "D1"])))) (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (And (Pred "name" [Var "X",Var "Lily"]) (Pred "name" [Var "Y",Var "Kathy"])) (Pred "isDate" [Var "D1"])) (Pred "dateAfter" [Var "D",Var "D1",Var "7"])) (Pred "objectOtherObject" [Var "O",Var "bicycle"])) (Pred "mustDeliverBefore" [Var "X",Var "Y",Var "O",Var "D"]))))))
consistencyContract6FOL = ForAll [Var "D1"] (Brackets (And (Brackets (Implies (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Mandy"]) (Pred "name" [Var "Y",Var "Nancy"])) (Pred "date" [Var "D",Var "59311"])) (Pred "objectPound" [Var "O",Var "17"])) (Pred "paid" [Var "X",Var "Y",Var "O",Var "D"])))) (ForAll [Var "X",Var "Y",Var "O"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Nancy"]) (Pred "name" [Var "Y",Var "Mandy"])) (Pred "isDate" [Var "D1"])) (Pred "objectOtherObject" [Var "O",Var "bag"])) (Pred "mustDeliver" [Var "X",Var "Y",Var "O",Var "D1"])))))) (Brackets (Equal (Var "unknownOne") (Var "59313")))))
consistencyContract7FOL = And (Implies (Or (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Alice"]) (Pred "name" [Var "Y",Var "Bob"])) (Pred "date" [Var "D",Var "59305"])) (Pred "objectPound" [Var "O",Var "100"])) (Pred "paid" [Var "X",Var "Y",Var "O",Var "D"])))) (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Alice"]) (Pred "name" [Var "Y",Var "Bob"])) (Pred "date" [Var "D",Var "59305"])) (Pred "objectDollar" [Var "O",Var "120"])) (Pred "paid" [Var "X",Var "Y",Var "O",Var "D"]))))) (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Bob"]) (Pred "name" [Var "Y",Var "Alice"])) (Pred "date" [Var "D",Var "59309"])) (Pred "objectOtherObject" [Var "O",Var "bicycle"])) (Pred "mustDeliver" [Var "X",Var "Y",Var "O",Var "D"]))))) (Implies (Not (Or (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Alice"]) (Pred "name" [Var "Y",Var "Bob"])) (Pred "date" [Var "D",Var "59305"])) (Pred "objectPound" [Var "O",Var "100"])) (Pred "paid" [Var "X",Var "Y",Var "O",Var "D"])))) (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Alice"]) (Pred "name" [Var "Y",Var "Bob"])) (Pred "date" [Var "D",Var "59305"])) (Pred "objectDollar" [Var "O",Var "120"])) (Pred "paid" [Var "X",Var "Y",Var "O",Var "D"])))))) (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Bob"]) (Pred "name" [Var "Y",Var "Alice"])) (Pred "date" [Var "D",Var "59309"])) (Pred "objectOtherObject" [Var "O",Var "orange"])) (Pred "mustDeliver" [Var "X",Var "Y",Var "O",Var "D"])))))

bikeDeliveryOriginalFOL = And (Brackets (Implies (Or (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Alice"]) (Pred "name" [Var "Y",Var "Bob"])) (Pred "date" [Var "D",Var "59305"])) (Pred "objectPound" [Var "O",Var "100"])) (Pred "paid" [Var "X",Var "Y",Var "O",Var "D"])))) (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Alice"]) (Pred "name" [Var "Y",Var "Bob"])) (Pred "date" [Var "D",Var "59305"])) (Pred "objectDollar" [Var "O",Var "120"])) (Pred "paid" [Var "X",Var "Y",Var "O",Var "D"]))))) (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Bob"]) (Pred "name" [Var "Y",Var "Alice"])) (Pred "date" [Var "D",Var "59309"])) (Pred "objectOtherObject" [Var "O",Var "bicycle"])) (Pred "mustDeliver" [Var "X",Var "Y",Var "O",Var "D"])))))) (Brackets (And (ForAll [Var "X",Var "Y",Var "O"] (Brackets (Exists [Var "D"] (Brackets (And (And (And (Pred "name" [Var "X",Var "Bob"]) (Pred "name" [Var "Y",Var "Alice"])) (Pred "objectReport" [Var "O",Var "receipt"])) (Pred "mayDeliver" [Var "X",Var "Y",Var "O",Var "D"])))))) (Not (ForAll [Var "X",Var "Y",Var "O"] (Brackets (Exists [Var "D"] (Brackets (And (And (And (Pred "name" [Var "X",Var "Bob"]) (Pred "name" [Var "Y",Var "Alice"])) (Pred "objectAmount" [Var "O",Var "delivery fee"])) (Pred "mayCharge" [Var "X",Var "Y",Var "O",Var "D"])))))))))
bikeDeliveryModifiedFOL = Implies (Or (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Alice"]) (Pred "name" [Var "Y",Var "Bob"])) (Pred "date" [Var "D",Var "59305"])) (Pred "objectPound" [Var "O",Var "100"])) (Pred "paid" [Var "X",Var "Y",Var "O",Var "D"])))) (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Alice"]) (Pred "name" [Var "Y",Var "Bob"])) (Pred "date" [Var "D",Var "59305"])) (Pred "objectDollar" [Var "O",Var "120"])) (Pred "paid" [Var "X",Var "Y",Var "O",Var "D"]))))) (And (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Bob"]) (Pred "name" [Var "Y",Var "Alice"])) (Pred "date" [Var "D",Var "59309"])) (Pred "objectOtherObject" [Var "O",Var "bicycle"])) (Pred "mustDeliver" [Var "X",Var "Y",Var "O",Var "D"])))) (And (ForAll [Var "X",Var "Y",Var "O"] (Brackets (Exists [Var "D"] (Brackets (And (And (And (Pred "name" [Var "X",Var "Bob"]) (Pred "name" [Var "Y",Var "Alice"])) (Pred "objectReport" [Var "O",Var "receipt"])) (Pred "mayDeliver" [Var "X",Var "Y",Var "O",Var "D"])))))) (Not (ForAll [Var "X",Var "Y",Var "O"] (Brackets (Exists [Var "D"] (Brackets (And (And (And (Pred "name" [Var "X",Var "Bob"]) (Pred "name" [Var "Y",Var "Alice"])) (Pred "objectAmount" [Var "O",Var "delivery fee"])) (Pred "mayCharge" [Var "X",Var "Y",Var "O",Var "D"])))))))))
bikeDeliverySanctionFOL = And (Brackets (Implies (Or (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Alice"]) (Pred "name" [Var "Y",Var "Bob"])) (Pred "date" [Var "D",Var "59305"])) (Pred "objectPound" [Var "O",Var "100"])) (Pred "paid" [Var "X",Var "Y",Var "O",Var "D"])))) (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Alice"]) (Pred "name" [Var "Y",Var "Bob"])) (Pred "date" [Var "D",Var "59305"])) (Pred "objectDollar" [Var "O",Var "120"])) (Pred "paid" [Var "X",Var "Y",Var "O",Var "D"]))))) (And (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Bob"]) (Pred "name" [Var "Y",Var "Alice"])) (Pred "date" [Var "D",Var "59309"])) (Pred "objectOtherObject" [Var "O",Var "bicycle"])) (Pred "mustDeliver" [Var "X",Var "Y",Var "O",Var "D"])))) (Not (ForAll [Var "X",Var "Y",Var "O"] (Brackets (Exists [Var "D"] (Brackets (And (And (And (Pred "name" [Var "X",Var "Bob"]) (Pred "name" [Var "Y",Var "Alice"])) (Pred "objectAmount" [Var "O",Var "delivery fee"])) (Pred "mayCharge" [Var "X",Var "Y",Var "O",Var "D"])))))))))) (Brackets (And (Brackets (Implies (And (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Bob"]) (Pred "name" [Var "Y",Var "Alice"])) (Pred "date" [Var "D",Var "59309"])) (Pred "objectOtherObject" [Var "O",Var "bicycle"])) (Pred "delivered" [Var "X",Var "Y",Var "O",Var "D"])))) (Not (ForAll [Var "X",Var "Y",Var "O"] (Brackets (Exists [Var "D"] (Brackets (And (And (And (Pred "name" [Var "X",Var "Bob"]) (Pred "name" [Var "Y",Var "Alice"])) (Pred "objectAmount" [Var "O",Var "delivery fee"])) (Pred "charged" [Var "X",Var "Y",Var "O",Var "D"])))))))) (ForAll [Var "X",Var "Y",Var "O"] (Brackets (Exists [Var "D"] (Brackets (And (And (And (Pred "name" [Var "X",Var "Bob"]) (Pred "name" [Var "Y",Var "Alice"])) (Pred "objectReport" [Var "O",Var "receipt"])) (Pred "mayDeliver" [Var "X",Var "Y",Var "O",Var "D"])))))))) (Brackets (Implies (Not (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Bob"]) (Pred "name" [Var "Y",Var "Alice"])) (Pred "date" [Var "D",Var "59309"])) (Pred "objectOtherObject" [Var "O",Var "bicycle"])) (Pred "delivered" [Var "X",Var "Y",Var "O",Var "D"]))))) (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Alice"]) (Pred "name" [Var "Y",Var "Bob"])) (Pred "date" [Var "D",Var "59312"])) (Pred "objectPound" [Var "O",Var "100"])) (Pred "mayCharge" [Var "X",Var "Y",Var "O",Var "D"]))))))))
isdaOriginalFOL = ForAll [Var "D1"] (Brackets (And (Brackets (Implies (And (ForAll [Var "X",Var "Y",Var "O"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "PartyA"]) (Pred "name" [Var "Y",Var "PartyB"])) (Pred "isDate" [Var "D1"])) (Pred "objectAmount" [Var "O",Var "AmountA"])) (Pred "mustPay" [Var "X",Var "Y",Var "O",Var "D1"])))) (ForAll [Var "X",Var "Y",Var "O"] (Brackets (And (And (And (Pred "name" [Var "X",Var "PartyB"]) (Pred "name" [Var "Y",Var "PartyA"])) (Pred "objectAmount" [Var "O",Var "AmountB"])) (Pred "mustPay" [Var "X",Var "Y",Var "O",Var "D1"]))))) (And (Not (ForAll [Var "X",Var "Y",Var "O"] (Brackets (And (And (And (Pred "name" [Var "X",Var "PartyA"]) (Pred "name" [Var "Y",Var "PartyB"])) (Pred "objectAmount" [Var "O",Var "AmountA"])) (Pred "mustPay" [Var "X",Var "Y",Var "O",Var "D1"]))))) (Not (ForAll [Var "X",Var "Y",Var "O"] (Brackets (And (And (And (Pred "name" [Var "X",Var "PartyB"]) (Pred "name" [Var "Y",Var "PartyA"])) (Pred "objectAmount" [Var "O",Var "AmountB"])) (Pred "mustPay" [Var "X",Var "Y",Var "O",Var "D1"])))))))) (Brackets (And (Brackets (ForAll [Var "X",Var "Y",Var "O"] (Brackets (And (And (And (Pred "name" [Var "X",Var "ExcessParty"]) (Pred "name" [Var "Y",Var "AnotherParty"])) (Pred "objectAmount" [Var "O",Var "ExcessAmount"])) (Pred "mustPay" [Var "X",Var "Y",Var "O",Var "D1"]))))) (Brackets (And (Brackets (Implies (ForAll [Var "X",Var "Y"] (Brackets (And (And (Pred "name" [Var "X",Var "PartyA"]) (Pred "name" [Var "Y",Var "PartyB"])) (Pred "paidMore" [Var "X",Var "Y"])))) (And (Equal (Var "ExcessParty") (Var "PartyA")) (Equal (Var "ExcessAmount") (ContractAnalysis.AstToFol.Fun "minus" [Var "AmountA",Var "AmountB"]))))) (Brackets (Implies (ForAll [Var "X",Var "Y"] (Brackets (And (And (Pred "name" [Var "X",Var "PartyB"]) (Pred "name" [Var "Y",Var "PartyA"])) (Pred "paidMore" [Var "X",Var "Y"])))) (And (Equal (Var "ExcessParty") (Var "PartyB")) (Equal (Var "ExcessAmount") (ContractAnalysis.AstToFol.Fun "minus" [Var "AmountB",Var "AmountA"])))))))))))
isdaModifiedFOL = ForAll [Var "D1"] (Brackets (And (Brackets (Implies (And (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "PartyA"]) (Pred "name" [Var "Y",Var "PartyB"])) (Pred "date" [Var "D",Var "40587"])) (Pred "objectAmount" [Var "O",Var "AmountA"])) (Pred "mustPay" [Var "X",Var "Y",Var "O",Var "D"])))) (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "PartyB"]) (Pred "name" [Var "Y",Var "PartyA"])) (Pred "date" [Var "D",Var "40587"])) (Pred "objectAmount" [Var "O",Var "AmountB"])) (Pred "mustPay" [Var "X",Var "Y",Var "O",Var "D"]))))) (And (Not (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "PartyA"]) (Pred "name" [Var "Y",Var "PartyB"])) (Pred "date" [Var "D",Var "40587"])) (Pred "objectAmount" [Var "O",Var "AmountA"])) (Pred "mustPay" [Var "X",Var "Y",Var "O",Var "D"]))))) (And (Not (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "PartyB"]) (Pred "name" [Var "Y",Var "PartyA"])) (Pred "date" [Var "D",Var "40587"])) (Pred "objectAmount" [Var "O",Var "AmountB"])) (Pred "mustPay" [Var "X",Var "Y",Var "O",Var "D"]))))) (ForAll [Var "X",Var "Y",Var "O"] (Brackets (And (And (And (Pred "name" [Var "X",Var "Excessparty"]) (Pred "name" [Var "Y",Var "AnotherParty"])) (Pred "objectAmount" [Var "O",Var "ExcessAmount"])) (Pred "mustPay" [Var "X",Var "Y",Var "O",Var "D1"])))))))) (Brackets (And (Brackets (Implies (ForAll [Var "X",Var "Y"] (Brackets (And (And (Pred "name" [Var "X",Var "PartyA"]) (Pred "name" [Var "Y",Var "PartyB"])) (Pred "paidMore" [Var "X",Var "Y"])))) (And (Equal (Var "ExcessParty") (Var "PartyA")) (Equal (Var "ExcessAmount") (ContractAnalysis.AstToFol.Fun "minus" [Var "AmountA",Var "AmountB"]))))) (Brackets (Implies (ForAll [Var "X",Var "Y"] (Brackets (And (And (Pred "name" [Var "X",Var "PartyB"]) (Pred "name" [Var "Y",Var "PartyA"])) (Pred "paidMore" [Var "X",Var "Y"])))) (And (Equal (Var "ExcessParty") (Var "PartyA")) (Equal (Var "ExcessAmount") (ContractAnalysis.AstToFol.Fun "minus" [Var "AmountB",Var "AmountA"])))))))))
guarantorFOL = And (Brackets (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Landlord"]) (Pred "name" [Var "Y",Var "Tenant"])) (Pred "date" [Var "D",Var "60036"])) (Pred "objectOtherObject" [Var "O",Var "property"])) (Pred "mustDeliver" [Var "X",Var "Y",Var "O",Var "D"]))))) (Brackets (And (Brackets (Implies (And (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Landlord"]) (Pred "name" [Var "Y",Var "Tenant"])) (Pred "date" [Var "D",Var "60036"])) (Pred "objectOtherObject" [Var "O",Var "demandOfTenantPayment"])) (Pred "delivered" [Var "X",Var "Y",Var "O",Var "D"])))) (Not (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Tenant"]) (Pred "name" [Var "Y",Var "Landlord"])) (Pred "date" [Var "D",Var "60039"])) (Pred "objectAmount" [Var "O",Var "AmountA"])) (Pred "paidBefore" [Var "X",Var "Y",Var "O",Var "D"])))))) (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Landlord"]) (Pred "name" [Var "Y",Var "Tenant"])) (Pred "date" [Var "D",Var "60040"])) (Pred "objectOtherObject" [Var "O",Var "demandOfGuarantorPayment"])) (Pred "mayDeliver" [Var "X",Var "Y",Var "O",Var "D"])))))) (Brackets (And (Brackets (Implies (And (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Landlord"]) (Pred "name" [Var "Y",Var "Tenant"])) (Pred "date" [Var "D",Var "60036"])) (Pred "objectOtherObject" [Var "O",Var "demandOfTenantPayment"])) (Pred "delivered" [Var "X",Var "Y",Var "O",Var "D"])))) (And (Not (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Tenant"]) (Pred "name" [Var "Y",Var "Landlord"])) (Pred "date" [Var "D",Var "60044"])) (Pred "objectAmount" [Var "O",Var "AmountA"])) (Pred "paidBefore" [Var "X",Var "Y",Var "O",Var "D"]))))) (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Landlord"]) (Pred "name" [Var "Y",Var "Tenant"])) (Pred "date" [Var "D",Var "60040"])) (Pred "objectOtherObject" [Var "O",Var "demandOfGuarantorPayment"])) (Pred "delivered" [Var "X",Var "Y",Var "O",Var "D"])))))) (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Guarantor"]) (Pred "name" [Var "Y",Var "Landlord"])) (Pred "date" [Var "D",Var "60045"])) (Pred "objectAmount" [Var "O",Var "AmountA"])) (Pred "mustPay" [Var "X",Var "Y",Var "O",Var "D"])))))) (Brackets (And (Brackets (Implies (Not (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Tenant"]) (Pred "name" [Var "Y",Var "Landlord"])) (Pred "date" [Var "D",Var "60198"])) (Pred "objectAmount" [Var "O",Var "AmountB"])) (Pred "paidBefore" [Var "X",Var "Y",Var "O",Var "D"]))))) (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Guarantor"]) (Pred "name" [Var "Y",Var "Landlord"])) (Pred "date" [Var "D",Var "60199"])) (Pred "objectAmount" [Var "O",Var "AmountB"])) (Pred "mustPay" [Var "X",Var "Y",Var "O",Var "D"])))))) (Brackets (Implies (And (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "HousingBenefitScheme"]) (Pred "name" [Var "Y",Var "Tenant"])) (Pred "date" [Var "D",Var "60310"])) (Pred "objectAmount" [Var "O",Var "AmountC"])) (Pred "paid" [Var "X",Var "Y",Var "O",Var "D"])))) (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "LocalAuthority"]) (Pred "name" [Var "Y",Var "Tenant"])) (Pred "date" [Var "D",Var "60316"])) (Pred "objectOtherObject" [Var "O",Var "overpaymentClaim"])) (Pred "delivered" [Var "X",Var "Y",Var "O",Var "D"]))))) (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Guarantor"]) (Pred "name" [Var "Y",Var "Landlord"])) (Pred "date" [Var "D",Var "60319"])) (Pred "objectAmount" [Var "O",Var "AmountC"])) (Pred "mustPayBefore" [Var "X",Var "Y",Var "O",Var "D"]))))))))))))
employmentFOL = ForAll [Var "D4",Var "D1",Var "D3",Var "D2"] (Brackets (And (Brackets (Implies (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Employee"]) (Pred "name" [Var "Y",Var "Employer"])) (Pred "date" [Var "D",Var "60394"])) (Pred "objectOtherObject" [Var "O",Var "responsibilities"])) (Pred "deliveredBefore" [Var "X",Var "Y",Var "O",Var "D"])))) (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Employer"]) (Pred "name" [Var "Y",Var "Employee"])) (Pred "date" [Var "D",Var "60394"])) (Pred "objectPound" [Var "O",Var "2500"])) (Pred "mustPay" [Var "X",Var "Y",Var "O",Var "D"])))))) (Brackets (And (Brackets (Implies (ForAll [Var "X",Var "Y",Var "O"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Employee"]) (Pred "name" [Var "Y",Var "Employer"])) (Pred "isDate" [Var "D1"])) (Pred "objectOtherObject" [Var "O",Var "absenceWithoutPriorNotice"])) (Pred "delivered" [Var "X",Var "Y",Var "O",Var "D1"])))) (And (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Employee"]) (Pred "name" [Var "Y",Var "Employer"])) (Pred "dateAfter" [Var "D",Var "D1",Var "3"])) (Pred "objectReport" [Var "O",Var "absenceReason"])) (Pred "mustDeliverBefore" [Var "X",Var "Y",Var "O",Var "D"])))) (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Employer"]) (Pred "name" [Var "Y",Var "Employee"])) (Pred "dateAfter" [Var "D",Var "D1",Var "28"])) (Pred "objectPound" [Var "O",Var "100"])) (Pred "mayCharge" [Var "X",Var "Y",Var "O",Var "D"]))))))) (Brackets (And (Brackets (Implies (ForAll [Var "X",Var "Y",Var "O"] (Brackets (And (And (And (Pred "name" [Var "X",Var "Employee"]) (Pred "name" [Var "Y",Var "Employer"])) (Pred "objectReport" [Var "O",Var "requestForReimbursement"])) (Pred "delivered" [Var "X",Var "Y",Var "O",Var "D2"])))) (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (And (Pred "name" [Var "X",Var "Employee"]) (Pred "name" [Var "Y",Var "Employer"])) (Pred "isDate" [Var "D2"])) (Pred "dateAfter" [Var "D",Var "D2",Var "14"])) (Pred "objectReport" [Var "O",Var "approvalRequest"])) (Pred "mustDeliverBefore" [Var "X",Var "Y",Var "O",Var "D"])))))) (Brackets (And (Brackets (Implies (ForAll [Var "X",Var "Y",Var "O"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Employee"]) (Pred "name" [Var "Y",Var "Employer"])) (Pred "isDate" [Var "D3"])) (Pred "objectOtherObject" [Var "O",Var "extraHoursOfWork"])) (Pred "delivered" [Var "X",Var "Y",Var "O",Var "D3"])))) (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Employer"]) (Pred "name" [Var "Y",Var "Employee"])) (Pred "dateAfter" [Var "D",Var "D3",Var "28"])) (Pred "objectAmount" [Var "O",Var "overtimePayment"])) (Pred "mustPayBefore" [Var "X",Var "Y",Var "O",Var "D"])))))) (Brackets (And (Brackets (ForAll [Var "X",Var "Y",Var "O"] (Brackets (Exists [Var "D"] (Brackets (And (And (And (Pred "name" [Var "X",Var "Employee"]) (Pred "name" [Var "Y",Var "Client"])) (Pred "objectOtherObject" [Var "O",Var "presentationsOnBehalfOfEmployer"])) (Pred "mayDeliver" [Var "X",Var "Y",Var "O",Var "D"]))))))) (Brackets (And (Brackets (Not (ForAll [Var "X",Var "Y",Var "O"] (Brackets (Exists [Var "D"] (Brackets (And (And (And (Pred "name" [Var "X",Var "Employee"]) (Pred "name" [Var "Y",Var "Employer"])) (Pred "objectAmount" [Var "O",Var "unauthorisedExpenses"])) (Pred "mayCharge" [Var "X",Var "Y",Var "O",Var "D"])))))))) (Brackets (And (Brackets (Implies (ForAll [Var "X",Var "Y",Var "O"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Employee"]) (Pred "name" [Var "Y",Var "Employer"])) (Pred "isDate" [Var "D4"])) (Pred "objectOtherObject" [Var "O",Var "targetOutperformance"])) (Pred "delivered" [Var "X",Var "Y",Var "O",Var "D4"])))) (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (And (Pred "name" [Var "X",Var "Employer"]) (Pred "name" [Var "Y",Var "Employee"])) (Pred "isDate" [Var "D4"])) (Pred "dateAfter" [Var "D",Var "D4",Var "365"])) (Pred "objectAmount" [Var "O",Var "bonus"])) (Pred "mustPay" [Var "X",Var "Y",Var "O",Var "D"])))))) (Brackets (ForAll [Var "X",Var "Y",Var "O"] (Brackets (Exists [Var "D"] (Brackets (And (And (And (Pred "name" [Var "X",Var "Employee"]) (Pred "name" [Var "Y",Var "Client"])) (Pred "objectAmount" [Var "O",Var "serviceChargeOnBehalfOfEmployer"])) (Pred "mayCharge" [Var "X",Var "Y",Var "O",Var "D"])))))))))))))))))))))
tenancyFOL = ForAll [Var "D1"] (Brackets (And (Brackets (Implies (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Tenant"]) (Pred "name" [Var "Y",Var "Landlord"])) (Pred "date" [Var "D",Var "60188"])) (Pred "objectPound" [Var "O",Var "15000"])) (Pred "paidBefore" [Var "X",Var "Y",Var "O",Var "D"])))) (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Landlord"]) (Pred "name" [Var "Y",Var "Tenant"])) (Pred "date" [Var "D",Var "60210"])) (Pred "objectOtherObject" [Var "O",Var "property"])) (Pred "mustDeliver" [Var "X",Var "Y",Var "O",Var "D"])))))) (Brackets (And (Brackets (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Landlord"]) (Pred "name" [Var "Y",Var "Tenant"])) (Pred "date" [Var "D",Var "60179"])) (Pred "objectAmount" [Var "O",Var "securityDeposit"])) (Pred "mayCharge" [Var "X",Var "Y",Var "O",Var "D"]))))) (Brackets (And (Brackets (ForAll [Var "X",Var "Y",Var "O"] (Brackets (Exists [Var "D"] (Brackets (And (And (And (Pred "name" [Var "X",Var "Tenant"]) (Pred "name" [Var "Y",Var "Landlord"])) (Pred "objectOtherObject" [Var "O",Var "requestForRepair"])) (Pred "mayDeliver" [Var "X",Var "Y",Var "O",Var "D"]))))))) (Brackets (And (Brackets (Not (ForAll [Var "X",Var "Y",Var "O"] (Brackets (Exists [Var "D"] (Brackets (And (And (And (Pred "name" [Var "X",Var "Tenant"]) (Pred "name" [Var "Y",Var "Others"])) (Pred "objectOtherObject" [Var "O",Var "subletOfProperty"])) (Pred "mayDeliver" [Var "X",Var "Y",Var "O",Var "D"])))))))) (Brackets (And (Brackets (Implies (And (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Tenant"]) (Pred "name" [Var "Y",Var "Landlord"])) (Pred "date" [Var "D",Var "60575"])) (Pred "objectOtherObject" [Var "O",Var "notDamagedProperty"])) (Pred "delivered" [Var "X",Var "Y",Var "O",Var "D"])))) (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Landlord"]) (Pred "name" [Var "Y",Var "Tenant"])) (Pred "date" [Var "D",Var "60179"])) (Pred "objectAmount" [Var "O",Var "securityDeposit"])) (Pred "charged" [Var "X",Var "Y",Var "O",Var "D"]))))) (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Landlord"]) (Pred "name" [Var "Y",Var "Tenant"])) (Pred "date" [Var "D",Var "60584"])) (Pred "objectAmount" [Var "O",Var "securityDeposit"])) (Pred "mustRefundBefore" [Var "X",Var "Y",Var "O",Var "D"])))))) (Brackets (Implies (ForAll [Var "X",Var "Y",Var "O"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Landlord"]) (Pred "name" [Var "Y",Var "Tenant"])) (Pred "isDate" [Var "D1"])) (Pred "objectAmount" [Var "O",Var "incrementInRental"])) (Pred "charged" [Var "X",Var "Y",Var "O",Var "D1"])))) (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (And (Pred "name" [Var "X",Var "Landlord"]) (Pred "name" [Var "Y",Var "Tenant"])) (Pred "isDate" [Var "D1"])) (Pred "dateBefore" [Var "D",Var "D1",Var "28"])) (Pred "objectReport" [Var "O",Var "noticeOfRentalIncrement"])) (Pred "mustDeliver" [Var "X",Var "Y",Var "O",Var "D"]))))))))))))))))
serviceLevelAgreementFOL = And (Brackets (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "ServiceProvider"]) (Pred "name" [Var "Y",Var "Client"])) (Pred "date" [Var "D",Var "60319"])) (Pred "objectOtherObject" [Var "O",Var "agreedService"])) (Pred "mustDeliver" [Var "X",Var "Y",Var "O",Var "D"]))))) (Brackets (And (Brackets (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Client"]) (Pred "name" [Var "Y",Var "ServiceProvider"])) (Pred "date" [Var "D",Var "60379"])) (Pred "objectPound" [Var "O",Var "100"])) (Pred "mustPayBefore" [Var "X",Var "Y",Var "O",Var "D"]))))) (Brackets (And (Brackets (Implies (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "ServiceProvider"]) (Pred "name" [Var "Y",Var "Client"])) (Pred "date" [Var "D",Var "60319"])) (Pred "objectOtherObject" [Var "O",Var "agreedService"])) (Pred "deliveredAfter" [Var "X",Var "Y",Var "O",Var "D"])))) (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "ServiceProvider"]) (Pred "name" [Var "Y",Var "Client"])) (Pred "date" [Var "D",Var "60326"])) (Pred "objectPound" [Var "O",Var "10"])) (Pred "mustRefundBefore" [Var "X",Var "Y",Var "O",Var "D"])))))) (Brackets (And (Brackets (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Client"]) (Pred "name" [Var "Y",Var "ServiceProvider"])) (Pred "date" [Var "D",Var "60322"])) (Pred "objectReport" [Var "O",Var "requestForRefund"])) (Pred "mayDeliverBefore" [Var "X",Var "Y",Var "O",Var "D"]))))) (Brackets (Implies (Not (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "Client"]) (Pred "name" [Var "Y",Var "ServiceProvider"])) (Pred "date" [Var "D",Var "60410"])) (Pred "objectPound" [Var "O",Var "100"])) (Pred "paidBefore" [Var "X",Var "Y",Var "O",Var "D"]))))) (ForAll [Var "X",Var "Y",Var "O",Var "D"] (Brackets (And (And (And (And (Pred "name" [Var "X",Var "ServiceProvider"]) (Pred "name" [Var "Y",Var "Client"])) (Pred "date" [Var "D",Var "60410"])) (Pred "objectReport" [Var "O",Var "terminationOfAgreement"])) (Pred "mayDeliverAfter" [Var "X",Var "Y",Var "O",Var "D"]))))))))))))

tqDict1 = Map.fromList [("BobDeliverAliceObjectOtherObjectwatermelon",("Before",60319))]
tqDict2 = Map.fromList [("CannotCindyRefundAlexObjectEuro10",("Before",60197)),("CindyDeliverAlexObjectReportreceipt",("After",60195))]
tqDict3 = Map.fromList []

folSampleTests :: [(String, FOLFormula)]
folSampleTests = 
    [(emptyContract, emptyContractFOL)
    , (simpleDefinition1, simpleDefinitionFOL)
    , (andDefinition4, andDefinitionFOL)
    , (conditionalDefinition8, conditionalDefinitionFOL)
    , (simpleStatement3, simpleStatementFOL)
    , (andStatement3, andStatementFOL)
    , (orStatement1, orStatementFOL)
    , (conditionalStatement8, conditionalStatementFOL)
    , (consistencyContract1, consistencyContract1FOL)
    , (consistencyContract2, consistencyContract2FOL)
    , (consistencyContract3, consistencyContract3FOL)
    , (consistencyContract4, consistencyContract4FOL)
    , (consistencyContract5, consistencyContract5FOL)
    , (consistencyContract6, consistencyContract6FOL)
    , (consistencyContract7, consistencyContract7FOL)
    , (bikeDeliveryOriginal, bikeDeliveryOriginalFOL)
    , (bikeDeliveryModified, bikeDeliveryModifiedFOL)
    , (bikeDeliverySanction, bikeDeliverySanctionFOL)
    , (isdaOriginal, isdaOriginalFOL)
    , (isdaModified, isdaModifiedFOL)
    , (guarantor, guarantorFOL)
    , (employment, employmentFOL)
    , (tenancy, tenancyFOL)
    , (serviceLevelAgreement, serviceLevelAgreementFOL)]

tqDictSampleTests :: [(String, TempQuanDictionary)]
tqDictSampleTests = 
    [(tqTestContract1, tqDict1)
    , (tqTestContract2, tqDict2)
    , (tqTestContract3, tqDict3)]

folConversionTest :: IO ()
folConversionTest = do
    putStrLn "Running QuickCheck tests for AST to FOL converter ..."
    quickCheck $ forAll (elements folSampleTests) (\(input, expected) -> prop_convertToFOL input expected)
    putStrLn "Running QuickCheck tests for temporal quantifier dictionary in FOL converter ..."
    quickCheck $ forAll (elements tqDictSampleTests) (\(input, expected) ->  prop_correctTempQuanDictionary input expected)
    putStrLn "Done."