module Test.InconsistencyAnalysisTest where

import Test.QuickCheck
import Data.List (isInfixOf)
import qualified Main as CoLaParser
import AbsCoLa 
import ExampleContracts

prop_consistencyChecking :: String -> String -> String -> Property
prop_consistencyChecking inputContract inputPerformance expectedResult =
    CoLaParser.getConsistencyAnalysisResult (CoLaParser.checkInconsistencyInContract (inputContract) (inputPerformance)) === expectedResult

consistencyPerformance1a = "[1] Carol paid EUROS 30 to David on 19 September 2023 C-AND [2] David delivered OTHEROBJECT orange to Carol on 25 September 2023"
consistencyPerformance1b = "[1] Carol paid EUROS 30 to David before 20 September 2023 C-AND [2] David delivered OTHEROBJECT orange to Carol on 29 September 2023"
consistencyPerformance1c = "[1] it is not the case that Carol paid EUROS 30 to David before 20 September 2023 C-AND [2] it is not the case that David delivered OTHEROBJECT orange to Carol before 27 September 2023"
consistencyPerformance2a = "[1] Emily paid DOLLARS 45 to Frank on the 30 November 2023 C-AND [2] Frank delivered OTHEROBJECT notebook to Emily on 2 December 2023"
consistencyPerformance2b = "[1] Emily paid DOLLARS 45 to Frank on the 30 November 2023 C-AND [2] it is not the case that Frank delivered OTHEROBJECT notebook to Emily on ANYDATE"
consistencyPerformance2c = "[1] it is not the case that Frank delivered OTHEROBJECT notebook to Emily on ANYDATE"
consistencyPerformance3a = "[1] Grace refunded POUNDS 20 to Helena on the 6 July 2021 C-AND [2] Helena delivered REPORT \"financial report\" to Grace on THEDATE unknownOne"
consistencyPerformance3b = "[1] Grace refunded POUNDS 20 to Helena on the 6 July 2021 C-AND [2] it is not the case that Helena delivered REPORT \"financial report\" to Grace on THEDATE unknownOne"
consistencyPerformance4a = "[1] Ivana charged DOLLARS 13 to Jess before THEDATE unknownOne C-AND [2] Ivana delivered OTHEROBJECT dress to Jess after THEDATE unknownOne"
consistencyPerformance4b = "[1] Ivana charged DOLLARS 13 to Jess before SOMEDATE unknownOne C-AND [2] it is not the case that Ivana delivered OTHEROBJECT dress to Jess after THEDATE unknownOne"
consistencyPerformance5a = "[1] Kathy paid EUROS 3 to Lily on SOMEDATE unknownOne C-AND [2] Lily delivered OTHEROBJECT bicycle to Kathy 7 days after THEDATE unknownOne"
consistencyPerformance5b = "[1] Kathy paid EUROS 3 to Lily on SOMEDATE unknownOne C-AND [2] Lily delivered OTHEROBJECT bicycle to Kathy 8 days after THEDATE unknownOne"
consistencyPerformance6a = "[1] Mandy paid POUNDS 17 to Nancy on 7 April 2021 C-AND [2] Nancy delivered OTHEROBJECT bag to Mandy on 9 April 2021"
consistencyPerformance7a = "[1] it is not the case that Alice paid DOLLARS 120 to Bob on the 1 April 2021 C-AND [2] it is not the case that Alice paid POUNDS 100 to Bob on the 1 April 2021 C-AND [3] it is not the case that Bob delivered OTHEROBJECT orange to Alice on the 5 April 2021"
consistencyPerformance7b = "[1] Alice paid DOLLARS 120 to Bob on 1 April 2021 C-AND [2] Bob delivered OTHEROBJECT bicycle to Alice on 5 April 2021"
consistencyPerformance7c = "[1] it is not the case that Alice paid DOLLARS 120 to Bob on the 1 April 2021 C-AND [2] it is not the case that Alice paid POUNDS 100 to Bob on the 1 April 2021 C-AND [3] Bob delivered OTHEROBJECT orange to Alice on the 5 April 2021"

bikeOriginalPerformance1a = "[1] it is the case that Alice paid POUNDS 100 to Bob on the 1 April 2021 C-AND [2] it is the case that Bob delivered OTHEROBJECT bicycle to Alice on the 5 April 2021"
bikeOriginalPerformance1b = "[1] it is the case that Alice paid POUNDS 100 to Bob on the 1 April 2021 C-AND [2] it is not the case that Bob delivered OTHEROBJECT bicycle to Alice on the 5 April 2021"
bikeOriginalPerformance1c = "[1] it is the case that Bob charged AMOUNT \"delivery fee\" to Alice on the 1 April 2021"
bikeOriginalPerformance1d = "[1] Alice paid POUNDS 100 to Bob on the 4 April 2021 C-AND [2] it is not the case that Bob delivered OTHEROBJECT bicycle to Alice on the 5 April 2021"
bikeOriginalPerformance1e = "[1] Bob delivered REPORT receipt to Alice on 5 April 2021"

guarantorPerformance1a = "[1] it is not the case that Landlord delivered OTHEROBJECT property to Tenant on the 2 April 2023"
guarantorPerformance1b = "[1] Landlord delivered OTHEROBJECT demandOfTenantPayment to Tenant on the 2 April 2023 C-AND [2] it is not the case that Tenant paid AMOUNT AmountA to Landlord before 10 April 2023 C-AND [3] Landlord delivered OTHEROBJECT demandOfGuarantorPayment to Tenant on the 6 April 2023 C-AND [4] it is not the case that Guarantor paid AMOUNT AmountA to Landlord on the 11 April 2023"
guarantorPerformance1c = "[1] it is not the case that Tenant paid AMOUNT AmountB to Landlord before 11 September 2023 C-AND [2] Guarantor paid AMOUNT AmountB to Landlord on the 12 September 2023"

noInconsistencyExpected = "Consistent"
inconsistencyExpected = "Inconsistent"

consistencyAnalysisSampleTests :: [(String, String, String)]
consistencyAnalysisSampleTests = 
    [(consistencyContract1, consistencyPerformance1a, noInconsistencyExpected)
    , (consistencyContract1, consistencyPerformance1b, inconsistencyExpected)
    , (consistencyContract1, consistencyPerformance1c, noInconsistencyExpected)
    , (consistencyContract2, consistencyPerformance2a, noInconsistencyExpected)
    , (consistencyContract2, consistencyPerformance2b, inconsistencyExpected)
    , (consistencyContract2, consistencyPerformance2c, noInconsistencyExpected)
    , (consistencyContract3, consistencyPerformance3a, noInconsistencyExpected)
    , (consistencyContract3, consistencyPerformance3b, inconsistencyExpected)
    , (consistencyContract4, consistencyPerformance4a, noInconsistencyExpected)
    , (consistencyContract4, consistencyPerformance4b, inconsistencyExpected)
    , (consistencyContract5, consistencyPerformance5a, noInconsistencyExpected)
    , (consistencyContract5, consistencyPerformance5b, inconsistencyExpected)
    , (consistencyContract6, consistencyPerformance6a, noInconsistencyExpected)
    , (consistencyContract7, consistencyPerformance7a, inconsistencyExpected)
    , (consistencyContract7, consistencyPerformance7b, noInconsistencyExpected)
    , (consistencyContract7, consistencyPerformance7c, noInconsistencyExpected)
    , (bikeDeliveryOriginal, bikeOriginalPerformance1a, noInconsistencyExpected)
    , (bikeDeliveryOriginal, bikeOriginalPerformance1b, inconsistencyExpected)
    , (bikeDeliveryOriginal, bikeOriginalPerformance1c, inconsistencyExpected)
    , (bikeDeliveryOriginal, bikeOriginalPerformance1d, noInconsistencyExpected)
    , (bikeDeliveryOriginal, bikeOriginalPerformance1e, noInconsistencyExpected)
    , (guarantor, guarantorPerformance1a, inconsistencyExpected)
    , (guarantor, guarantorPerformance1b, inconsistencyExpected)
    , (guarantor, guarantorPerformance1c, noInconsistencyExpected)
    ]

consistencyAnalysisTest :: IO ()
consistencyAnalysisTest = do
    putStrLn "Running QuickCheck tests for consistency checking pipeline ..."
    quickCheck $ forAll (elements consistencyAnalysisSampleTests) (\(input1, input2, expected) -> prop_consistencyChecking input1 input2 expected)
    putStrLn "Done."