module Test.ComplexityAnalysisTest where

import Test.QuickCheck
import qualified Main as CoLaParser
import Parser.AbsCoLa 
import Helper.ExampleContracts
import ContractAnalysis.ComplexityAnalysis

prop_complexityMetrics :: String -> [Integer] -> Property
prop_complexityMetrics input expectedComplexityMetrics =
    getComplexityMetrics (CoLaParser.parseContract input) === expectedComplexityMetrics

prop_complexityReport :: String -> String -> Property
prop_complexityReport input expectedComplexityReport =
    printComplexityReport (getComplexityMetrics (CoLaParser.parseContract input)) === expectedComplexityReport

emptyContractComplexityMetrics = [1,0,0,0,0,0,0]
simpleDefinitionComplexityMetrics = [1,1,0,0,0,0,0]
andDefinitionComplexityMetrics = [1,5,0,0,0,0,0]
conditionalDefinitionComplexityMetrics = [4,7,2,0,1,0,0]
simpleStatementComplexityMetrics = [2,1,2,0,0,0,0]
andStatementComplexityMetrics = [4,4,4,0,0,0,0]
orStatementComplexityMetrics = [5,4,2,0,0,0,0]
conditionalStatementComplexityMetrics = [5,9,2,0,1,0,3]

bikeDeliveryOriginalComplexityMetrics = [6,14,2,0,0,0,0]
bikeDeliveryModifiedComplexityMetrics = [4,9,2,0,0,0,0]
bikeDeliverySanctionComplexityMetrics = [8,26,2,0,0,0,0]
isdaOriginalComplexityMetrics = [11,111,4,4,0,0,0]
isdaModifiedComplexityMetrics = [10,86,4,4,0,0,0]
guarantorComplexityMetrics = [17,51,5,0,4,0,0]
employmentComplexityMetrics = [21,108,3,0,9,10,1107]
tenancyComplexityMetrics = [14,54,3,0,3,2,0]
serviceLevelAgreementComplexityMetrics = [10,38,2,0,6,0,25]

emptyContractComplexityReport = "Number of States: 1\nNumber of Transitions: 0\nNumber of Parties Involved: 0\nNumber of Numerical Operators: 0\nNumber of Temporal Quantifiers: 0\nNumber of Temporal Offsets: 0\nScore for Payment Variability: 0\n\nTotal Complexity Score: 1"
bikeDeliveryOriginalComplexityReport = "Number of States: 6\nNumber of Transitions: 14\nNumber of Parties Involved: 2\nNumber of Numerical Operators: 0\nNumber of Temporal Quantifiers: 0\nNumber of Temporal Offsets: 0\nScore for Payment Variability: 0\n\nTotal Complexity Score: 22"
bikeDeliveryModifiedComplexityReport = "Number of States: 4\nNumber of Transitions: 9\nNumber of Parties Involved: 2\nNumber of Numerical Operators: 0\nNumber of Temporal Quantifiers: 0\nNumber of Temporal Offsets: 0\nScore for Payment Variability: 0\n\nTotal Complexity Score: 15"
bikeDeliverySanctionComplexityReport = "Number of States: 8\nNumber of Transitions: 26\nNumber of Parties Involved: 2\nNumber of Numerical Operators: 0\nNumber of Temporal Quantifiers: 0\nNumber of Temporal Offsets: 0\nScore for Payment Variability: 0\n\nTotal Complexity Score: 36"
isdaOriginalComplexityReport = "Number of States: 11\nNumber of Transitions: 111\nNumber of Parties Involved: 4\nNumber of Numerical Operators: 4\nNumber of Temporal Quantifiers: 0\nNumber of Temporal Offsets: 0\nScore for Payment Variability: 0\n\nTotal Complexity Score: 130"
isdaModifiedComplexityReport = "Number of States: 10\nNumber of Transitions: 86\nNumber of Parties Involved: 4\nNumber of Numerical Operators: 4\nNumber of Temporal Quantifiers: 0\nNumber of Temporal Offsets: 0\nScore for Payment Variability: 0\n\nTotal Complexity Score: 104"
guarantorComplexityReport = "Number of States: 17\nNumber of Transitions: 51\nNumber of Parties Involved: 5\nNumber of Numerical Operators: 0\nNumber of Temporal Quantifiers: 4\nNumber of Temporal Offsets: 0\nScore for Payment Variability: 0\n\nTotal Complexity Score: 77"
employmentComplexityReport = "Number of States: 21\nNumber of Transitions: 108\nNumber of Parties Involved: 3\nNumber of Numerical Operators: 0\nNumber of Temporal Quantifiers: 9\nNumber of Temporal Offsets: 10\nScore for Payment Variability: 1107\n\nTotal Complexity Score: 1258"
tenancyComplexityReport = "Number of States: 14\nNumber of Transitions: 54\nNumber of Parties Involved: 3\nNumber of Numerical Operators: 0\nNumber of Temporal Quantifiers: 3\nNumber of Temporal Offsets: 2\nScore for Payment Variability: 0\n\nTotal Complexity Score: 76"
serviceLevelAgreementComplexityReport = "Number of States: 10\nNumber of Transitions: 38\nNumber of Parties Involved: 2\nNumber of Numerical Operators: 0\nNumber of Temporal Quantifiers: 6\nNumber of Temporal Offsets: 0\nScore for Payment Variability: 25\n\nTotal Complexity Score: 81"

complexityMetricsSampleTests :: [(String, [Integer])]
complexityMetricsSampleTests = 
    [(emptyContract, emptyContractComplexityMetrics)
    , (simpleDefinition1, simpleDefinitionComplexityMetrics)
    , (andDefinition4, andDefinitionComplexityMetrics)
    , (conditionalDefinition8, conditionalDefinitionComplexityMetrics)
    , (simpleStatement3, simpleStatementComplexityMetrics)
    , (andStatement3, andStatementComplexityMetrics)
    , (orStatement1, orStatementComplexityMetrics)
    , (conditionalStatement8, conditionalStatementComplexityMetrics)
    , (bikeDeliveryOriginal, bikeDeliveryOriginalComplexityMetrics)
    , (bikeDeliveryModified, bikeDeliveryModifiedComplexityMetrics)
    , (bikeDeliverySanction, bikeDeliverySanctionComplexityMetrics)
    , (isdaOriginal, isdaOriginalComplexityMetrics)
    , (isdaModified, isdaModifiedComplexityMetrics)
    , (guarantor, guarantorComplexityMetrics)
    , (employment, employmentComplexityMetrics)
    , (tenancy, tenancyComplexityMetrics)
    , (serviceLevelAgreement, serviceLevelAgreementComplexityMetrics)]

complexityReportSampleTests :: [(String, String)]
complexityReportSampleTests = 
    [(emptyContract, emptyContractComplexityReport)
    , (bikeDeliveryOriginal, bikeDeliveryOriginalComplexityReport)
    , (bikeDeliveryModified, bikeDeliveryModifiedComplexityReport)
    , (bikeDeliverySanction, bikeDeliverySanctionComplexityReport)
    , (isdaOriginal, isdaOriginalComplexityReport)
    , (isdaModified, isdaModifiedComplexityReport)
    , (guarantor, guarantorComplexityReport)
    , (employment, employmentComplexityReport)
    , (tenancy, tenancyComplexityReport)
    , (serviceLevelAgreement, serviceLevelAgreementComplexityReport)]

complexityAnalysisTest :: IO ()
complexityAnalysisTest = do
    putStrLn "Running QuickCheck tests for abstracting complexity metrics ..."
    quickCheck $ forAll (elements complexityMetricsSampleTests) (\(input, expected) -> prop_complexityMetrics input expected)
    putStrLn "Running QuickCheck tests for complexity analysis pipeline ..."
    quickCheck $ forAll (elements complexityReportSampleTests) (\(input, expected) -> prop_complexityReport input expected)
    putStrLn "Done."