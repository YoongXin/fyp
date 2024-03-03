module Main where

import Prelude
  ( ($), (.), (==), (<$>)
  , Either(..)
  , Int, (>)
  , String, (++), concat, unlines
  , Show, show
  , IO, (>>), (>>=), mapM_, putStrLn
  , FilePath
  , getContents, readFile, error, otherwise
  )
import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad      ( when )
import Data.Graph.Inductive.Graph 
import System.IO.Unsafe (unsafePerformIO)

import AbsCoLa   
import LexCoLa   ( Token, mkPosToken )
import ParCoLa   ( pContract, myLexer )
import PrintCoLa ( Print, printTree )
import SkelCoLa  ()
import GenerateAST
import AstToFOL
import FOLToTPTP
import AstToNFA
import AstToPetriNet
import ExampleContracts
import CheckCompleteness
import AstToDFA
import ComplexityAnalysis

import Data.List (isInfixOf)
import qualified Data.Map as Map
import Control.Monad.State
import System.IO 
import System.Process
import Data.Time

parseContract :: String -> Contract
parseContract contractString =
    case runAST pContract contractString of
        Left errMsg -> error ("Parsing failed: " ++ errMsg)
        Right parsedContract -> parsedContract

main :: IO ()
main = do
    putStrLn "Enter the contract string:"
    contractString <- getLine
    let parsedContract = parseContract contractString
    putStrLn "Parsed contract:"
    print parsedContract

determineOutputTwo :: String -> String
determineOutputTwo output
    | "Termination reason: Satisfiable" `isInfixOf` output = "No inconsistency detected"
    | otherwise = "Inconsistency detected"

checkInconsistency :: FilePath -> FilePath -> IO ()
checkInconsistency contractFilePath performanceFilePath = do
    contractString <- readFile contractFilePath
    let (contract, dateDictionary, tempQuanDictionary) = runFOLConversion' (parseContract contractString)
    let tptpContract = folToTPTPString "contract" contract
    let contractComment = "%TPTP representation for the contract:"
    let contractFullForm = contractComment ++ "\n\n" ++ tptpContract

    performanceString <- readFile performanceFilePath
    let performance = evalState (contractToFOLWithCheck (parseContract performanceString)) (dateDictionary, tempQuanDictionary)
    let tptpPerformance = folToTPTPString "performance" performance
    let performanceComment = "%TPTP representation for the performance:"
    let performanceFullForm = performanceComment ++ "\n\n" ++ tptpPerformance

    let mustAxiom = "fof(mustCondition, axiom, (" ++
                    "! [X,Y,O,D] :" ++
                    "(mustDeliver(X,Y,O,D) => delivered(X,Y,O,D)) & " ++
                    "(mustPay(X,Y,O,D) => paid(X,Y,O,D)) & " ++
                    "(mustCharge(X,Y,O,D) => charged(X,Y,O,D)) & " ++
                    "(mustRefund(X,Y,O,D) => refunded(X,Y,O,D))" ++
                    "))."

    let forbiddenAxiom = "fof(forbiddenContradiction, axiom, (" ++
                         "! [X, Y, D, O] : (" ++
                         "(~ mayDeliver(X, Y, O, D) & delivered(X, Y, O, D)) |" ++
                         "(~ mayPay(X, Y, O, D) & paid(X, Y, O, D)) |" ++
                         "(~ mayCharge(X, Y, O, D) & charged(X, Y, O, D)) |" ++
                         "(~ mayRefund(X, Y, O, D) & refunded(X, Y, O, D))" ++
                         " => $false" ++
                         ")" ++
                         "))."

    let mustTemporalQuantifierAxiom = "fof(mustWithTemporalQuantifierContradiction, axiom, (" ++
                                      "! [X, Y, O, D] : (" ++
                                      "(mustDeliverAfter(X, Y, O, D) & ~ deliveredAfter(X, Y, O, D)) |" ++
                                      "(mustPayAfter(X, Y, O, D) & ~ paidAfter(X, Y, O, D)) |" ++
                                      "(mustChargeAfter(X, Y, O, D) & ~ chargedAfter(X, Y, O, D)) |" ++
                                      "(mustRefundAfter(X, Y, O, D) & ~ refundedAfter(X, Y, O, D)) |" ++
                                      "(mustDeliverBefore(X, Y, O, D) & ~ deliveredBefore(X, Y, O, D)) |" ++
                                      "(mustPayBefore(X, Y, O, D) & ~ paidBefore(X, Y, O, D)) |" ++
                                      "(mustChargeBefore(X, Y, O, D) & ~ chargedBefore(X, Y, O, D)) |" ++
                                      "(mustRefundBefore(X, Y, O, D) & ~ refundedBefore(X, Y, O, D))" ++
                                      " => $false)))."

    let axioms = mustAxiom ++ "\n\n" ++ forbiddenAxiom ++ "\n\n" ++ mustTemporalQuantifierAxiom

    let combinedOutput = contractFullForm ++ "\n\n" ++ performanceFullForm ++ "\n\n" ++ axioms

    timeStamp <- formatTime defaultTimeLocale "-%Y-%m-%d—%H:%M:%S" <$> getCurrentTime

    let tptpFilePath = "output/checkInconsistency" ++ timeStamp ++ ".p"

    writeFile tptpFilePath combinedOutput

    putStrLn $ "Combined TPTP representation written to " ++ tptpFilePath ++ "\n"
      
    -- Run the Vampire command and capture its output
    output <- readProcess "./vampire_rel" ["--input_file", tptpFilePath] ""

    putStrLn "Vampire command output:"
    putStrLn output

    let outputTwo = determineOutputTwo output
    putStrLn outputTwo

checkInconsistencyContract :: String -> String -> IO String
checkInconsistencyContract contractString performanceString = do
    let (contract, dateDictionary, tempQuanDictionary) = runFOLConversion' (parseContract contractString)
    let tptpContract = folToTPTPString "contract" contract
    let contractComment = "%TPTP representation for the contract:"
    let contractFullForm = contractComment ++ "\n\n" ++ tptpContract

    let performance = evalState (contractToFOLWithCheck (parseContract performanceString)) (dateDictionary, tempQuanDictionary)
    let tptpPerformance = folToTPTPString "performance" performance
    let performanceComment = "%TPTP representation for the performance:"
    let performanceFullForm = performanceComment ++ "\n\n" ++ tptpPerformance

    let mustAxiom = "fof(mustCondition, axiom, (" ++
                    "! [X,Y,O,D] :" ++
                    "(mustDeliver(X,Y,O,D) => delivered(X,Y,O,D)) & " ++
                    "(mustPay(X,Y,O,D) => paid(X,Y,O,D)) & " ++
                    "(mustCharge(X,Y,O,D) => charged(X,Y,O,D)) & " ++
                    "(mustRefund(X,Y,O,D) => refunded(X,Y,O,D))" ++
                    "))."

    let forbiddenAxiom = "fof(forbiddenContradiction, axiom, (" ++
                         "! [X, Y, D, O] : (" ++
                         "(~ mayDeliver(X, Y, O, D) & delivered(X, Y, O, D)) |" ++
                         "(~ mayPay(X, Y, O, D) & paid(X, Y, O, D)) |" ++
                         "(~ mayCharge(X, Y, O, D) & charged(X, Y, O, D)) |" ++
                         "(~ mayRefund(X, Y, O, D) & refunded(X, Y, O, D))" ++
                         " => $false" ++
                         ")" ++
                         "))."

    let mustTemporalQuantifierAxiom = "fof(mustWithTemporalQuantifierContradiction, axiom, (" ++
                                      "! [X, Y, O, D] : (" ++
                                      "(mustDeliverAfter(X, Y, O, D) & ~ deliveredAfter(X, Y, O, D)) |" ++
                                      "(mustPayAfter(X, Y, O, D) & ~ paidAfter(X, Y, O, D)) |" ++
                                      "(mustChargeAfter(X, Y, O, D) & ~ chargedAfter(X, Y, O, D)) |" ++
                                      "(mustRefundAfter(X, Y, O, D) & ~ refundedAfter(X, Y, O, D)) |" ++
                                      "(mustDeliverBefore(X, Y, O, D) & ~ deliveredBefore(X, Y, O, D)) |" ++
                                      "(mustPayBefore(X, Y, O, D) & ~ paidBefore(X, Y, O, D)) |" ++
                                      "(mustChargeBefore(X, Y, O, D) & ~ chargedBefore(X, Y, O, D)) |" ++
                                      "(mustRefundBefore(X, Y, O, D) & ~ refundedBefore(X, Y, O, D))" ++
                                      " => $false)))."

    let axioms = mustAxiom ++ "\n\n" ++ forbiddenAxiom ++ "\n\n" ++ mustTemporalQuantifierAxiom

    let combinedOutput = contractFullForm ++ "\n\n" ++ performanceFullForm ++ "\n\n" ++ axioms

    writeFile "output/checkInconsistency.p" combinedOutput
      
    output <- readProcess "./vampire_rel" ["--input_file", "output/checkInconsistency.p"] ""

    let outputTwo = determineOutputTwo output
    return outputTwo

consistencyAnalysis :: IO String -> String
consistencyAnalysis ioResult = unsafePerformIO $ do
    result <- ioResult
    return (if result == "No inconsistency detected" then "Consistent" else "Inconsistent")

convertToNFA :: FilePath -> IO ()
convertToNFA contractFilePath = do
    contractString <- readFile contractFilePath
    let nfa = runNFAConversion (parseContract contractString)
    let graph = nfaToGraph nfa
    let dotFile = visualizeGraph nfa graph

    timeStamp <- formatTime defaultTimeLocale "-%Y-%m-%d—%H:%M:%S" <$> getCurrentTime

    let graphFilePath = "output/graph" ++ timeStamp ++ ".dot" 
    let pngFilePath = "output/graph" ++ timeStamp ++ ".png"

    writeFile graphFilePath dotFile

    putStrLn $ "Dot file written to " ++ graphFilePath ++ "\n"

    callCommand $ "dot -Tpng " ++ graphFilePath ++ " -o " ++ pngFilePath

    putStrLn "Graph image generated"

findPathInNFA :: FilePath -> Node -> Node -> Int -> IO ()
findPathInNFA contractFilePath startNode endNode numEdges = do
    contractString <- readFile contractFilePath
    let nfa = runNFAConversion (parseContract contractString)
    let dotFile = visualisePossiblePath nfa startNode endNode numEdges

    timeStamp <- formatTime defaultTimeLocale "-%Y-%m-%d—%H:%M:%S" <$> getCurrentTime

    let graphFilePath = "output/graphWithPathHighlighted" ++ timeStamp ++ ".dot" 
    let pngFilePath = "output/graphWithpPathHighlighted" ++ timeStamp ++ ".png"

    writeFile graphFilePath dotFile

    putStrLn $ "Dot file written to " ++ graphFilePath ++ "\n"

    callCommand $ "dot -Tpng " ++ graphFilePath ++ " -o " ++ pngFilePath

    putStrLn "NFA image with possible path highlighted generated"

convertToPetriNet :: FilePath -> IO()
convertToPetriNet contractFilePath = do
    contractString <- readFile contractFilePath
    let petriNet = printPNContract (contractToPN (parseContract contractString))

    timeStamp <- formatTime defaultTimeLocale "-%Y-%m-%d—%H:%M:%S" <$> getCurrentTime

    let codeFilePath = "output/petriNetPythonCode" ++ timeStamp ++ ".txt"

    writeFile codeFilePath petriNet

    putStrLn $ "Petri net file written to " ++ codeFilePath ++ "\n"

    putStrLn "Copy file content to python for visualization"

checkCompleteness :: FilePath -> IO()
checkCompleteness contractFilePath = do
    contractString <- readFile contractFilePath 

    let ast = parseContract contractString
        completenessReport' = runCheckCompleteness ast
        completenessScore = generateCompletenessScoring ast completenessReport'
        completenessReport = printCompletenessReport completenessReport' completenessScore

    timeStamp <- formatTime defaultTimeLocale "-%Y-%m-%d—%H:%M:%S" <$> getCurrentTime

    let reportFilePath = "output/completenessReport" ++ timeStamp ++ ".txt"

    writeFile reportFilePath completenessReport

    putStrLn "Completeness Report Generated\n"

    putStrLn $ "Check " ++ reportFilePath

convertToDFA :: FilePath -> IO ()
convertToDFA contractFilePath = do
    contractString <- readFile contractFilePath
    let dfa = runDFAConversionFinal (parseContract contractString)
    let graph = dfaToGraph dfa
    let dotFile = visualizeGraphDFA dfa graph

    timeStamp <- formatTime defaultTimeLocale "-%Y-%m-%d—%H:%M:%S" <$> getCurrentTime

    let dotFilePath = "output/dfa" ++ timeStamp ++ ".dot"
    let pngFilePath = "output/dfa" ++ timeStamp ++ ".png"

    writeFile dotFilePath dotFile

    putStrLn $ "Dot file written to " ++ dotFilePath ++ "\n"

    callCommand $ "dot -Tpng " ++ dotFilePath ++ " -o " ++ pngFilePath

    putStrLn "DFA image generated"

-- other object spelling (fol)
