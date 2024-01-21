module Main where

import Prelude
  ( ($), (.)
  , Either(..)
  , Int, (>)
  , String, (++), concat, unlines
  , Show, show
  , IO, (>>), (>>=), mapM_, putStrLn
  , FilePath
  , getContents, readFile, error
  )
import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad      ( when )
import Data.Graph.Inductive.Graph 

import AbsCoLa   
import LexCoLa   ( Token, mkPosToken )
import ParCoLa   ( pContract, myLexer )
import PrintCoLa ( Print, printTree )
import SkelCoLa  ()
import AstToFOL
import FOLToTPTP
import AstToNFA
import AstToPetriNet
import ExampleContracts
import CheckCompleteness
import AstToDFA

import qualified Data.Map as Map
import Control.Monad.State
import System.IO 
import System.Process

type Err        = Either String
type ParseFun a = [Token] -> Err a
type Verbosity  = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()
run v p s =
  case p ts of
    Left err -> do
      putStrLn "\nParse              Failed...\n"
      putStrV v "Tokens:"
      mapM_ (putStrV v . showPosToken . mkPosToken) ts
      putStrLn err
      exitFailure
    Right tree -> do
      putStrLn "\nParse Successful!"
      showTree v tree
  where
  ts = myLexer s
  showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

runAST :: ParseFun Contract -> String -> Either String Contract
runAST p s = case p ts of
  Right contract -> Right contract
  Left parseError -> Left $ "Parse failed: " 
  where
    ts = myLexer s

parseSentence :: String -> Contract
parseSentence sentence =
  case runAST pContract sentence of
    Left errMsg -> error ("Parsing failed: " ++ errMsg)
    Right parsedContract -> parsedContract

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree = do
  putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    []         -> getContents >>= run 2 pContract
    "-s":fs    -> mapM_ (runFile 0 pContract) fs
    fs         -> mapM_ (runFile 2 pContract) fs

-- Function to interactively get user input for a contract or performance
getUserInput :: String -> IO String
getUserInput prompt = do
  putStrLn prompt
  putStr "> "
  hFlush stdout
  getLine


checkInconsistencyCL :: IO ()
checkInconsistencyCL = do

    contractString <- getUserInput "Enter a contract:"
    let (contract, dateDictionary, tempQuanDictionary) = runFOLConversion' (parseSentence contractString)
    let tptpContract = folToTPTPString "contract" contract

    putStrLn "\n"

    performanceString <- getUserInput "Enter a performance:"
    let performance = evalState (contractToFOLWithCheck (parseSentence performanceString)) (dateDictionary, tempQuanDictionary)
    let tptpPerformance = folToTPTPString "performance" performance

    putStrLn "%TPTP representation for the contract:"
    putStrLn tptpContract
    putStrLn "\n"

    putStrLn "%TPTP representation for the performance:"
    putStrLn tptpPerformance
    putStrLn "\n"

    putStrLn $ "fof(mustCondition, axiom, (" ++
      "! [X,Y,O,D] :" ++
      "(mustDeliver(X,Y,O,D) => delivered(X,Y,O,D)) & " ++
      "(mustPay(X,Y,O,D) => paid(X,Y,O,D)) & " ++
      "(mustCharge(X,Y,O,D) => charged(X,Y,O,D)) & " ++
      "(mustRefund(X,Y,O,D) => refunded(X,Y,O,D))" ++
      "))."

    putStrLn "\n"

    putStrLn $ "fof(forbiddenContradiction, axiom, (" ++
      "! [X, Y, D, O] : (" ++
      "(~ mayDeliver(X, Y, O, D) & delivered(X, Y, O, D)) |" ++
      "(~ mayPay(X, Y, O, D) & paid(X, Y, O, D)) |" ++
      "(~ mayCharge(X, Y, O, D) & charged(X, Y, O, D)) |" ++
      "(~ mayRefund(X, Y, O, D) & refunded(X, Y, O, D))" ++
      " => $false" ++
      ")" ++
      "))."

    putStrLn "\n"

    putStrLn $ "fof(mustWithTemporalQuantifierContradiction, axiom, (" ++
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

checkInconsistency :: FilePath -> FilePath -> IO ()
checkInconsistency contractFilePath performanceFilePath = do
    contractString <- readFile contractFilePath
    let (contract, dateDictionary, tempQuanDictionary) = runFOLConversion' (parseSentence contractString)
    let tptpContract = folToTPTPString "contract" contract
    let contractComment = "%TPTP representation for the contract:"
    let contractFullForm = contractComment ++ "\n\n" ++ tptpContract

    performanceString <- readFile performanceFilePath
    let performance = evalState (contractToFOLWithCheck (parseSentence performanceString)) (dateDictionary, tempQuanDictionary)
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

    -- Write the combined output to a new file
    writeFile "checkInconsistency.p" combinedOutput

    putStrLn "Combined TPTP representation written to checkInconsistency.p\n"
      
    -- Run the Vampire command and capture its output
    output <- readProcess "./vampire_rel" ["--input_file", "checkInconsistency.p"] ""

    -- Display the output in the terminal
    putStrLn "Vampire command output:"
    putStrLn output
    
convertToNFA :: FilePath -> IO ()
convertToNFA contractFilePath = do
    contractString <- readFile contractFilePath
    let nfa = runNFAConversion (parseSentence contractString)
    let graph = nfaToGraph nfa
    let dotFile = visualizeGraph nfa graph

    writeFile "nfa.dot" dotFile

    putStrLn "Dot file written to nfa.dot\n"

    callCommand $ "dot -Tpng nfa.dot -o nfa.png"

    putStrLn "NFA image generated"

findPathInNFA :: FilePath -> Node -> Node -> Int -> IO ()
findPathInNFA contractFilePath startNode endNode numEdges = do
    contractString <- readFile contractFilePath
    let nfa = runNFAConversion (parseSentence contractString)
    let dotFile = visualisePossiblePath nfa startNode endNode numEdges

    writeFile "nfaWithPathHighlighted.dot" dotFile

    putStrLn "Dot file written to nfaWithPathHighlighted.dot\n"

    callCommand $ "dot -Tpng nfaWithPathHighlighted.dot -o nfaWithPathHighlighted.png"

    putStrLn "NFA image with possible path highlighted generated"

convertToPetriNet :: FilePath -> IO()
convertToPetriNet contractFilePath = do
    contractString <- readFile contractFilePath
    let petriNet = printPNContract (contractToPN (parseSentence contractString))

    writeFile "petriNetPythonCode.txt" petriNet

    putStrLn "Petri net file written to petriNetPythonCode.txt"

    putStrLn "Copy file content to python for visualization"

checkCompleteness :: FilePath -> IO()
checkCompleteness contractFilePath = do
    contractString <- readFile contractFilePath 

    let ast = parseSentence contractString
        completenessReport = printCompletenessReport $ runCheckCompleteness ast

    writeFile "completenessReport.txt" completenessReport

    putStrLn "Completeness Report Generated"

    putStrLn "Check completenessReport.txt"

convertToDFA :: FilePath -> IO ()
convertToDFA contractFilePath = do
    contractString <- readFile contractFilePath
    let dfa = runDFAConversionFinal (parseSentence contractString)
    let graph = dfaToGraph dfa
    let dotFile = visualizeGraphDFA dfa graph

    writeFile "dfa.dot" dotFile

    putStrLn "Dot file written to dfa.dot\n"

    callCommand $ "dot -Tpng dfa.dot -o dfa.png"

    putStrLn "DFA image generated"


