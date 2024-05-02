module Tests where

import Hart
import Types
import qualified Data.IntMap.Strict as IM
import Debug.Trace

type ErrMsg = String
type Success = ()
type TestResult = Either ErrMsg Success
type TestSuite = [TestCase]

-- | Test suite that should run the hart to completion and evaluate it after
-- Maybe I will use HUnit at some point
data TestCase = TestCase { name :: String, 
                        test :: Hart -> TestResult, 
                        start :: Hart }

-- | Gives a testing report of all tests provided in autoTests
testMain :: IO ()
testMain = runTests autoTests

-- | Evaluates the test cases in the given test suite
runTests :: TestSuite -> IO ()
runTests tests = do
    let testedHarts = runHart <$> (start <$> tests)
    let testResults = test <$> tests <*> testedHarts
    putStrLn $ formatTestResults testResults
{-runTests tests = putStrLn $ formatTestResults testresults
    where
        testresults = test <$> tests <*> testedharts
        testedharts = runHart <$> harts
        harts = start <$> tests-}

-- | Formatting of the test results to be printed, could be instanceof Show (later)
formatTestResults :: [TestResult] -> String
formatTestResults = unlines . (map show)

-- Include all test suites to be automatically tested here
autoTests :: TestSuite
autoTests = [
    -- the fibonacci program, should contain fib(N) in x13 after running
    TestCase {  name = "fibonacci",
                test = fibEval, 
                start = testhart fibProg },
    -- tests all ALU immediate operations
    TestCase {  name = "ALU-immediates",
                test = immTestEval,
                start = testhart immTestProg }
    ]

-- | Tests ALU operations involving immediate values, immTestEval evaluates the Hart for correctness
immTestProg :: Prog
immTestProg = [
    Lui 1 0x00000FFF,       -- x1 = 0xFFF00000      Check if it takes all lower 12 bits
    Lui 2 0xFFFFF7FF,       -- x2 = 0x7FF00000      Check that it ignores upper bits
    Auipc 3 10              -- x3 = pc + 10         pc should be 2 here, expected=12
    ]
immTestEval :: Hart -> TestResult
immTestEval Hart {regFile = regs} = Right () -- placeholder

-- | prints first N numbers of the fibonacci sequence
fibProg :: Prog
fibProg = [
    Addi 10 0 20,                   -- a0 = N (change for different N)
    Addi 10 10 (fromIntegral $ -1),  -- take 1 off, compare it to 0 later
    Addi 11 0 0,                    -- a1 = 0
    Addi 12 0 1,                    -- a2 = 1
    Sout 11,                        -- print a1
    Sout 12,                        -- print a2
    Addi 10 10 (fromIntegral $ -2),  -- a0 = a0 - 2 (just printed 2)

    -- loop until it's done
    Add 13 12 11,                   -- a3 = a2 + a1
    Sout 13,
    Addi 11 12 0,                   -- a1 = a2
    Addi 12 13 0,                   -- a2 = a3
    Addi 10 10 (fromIntegral $ -1), -- a0 = a0 - 1
    Bge 10 0 (fromIntegral $ -5)    -- if a0 > 0 then do 1 more iteration
    ]
fibEval :: Hart -> TestResult
fibEval h = if ((regFile h) IM.! 13) == 4181 then Right () else Left $ "x13 != 4181"

-- | For quick testing
testProg :: Prog
testProg = [
    Addi 2 2 10,                -- x2 = x2 + 10
    Addi 2 2 255, Sw 2 0 0,     -- x2 = x2 + 255
    Jal 5 (fromIntegral $ -3)   -- x5 = pc; pc -= 3
    ]

-- | Used to instantiate a "standard" hart for testing, takes a program as input
testhart :: Prog -> Hart
testhart = initHart 0 10

-- | Steps through n instructions of one hart
runN :: Int -> Hart -> Hart
runN n = foldr (.) id $ replicate n processInstr

-- | Steps through the instructions of one hart until the end is reached
runHart :: Hart -> Hart
runHart hart
    | fromIntegral (pc hart) >= length (instrMem hart) = trace msg hart
    | otherwise = runHart $ processInstr hart
    where msg = "Hart " ++ (show $ hartid hart) ++ " finished!"

-- | Runs the fibonacci program as a demo
demoFibonacci :: IO ()
demoFibonacci = print $ runHart $ testhart fibProg
