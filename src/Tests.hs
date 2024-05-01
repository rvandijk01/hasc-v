module Tests where

import Hart
import Types

-- | prints first N numbers of the fibonacci sequence
fibprog :: Prog
fibprog = [
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

-- | For quick testing
testprog :: Prog
testprog = [
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
runHart :: Hart -> IO ()
runHart hart
    | fromIntegral (pc hart) >= length (instrMem hart) = putStrLn msg
    | otherwise = runHart $ processInstr hart
    where msg = "Hart " ++ (show $ hartid hart) ++ " finished!"

-- | Runs the fibonacci program as a demo
demoFibonacci :: IO ()
demoFibonacci = runHart $ testhart fibprog
