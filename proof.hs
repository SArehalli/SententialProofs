import System.Environment (getArgs)
import Rules 

-- print out a statement nicely

printNeat :: (Show a) => [a] -> IO () 
printNeat [] = return ()
printNeat a  = do
                 print $ head a
                 printNeat $ tail a

-- sentential proof verification: "does line n+1 follow from n?"

verify :: [Statement] -> [String]-> Bool

verify (x:y:zs) (a:bs) = if y `elem` (applyRule a x)
                       then verify (y:zs) bs
                       else False
verify (x:[]) [] = True

-- Apply a rule using it's string abbreviation

applyRule :: String -> Statement -> [Statement]
applyRule rule a
    | rule == "CE"      = rApply conditionalExchange a
    | rule == "BE"      = rApply biconditionalExchange a
    | rule == "Dup"     = rApply duplication a
    | rule == "Contra"  = rApply contraposition a
    | rule == "Dist"    = rApply distribution a
    | rule == "Commute" = rApply commutation a
    | rule == "Assoc"   = rApply association a
    | rule == "DeM"     = rApply demorgan a
    | rule == "DNeg"    = rApply doubleNegation a

-- Translate infix to postfix: a^b to ab^

toPostfix :: [Char] -> String -> String

toPostfix stack (x:xs) 
    | x `elem` operators = toPostfix (x:stack) xs
    | x == ')'           = inParens ++ toPostfix outParens xs
    | otherwise = x:(toPostfix stack xs)
    where
        inParens  = reverse $ takeWhile (/='(') stack
        outParens = tail $ dropWhile (/='(') stack

toPostfix stack [] = reverse stack 

-- Translate postfix into a Statement ADT

operators = "(~v^=>" 

parsePostfix :: [Statement] -> String -> Statement

parsePostfix (b:a:stack) (x:xs)
    | x == '^'  = parsePostfix ((Compound Conjunction a b):stack) xs
    | x == 'v'  = parsePostfix ((Compound Disjunction a b):stack) xs
    | x == '>'  = parsePostfix ((Compound Conditional a b):stack) xs
    | x == '='  = parsePostfix ((Compound Biconditional a b):stack) xs

parsePostfix (a:stack) (x:xs)
    | x == '~'  = parsePostfix ((Negation a):stack) xs

parsePostfix stack (x:xs) = parsePostfix ((Atom x):stack) xs

parsePostfix stack [] = head stack


-- main - Parse CLAs, open file, split lines, parse statements, and feed 
--      - to verify.

main = do
         [f] <- getArgs
         s <- readFile f
         let statements     = map (takeWhile (/=';')) $ lines s
         let parsedStates   = map (parsePostfix [] . toPostfix []) statements
         let justifications = tail $ map (tail . dropWhile (/=';')) $ lines s
         print $ verify parsedStates justifications

