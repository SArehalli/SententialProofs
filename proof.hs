import Data.List

data LogicalOperator = Conjunction 
                     | Disjunction 
                     | Conditional
                     | Biconditional
                     deriving (Show, Eq)

data Statement = Atom Char
               | Compound LogicalOperator Statement Statement
               | Negation Statement
               deriving (Show, Eq)


recurseApply :: (Statement -> Statement) -> Statement -> [Statement]
recurseApply f (Compound op phi psi) = nub $ [f $ Compound op phi psi] ++ (map f left) ++ (map f right) ++ left ++ right
                                       where 
                                         left  = [Compound op phi' psi  | phi' <- recurseApply f phi]
                                         right = [Compound op phi  psi' | psi' <- recurseApply f psi]
recurseApply f (Negation phi) = [f $ Negation phi] ++ recursed ++ (map f recursed)
                                 where 
                                   recursed = [Negation phi' | phi' <- recurseApply f phi]
recurseApply f (Atom a) = [Atom a]


conditionalExchange :: Statement -> Statement
conditionalExchange (Compound Conditional phi psi) = Compound Disjunction (Negation phi) psi
conditionalExchange (Compound Disjunction phi psi) = Compound Conditional (Negation phi) psi 
conditionalExchange phi = phi

biconditionalExchange :: Statement -> Statement
biconditionalExchange (Compound Biconditional phi psi) = Compound Conjunction (Compound Conditional phi psi) (Compound Conditional psi phi)
biconditionalExchange phi = phi

doubleNegation :: Statement -> Statement
doubleNegation (Negation (Negation phi)) = phi
doubleNegation phi = phi

printNeat :: (Show a) => [a] -> IO () 
printNeat [] = return ()
printNeat a  = do
                 print $ head a
                 printNeat $ tail a

main = do
         let phi = Compound Biconditional (Atom 'A') (Atom 'B')
         let psi = Compound Conjunction (Negation (Atom 'C')) (Atom 'A')
         let x   = Compound Disjunction phi psi
         print x
         putStrLn "Post Exchange:"
         printNeat $ foldl (++) [] $ map (recurseApply biconditionalExchange) (recurseApply conditionalExchange x)
