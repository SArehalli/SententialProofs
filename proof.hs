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

-- Creates a list of statements that can be derived from applying a rule to 
-- a statement
rApply :: (Statement -> Statement) -> Statement -> [Statement]
rApply f (Compound op phi psi) = 
         nub $ [f $ statement] ++ mapApply ++ applyBoth
               where 
                 statement  = Compound op phi psi
                 applyLeft  = [Compound op phi' psi  | phi' <- rApply f phi]
                 applyRight = [Compound op phi  psi' | psi' <- rApply f psi]
                 applyBoth  = applyLeft ++ applyRight         
                 mapApply   = (map f applyLeft) ++ (map f applyRight)


rApply f (Negation phi) = 
         nub $ [f $ statement] ++ mapApply ++ applyArg
               where 
                 statement = Negation phi
                 applyArg  = [Negation phi' | phi' <- rApply f phi]
                 mapApply  = map f applyArg

rApply f (Atom a) = [Atom a]

-- Conditional Exchange: x -> y <=> !x v y

conditionalExchange :: Statement -> Statement

conditionalExchange (Compound Conditional phi psi) = 
                    Compound Disjunction (Negation phi) psi

conditionalExchange (Compound Disjunction phi psi) = 
                    Compound Conditional (Negation phi) psi 

conditionalExchange phi = phi

-- Biconditional Exchange: x = y <=> x -> y ^ y --> x

biconditionalExchange :: Statement -> Statement

biconditionalExchange (Compound Biconditional phi psi) = 
                      Compound Conjunction forward backward
                            where
                              forward  = Compound Conditional phi psi
                              backward = Compound Conditional psi phi 

biconditionalExchange phi = phi

-- Double Negation: !!x <=> x

doubleNegation :: Statement -> Statement
doubleNegation (Negation (Negation phi)) = phi
doubleNegation phi = phi

-- Demorgan's Laws: !(x ^ y) <=> !x v !y
--                  !(x v y) <=> !x ^ !y

demorgan :: Statement -> Statement
demorgan (Negation (Compound Conjunction phi psi)) = 
         Compound Disjunction (Negation phi) (Negation psi)

demorgan (Negation (Compound Disjunction phi psi)) = 
         Compound Conjunction (Negation phi) (Negation psi)


demorgan (Compound Conjunction (Negation phi) (Negation psi)) = 
         Negation (Compound Disjunction phi psi)

demorgan (Compound Disjunction (Negation phi) (Negation psi)) = 
         Negation (Compound Conjunction phi psi)

demorgan phi = phi

-- Commutation: x v y <=> y v x
--              x ^ y <=> y ^ x

commute :: Statement -> Statement

commute (Compound Disjunction phi psi) = Compound Disjunction psi phi

commute (Compound Conjunction phi psi) = Compound Conjunction psi phi

commute phi = phi

-- Association: x ^ (y ^ z) <=> (x ^ y) ^ z
--              x v (y v z) <=> (x v y) ^ z

associate :: Statement -> Statement

associate (Compound Disjunction phi (Compound Disjunction psi xi)) = 
          (Compound Disjunction (Compound Disjunction phi psi) xi)

associate (Compound Conjunction phi (Compound Conjunction psi xi)) = 
          (Compound Conjunction (Compound Conjunction phi psi) xi)

associate phi = phi

-- Exportation: (x ^ y) -> x <=> x -> (y -> z)

export :: Statement -> Statement
export (Compound Conditional (Compound Conjunction phi psi) xi) = 
       Compound Conditional phi (Compound Conditional psi xi) 
export (Compound Conditional phi (Compound Conditional psi xi))  = 
       Compound Conditional (Compound Conjunction phi psi) xi 
export phi = phi

-- Distribution: x ^ (y v z) = (x v y) ^ (x v z)
--               x v (y ^ z) = (x ^ y) v (x ^ z)

distribute :: Statement -> Statement

distribute (Compound Disjunction phi (Compound Conjunction psi xi)) = 
           Compound Conjunction leftConjunct rightConjunct
                where
                  leftConjunct  = Compound Disjunction phi psi
                  rightConjunct = Compound Disjunction psi phi 

distribute (Compound Conjunction phi (Compound Disjunction psi xi)) = 
           Compound Disjunction leftDisjunct rightDisjunct
                where
                  leftDisjunct  = Compound Conjunction phi psi
                  rightDisjunct = Compound Conjunction psi phi 

distribute phi = phi

-- Contraposition: x -> y <=> !y -> !x

contrapose :: Statement -> Statement

contrapose (Compound Conditional phi psi) = 
           Compound Conditional (Negation psi) (Negation phi)

contrapose phi = phi

-- Duplication: x ^ x <=> x
--              x v x <=> x

duplicate :: Statement -> Statement

duplicate (Compound Conjunction phi psi) =  if phi == psi 
                                            then phi 
                                            else (Compound Conjunction phi psi)

duplicate (Compound Disjunction phi psi) =  if phi == psi 
                                            then phi 
                                            else (Compound Disjunction phi psi)

duplicate phi = phi

-- print out a statement nicely

printNeat :: (Show a) => [a] -> IO () 
printNeat [] = return ()
printNeat a  = do
                 print $ head a
                 printNeat $ tail a

main = do
         let x   = Negation (Compound Conjunction (Atom 'A')  (Atom 'A'))
         print x
         putStrLn "Post Exchange:"
         printNeat $ nub (rApply duplicate x)
