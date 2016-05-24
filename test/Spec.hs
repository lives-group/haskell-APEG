{-#LANGUAGE FlexibleInstances, 
            GADTs, 
            TypeFamilies,
            UndecidableInstances,
            TypeOperators, 
            DataKinds,
            KindSignatures,
            ScopedTypeVariables,
            PolyKinds #-}


import Text.APEG.Syntax.APEGSyn
import Control.Applicative
    
import Data.Char    
import Data.Proxy    
import Data.Singletons.Prelude   
import Data.Singletons.Prelude.List
    

foo :: PExp '[ '("lang", PExp '[ '("a", Bool), '("b", Char)] a), '("a", Bool), '("b", Char)] ()
foo = Set (sing :: Sing "a") True 

foo' :: PExp '[ '("lang", PExp '[ '("a", Bool), '("b", Char)] a),'("a", Bool), '("b", Char)] ()
foo' = Set (sing :: Sing "b") 'a' 

      
mytest1 :: APEG '[ '("a", Bool), '("b", Char)] Char      
mytest1 = APEG (((\_ _ c -> c) <$> foo <*> foo' <*> Get (sing :: Sing "b")))

-- more tests

digit :: PExp env Char
digit = Sat isDigit         

number :: PExp env Int
number = f <$> Star digit
         where
           f = foldl (\a b -> a * 10 + b) 0 . map g
           g c = ord c - ord '0'                 



main :: IO ()
main = putStrLn "Test suite not yet implemented"
