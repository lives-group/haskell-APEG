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
import qualified Text.APEG.Semantics.APEGSem as Sem
import Control.Applicative
    
import Data.Char    
import Data.Proxy    
import Data.Singletons.Prelude   
import Data.Singletons.Prelude.List
    

foo :: PExp '[ '("a", Bool), '("b", Char)] ()
foo = Set (sing :: Sing "a") True 

foo' :: PExp '[ '("a", Bool), '("b", Char)] ()
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
                 
pb :: PExp '[ '("x0", Int), '("x1", Int)] ()
pb = pz <|> po
     where
       pz, po :: PExp '[ '("x0", Int), '("x1", Int)] ()
       pz = char '0' *> (set (sing :: Sing "x1") 1)
       po = char '1' *> (set (sing :: Sing "x1") 1)
       char c = sat (c ==)       

pt :: PExp '[ '("x0", Int), '("x1", Int)] ()
pt = pb <* star pb      
                

ps :: PExp '[ '("x0", Int), '("x1", Int)] ()
ps = pt      
     
main :: IO ()
main = do
         let
           (r,att) = Sem.runAPEG mytest1 "ab"
         putStrLn $ show att
         print r         
