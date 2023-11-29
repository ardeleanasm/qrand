{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           JsonParser
import           System.Exit 
import           Data.Either

data QRandValue
  = QDoubles [Double]
  deriving (Show, Eq)

newtype QRand a = QRand
  {
    runQRand :: [Double] -> Maybe a
  }

instance Functor QRand where
  fmap f (QRand q) = QRand $ \input -> do
    x <- q input
    Just (f x)

instance Applicative QRand where
  pure q = QRand $ \_ -> Just q
  (QRand q1) <*> (QRand q2) = QRand ((<*>) <$> q1 <*> q2)
  


instance Alternative QRand where
  empty = QRand $ \_ -> Nothing
  (QRand q1) <|> (QRand q2) = QRand $ \input -> q1 input <|> q2 input

--(>>=) :: m a -> (a -> m b) -> m b
--  (>>) :: m a -> m b -> m b
--  return :: a -> m a
--  {-# MINIMAL (>>=) #-}

instance Monad QRand where
  return = pure
  (QRand q1) >>= f = QRand $ \t -> do
    a <- q1 t
    runQRand (f a) t






qrandDoubleArray::QRand QRandValue
qrandDoubleArray = pure $ QDoubles [1,2,3]

qrandRequest::String -> QRand QRandValue
qrandRequest s = pure $ QDoubles xs
  where
    xs = stringToList s




stringToList :: String -> [Double]
stringToList s = read s ::[Double]



main :: IO ()
main = do
  let inputString = "[1,2,3,4,5]"
      result = stringToList inputString
  putStrLn ( "Parsed string: " ++ show result)
--  putStrLn "Parsing [1,2,3,4,5]"
--  case runParser jsonValue $ Input 0 "[1.5,2.6,3.2]" of
--    Right (input, resultJson) -> do
--      putStrLn ("Success " ++ show resultJson)
--    Left (ParserError loc msg) -> do
--      putStrLn $ "Error: Parser failed at character " ++ show loc ++ ": " ++msg




