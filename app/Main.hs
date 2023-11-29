{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns      #-}

module Main where

import           Control.Applicative


data QRandValue
  = QNull
  | QInts [Int]
  | QDoubles [Double]
  deriving (Show, Eq)

newtype QRand a = QRand
  {
    runQRand :: String -> Maybe a
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



qrandNull :: QRand QRandValue
qrandNull = (\_ -> QNull) <$> pure QNull

qrandIntArray::QRand QRandValue
qrandIntArray = pure $ QInts [1,2,3]

qrandDoubleArray::QRand QRandValue
qrandDoubleArray = pure $ QDoubles [1.0, 2.0, 3.0]

qrandRequest::QRand QRandValue
qrandRequest = qrandNull <|> qrandIntArray <|> qrandDoubleArray

main :: IO ()
main = putStrLn "Hello World"
