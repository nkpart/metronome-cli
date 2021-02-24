{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
module Q where

-- Q values can be executed sometimes
import System.Random.MWC

data Q a =
   Always a | Sometimes Float a
   deriving (Eq, Show, Functor, Traversable, Foldable, Read)

(??) :: Q a -> Float -> Q a
Always b ?? f = Sometimes f b
Sometimes _ b ?? f = Sometimes f b

(++=) :: Q a -> Float -> Q a
Always b ++= f = Sometimes 1.0 b ++= f
Sometimes p b ++= f | f + p >= 1.0 = Always b
                    | f + p <= 0 = Sometimes 0 b
                    | otherwise = Sometimes (f+p) b

runQ g action = \case
                   Always t -> action (Just t)
                   Sometimes threshold t -> do
                      p <- uniform g
                      action $ if p < threshold then Just t else Nothing
