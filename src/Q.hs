{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
module Q where

import System.Random.MWC
import GHC.Generics (Generic)

-- Q values can be extracted sometimes
data Q a =
   Always a | Sometimes Float a
   deriving (Eq, Show, Functor, Traversable, Foldable, Read, Generic)

qOption :: Q p -> p
qOption (Always a) = a
qOption (Sometimes _ a) = a

setChance :: Float -> Q a -> Q a
setChance f = \case
   Always a -> Sometimes f a
   Sometimes _ a -> Sometimes f a

adjustChance :: Float -> Q a -> Q a
adjustChance f (Always b) = adjustChance f (Sometimes 1.0 b)
adjustChance f (Sometimes p b) | f + p >= 1.0 = Always b
                               | f + p <= 0 = Sometimes 0 b
                               | otherwise = Sometimes (f+p) b

runQ :: Gen _ -> (Maybe a -> IO b) -> Q a -> IO b
runQ g action = \case
                   Always t -> action (Just t)
                   Sometimes threshold t -> do
                      p <- uniform g
                      action $ if p < threshold then Just t else Nothing
