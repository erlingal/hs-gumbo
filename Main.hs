{-# LANGUAGE LambdaCase #-}

import Control.Monad
import qualified Data.ByteString.Lazy as L
import Data.Data
import Text.HTML.Gumbo
import System.Environment (getArgs)

clean = \case
  E a b xs -> [E a b (foldMap clean xs)]
  T Whitespace _ -> []
  x -> [x]

dump = L.putStr <=< prettyHTML pure 

main =
  getArgs >>= \case
    [] -> dump =<< L.getContents
    xs -> mapM_ (dump <=< L.readFile) xs
