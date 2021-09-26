{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.HTML.Gumbo (html, prettyHTML, parseStrict, Node (..), TextKind (..)) where

import Data.ByteString.Builder
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Text.HTML.Gumbo.C (parseStrict)
import Text.HTML.Gumbo.Types

type LB = L.ByteString
type SB = B.ByteString
type B = Builder

html, prettyHTML :: Functor f => (Node -> f Node) -> (LB -> f LB)
html f bs = toLazyByteString . dump id "" <$> f (parseStrict (L.toStrict bs))

prettyHTML f bs = toLazyByteString . dump (<>"\t") "\n" <$> f (parseStrict (L.toStrict bs))

rawTypes, voidTypes :: [SB]
rawTypes = ["script", "styles", "xmp", "iframe", "noembed", "noframes", "plaintext"]
voidTypes = ["basefont", "bgsound", "frame", "keygen", "area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source", "track", "wbr"]

dump :: (B -> B) -> B -> Node -> B
dump more ind = \case
  E "" [] chs -> foldMap (dump more ind) chs
  E h ats chs -> ind <> "<" <> byteString h <> mconcat (map attr ats) <> ">" <> tail
    where
      inner (T _ t) | elem h rawTypes = ind <> byteString t
      inner o = dump more (more ind) o
      tail | elem h voidTypes && null chs = ""
           | otherwise = foldMap inner chs <> ind <> "</" <> byteString h <> ">"      
  T _ t -> ind <> escape t
  Comment s -> ind <> "<!--" <> byteString s <> "-->"

attr :: (SB, SB) -> B
attr (k, v) = " " <> byteString k <> "=\"" <> escape v <> "\""

escape :: SB -> Builder
escape = foldMap f . B.unpack
  where
    f 0x26 = "&amp;"
    f 0x22 = "&quot;"
    f 0x3C = "&lt;"
    f c = word8 c