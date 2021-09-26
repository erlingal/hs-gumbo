module Text.HTML.Gumbo.Types (Node (..), TextKind (..)) where

import qualified Data.ByteString as B

type BS = B.ByteString

data TextKind = CData | Plain | Whitespace

data Node
  = E BS [(BS, BS)] [Node]
  | T TextKind BS
  | Comment BS