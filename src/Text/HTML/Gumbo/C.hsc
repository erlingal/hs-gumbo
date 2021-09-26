{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.HTML.Gumbo.C (parseStrict) where

import Text.HTML.Gumbo.Types

#include "gumbo.h"

import Control.Monad
import Control.Exception
import System.IO.Unsafe

import qualified Data.ByteString as B

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Data.String.Conversions

data GOutput
data GNode
data GOptions
data GStrPiece
data GAttr
data GVector x

type BS = B.ByteString

foreign import ccall gumbo_parse :: CString -> IO (Ptr GOutput)
foreign import ccall gumbo_destroy_output :: Ptr GOptions -> Ptr GOutput -> IO ()

foreign import ccall gumbo_tag_from_original_text :: Ptr GStrPiece -> IO ()

foreign import ccall "&" kGumboDefaultOptions :: Ptr GOptions

pack x | x == nullPtr = pure "NULL"
pack x = B.packCString x

-- Node: type/parent/ document/element/text
-- Doc: name/children(Vec)

mapVec :: (Ptr x -> IO y) -> Ptr (GVector x) -> IO [y]
mapVec f vec = do
  base <- (#peek GumboVector, data) vec :: IO (Ptr (Ptr x))
  len <- fromIntegral <$> ((#peek GumboVector, length) vec :: IO CInt)
  mapM (f <=< peekElemOff base) [0..len-1]

attr :: Ptr GAttr -> IO (BS, BS)
attr p = (,) <$> (pack =<< (#peek GumboAttribute, name) p)
             <*> (pack =<< (#peek GumboAttribute, value) p)

extractTagName :: Ptr GNode -> IO BS
extractTagName element = do
  allocaBytesAligned (#size GumboStringPiece) (#alignment GumboStringPiece) $ \piece -> do
    copyBytes piece ((#ptr GumboNode, v.element.original_tag) element) (#size GumboStringPiece)
    gumbo_tag_from_original_text piece
    ptr <- (#peek GumboStringPiece, data) piece
    len <- (#peek GumboStringPiece, length) piece    
    B.packCStringLen (ptr, len)
             
slurp :: Ptr GNode -> IO Node
slurp p = do
  typ <- (#peek GumboNode, type) p
  let element = E <$> extractTagName p
          <*> mapVec attr ((#ptr GumboNode, v.element.attributes) p)
          <*> mapVec slurp ((#ptr GumboNode, v.element.children) p)
  let text kind = kind <$> (pack =<< (#peek GumboNode, v.text.text) p)
  case typ :: CInt of
     #{const GUMBO_NODE_DOCUMENT} ->
        E "" <$> pure []
             <*> mapVec slurp ((#ptr GumboNode, v.document.children) p)
     #{const GUMBO_NODE_ELEMENT} -> element
     #{const GUMBO_NODE_TEXT} -> text (T Plain)
     #{const GUMBO_NODE_CDATA} -> text (T CData)
     #{const GUMBO_NODE_COMMENT} -> text Comment
     #{const GUMBO_NODE_WHITESPACE} -> text (T Whitespace)
     #{const GUMBO_NODE_TEMPLATE} -> element
     _ -> return . Comment . cs $ "node type " ++ show typ

-- | Parses a document, making a copy on the c stack
parseStrict :: BS -> Node
parseStrict s = unsafePerformIO $ do
  B.useAsCString s $ \st ->
    bracket (gumbo_parse st) (gumbo_destroy_output kGumboDefaultOptions) $ \g -> do
      doc <- (#peek GumboOutput, document) g
      slurp doc
