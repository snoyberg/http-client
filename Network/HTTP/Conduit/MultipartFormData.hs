{-# LANGUAGE CPP, OverloadedStrings #-}
-- | This module handles building multipart/form-data. Example usage:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Network
-- > import Network.HTTP.Conduit
-- > import Network.HTTP.Conduit.MultipartFormData
-- >
-- > import Data.Text.Encoding as TE
-- >
-- > import Control.Monad
-- >
-- > main = withSocketsDo $ withManager $ \m -> do
-- >     req1 <- parseUrl "http://random-cat-photo.net/cat.jpg"
-- >     res <- httpLbs req1 m
-- >     req2 <- parseUrl "http://example.org/~friedrich/blog/addPost.hs"
-- >     flip httpLbs m =<<
-- >         (formDataBody [partBS "title" "Bleaurgh"
-- >                       ,partBS "text" $ TE.encodeUtf8 "矢田矢田矢田矢田矢田"
-- >                       ,partFileSource "file1" "/home/friedrich/Photos/MyLittlePony.jpg"
-- >                       ,partFileRequestBody "file2" "cat.jpg" $ RequestBodyLBS $ responseBody res]
-- >             req2)
module Network.HTTP.Conduit.MultipartFormData
    (
    -- * Part type
     Part(..)
    -- * Constructing parts
    ,partBS
    ,partLBS
    ,partFile
    ,partFileSource
    ,partFileSourceChunked
    ,partFileRequestBody
    ,partFileRequestBodyM
    -- * Building form data
    ,formDataBody
    ,formDataBodyWithBoundary
    -- * Boundary
    ,webkitBoundary
    ,webkitBoundaryPure
    -- * Misc
    ,renderParts
    ,renderPart
    ) where

import Network.HTTP.Client.Types
import Network.Mime
import Network.HTTP.Types (hContentType, methodPost)
import Data.Monoid ((<>))

import Blaze.ByteString.Builder

import Data.Text
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS

import Control.Monad.Trans.State.Strict (state, runState)
import Control.Monad.IO.Class
import System.FilePath
import System.Random
import Data.Array.Base
import System.IO
import Data.Bits
import Data.Word
import Data.Monoid (Monoid(..))
import Control.Monad
import Data.ByteString.Lazy.Internal (defaultChunkSize)

-- | A single part of a multipart message.
data Part = Part
    { partName :: Text -- ^ Name of the corresponding \<input\>
    , partFilename :: Maybe String -- ^ A file name, if this is an attached file
    , partContentType :: Maybe MimeType -- ^ Content type
    , partGetBody :: IO RequestBody -- ^ Action in m which returns the body
                                   -- of a message.
    }

instance Show Part where
    showsPrec d (Part n f c _) =
        showParen (d>=11) $ showString "Part "
            . showsPrec 11 n
            . showString " "
            . showsPrec 11 f
            . showString " "
            . showsPrec 11 c
            . showString " "
            . showString "<m (RequestBody m)>"

partBS :: Text -> BS.ByteString -> Part
partBS n b = Part n mempty mempty $ return $ RequestBodyBS b

partLBS :: Text -> BL.ByteString -> Part
partLBS n b = Part n mempty mempty $ return $ RequestBodyLBS b

-- | Make a 'Part' from a file, the entire file will reside in memory at once.
-- If you want constant memory usage use 'partFileSource'
partFile :: Text -> FilePath -> Part
partFile n f =
    partFileRequestBodyM n f $ do
        liftM RequestBodyBS $ liftIO $ BS.readFile f

-- | Stream 'Part' from a file.
partFileSource :: Text -> FilePath -> Part
partFileSource n f =
    partFileRequestBodyM n f $ do
        size <- liftIO $ withBinaryFile f ReadMode hFileSize
        return $ RequestBodyStream (fromInteger size) $ streamFile f

streamFile :: FilePath -> GivesPopper ()
streamFile fp np =
    withFile fp ReadMode $ np . go
  where
    go h = BS.hGetSome h defaultChunkSize

-- | 'partFileSourceChunked' will read a file and send it in chunks.
--
-- Note that not all servers support this. Only use 'partFileSourceChunked'
-- if you know the server you're sending to supports chunked request bodies.
partFileSourceChunked :: Text -> FilePath -> Part
partFileSourceChunked n f =
    partFileRequestBody n f $ do
        RequestBodyStreamChunked $ streamFile f

-- | Construct a 'Part' from form name, filepath and a 'RequestBody'
--
-- > partFileRequestBody "who_calls" "caller.json" $ RequestBodyBS "{\"caller\":\"Jason J Jason\"}"
--
-- > -- empty upload form
-- > partFileRequestBody "file" mempty mempty
partFileRequestBody :: Text -> FilePath -> RequestBody -> Part
partFileRequestBody n f rqb =
    partFileRequestBodyM n f $ return rqb

-- | Construct a 'Part' from action returning the 'RequestBody'
--
-- > partFileRequestBodyM "cat_photo" "haskell-the-cat.jpg" $ do
-- >     size <- fromInteger <$> withBinaryFile "haskell-the-cat.jpg" ReadMode hFileSize
-- >     return $ RequestBodySource size $ CB.sourceFile "haskell-the-cat.jpg" $= CL.map fromByteString
partFileRequestBodyM :: Text -> FilePath -> IO RequestBody -> Part
partFileRequestBodyM n f rqb =
    Part n (Just f) (Just $ defaultMimeLookup $ pack f) rqb

{-# INLINE cp #-}
cp :: BS.ByteString -> RequestBody
cp bs = RequestBodyBuilder (fromIntegral $ BS.length bs) $ copyByteString bs

renderPart :: BS.ByteString -> Part -> IO RequestBody
renderPart boundary (Part name mfilename mcontenttype get) = liftM render get
  where render renderBody =
            cp "--" <> cp boundary <> cp "\r\n"
         <> cp "Content-Disposition: form-data; name=\""
         <> RequestBodyBS (TE.encodeUtf8 name)
         <> (case mfilename of
                 Just f -> cp "\"; filename=\""
                        <> RequestBodyBS (TE.encodeUtf8 $ pack $ takeFileName f)
                 _ -> mempty)
         <> cp "\""
         <> (case mcontenttype of
                Just ct -> cp "\r\n"
                        <> cp "Content-Type: "
                        <> cp ct
                _ -> mempty)
         <> cp "\r\n\r\n"
         <> renderBody <> cp "\r\n"

-- | Combine the 'Part's to form multipart/form-data body
renderParts :: BS.ByteString -> [Part] -> IO RequestBody
renderParts boundary parts = (fin . mconcat) `liftM` mapM (renderPart boundary) parts
  where fin = (<> cp "--" <> cp boundary <> cp "--\r\n")

-- | Generate a boundary simillar to those generated by WebKit-based browsers.
webkitBoundary :: IO BS.ByteString
webkitBoundary = getStdRandom webkitBoundaryPure

webkitBoundaryPure :: RandomGen g => g -> (BS.ByteString, g)
webkitBoundaryPure g = (`runState` g) $ do
    fmap (BS.append prefix . BS.pack . Prelude.concat) $ replicateM 4 $ do
        randomness <- state $ random
        return [unsafeAt alphaNumericEncodingMap $ randomness `shiftR` 24 .&. 0x3F
               ,unsafeAt alphaNumericEncodingMap $ randomness `shiftR` 16 .&. 0x3F
               ,unsafeAt alphaNumericEncodingMap $ randomness `shiftR` 8 .&. 0x3F
               ,unsafeAt alphaNumericEncodingMap $ randomness .&. 0x3F]
  where
    prefix = "----WebKitFormBoundary"
    alphaNumericEncodingMap :: UArray Int Word8
    alphaNumericEncodingMap = listArray (0, 63)
        [0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48,
         0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F, 0x50,
         0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58,
         0x59, 0x5A, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66,
         0x67, 0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E,
         0x6F, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76,
         0x77, 0x78, 0x79, 0x7A, 0x30, 0x31, 0x32, 0x33,
         0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x41, 0x42]

-- | Add form data to the 'Request'.
--
-- This sets a new 'requestBody', adds a content-type request header and changes the method to POST.
formDataBody :: MonadIO m => [Part] -> Request -> m Request
formDataBody a b = liftIO $ do
    boundary <- webkitBoundary
    formDataBodyWithBoundary boundary a b

-- | Add form data with supplied boundary
formDataBodyWithBoundary :: BS.ByteString -> [Part] -> Request -> IO Request
formDataBodyWithBoundary boundary parts req = do
    body <- renderParts boundary parts
    return $ req
        { method = methodPost
        , requestHeaders =
            (hContentType, "multipart/form-data; boundary=" <> boundary)
          : Prelude.filter (\(x, _) -> x /= hContentType) (requestHeaders req)
        , requestBody = body
        }
