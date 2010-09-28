{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

-- |
-- Module      : Network.TLS.Client
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- the Client module contains the necessary calls to create a connecting TLS socket
-- aka. a client socket.
--
module Network.TLS.Client
	( TLSClientParams(..)
	, TLSClientCallbacks(..)
	, TLSStateClient
	, newIState
	-- * low level packet sending receiving.
	, recvPacket
	, sendPacket
	-- * API, warning probably subject to change
	, connect
	, sendData
	, recvData
	, close
    -- * Enumerator interface
    , clientEnum
    , clientEnumSimple
	) where

import Data.Maybe
import Data.Word
import Control.Applicative ((<$>))
import Data.Certificate.X509
import Network.TLS.Cipher
import Network.TLS.Struct
import Network.TLS.Packet
import Network.TLS.State
import Network.TLS.Sending
import Network.TLS.Receiving
import Network.TLS.SRandom
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import System.IO (Handle, hFlush)
import Data.List (find)
import Data.IORef
import qualified Data.Enumerator as E
import qualified Control.Monad.IO.Class as Trans
import qualified Control.Monad.Trans.Class as Trans
import qualified Codec.Crypto.AES.Random as AESRand
import Data.Bits
import Control.Monad (when, unless)

data TLSClientCallbacks = TLSClientCallbacks
	{ cbCertificates :: Maybe ([Certificate] -> IO Bool) -- ^ optional callback to verify certificates
	}

instance Show TLSClientCallbacks where
	show _ = "[callbacks]"

data TLSClientParams = TLSClientParams
	{ cpConnectVersion  :: Version            -- ^ client version we're sending by default
	, cpAllowedVersions :: [Version]          -- ^ allowed versions from the server
	, cpSession         :: Maybe [Word8]      -- ^ session for this connection
	, cpCiphers         :: [Cipher]           -- ^ all ciphers for this connection
	, cpCertificate     :: Maybe Certificate  -- ^ an optional client certificate
	, cpCallbacks       :: TLSClientCallbacks -- ^ user callbacks
	} deriving (Show)

data TLSStateClient = TLSStateClient
	{ scParams   :: TLSClientParams -- ^ client params and config for this connection
	, scTLSState :: TLSState        -- ^ client TLS State for this connection
	, scCertRequested :: Bool       -- ^ mark that the server requested a certificate
	} deriving (Show)

type IState = IORef TLSStateClient

newIState :: TLSClientParams -> SRandomGen -> IO IState
newIState params rng = newIORef $ TLSStateClient
    { scParams = params
    , scTLSState = state
    , scCertRequested = False
    }
  where
    state = (newTLSState rng) { stVersion = cpConnectVersion params, stClientContext = True }

{- | receive a single TLS packet or on error a TLSError -}
recvPacket :: IState -> Handle -> IO (Either TLSError Packet)
recvPacket istate handle = do
	hdr <- B.hGet handle 5 >>= return . decodeHeader
	case hdr of
		Left err                          -> return $ Left err
		Right header@(Header _ _ readlen) -> do
			content <- B.hGet handle (fromIntegral readlen)
			runIStateWrapper (readPacket header (EncryptedData content)) istate

newtype IStateWrapper a = IStateWrapper { runIStateWrapper :: IState -> IO a }
instance Monad IStateWrapper where
	return = IStateWrapper . const . return
	(IStateWrapper f) >>= g = IStateWrapper $ \ i -> do
		x <- f i
		runIStateWrapper (g x) i
instance MonadTLSState IStateWrapper where
	getTLSState = IStateWrapper $ fmap scTLSState . readIORef
	putTLSState s = IStateWrapper $ \i -> do
		cs <- readIORef i
		writeIORef i cs { scTLSState = s }

{- | send a single TLS packet -}
sendPacket :: IState -> Handle -> Packet -> IO ()
sendPacket istate handle pkt = do
	dataToSend <- runIStateWrapper (writePacket pkt) istate
	B.hPut handle dataToSend

recvServerHello :: IState -> Handle -> IO ()
recvServerHello istate handle = do
	state' <- readIORef istate
	let ciphers = cpCiphers $ scParams state'
	let allowedvers = cpAllowedVersions $ scParams state'
	let callbacks = cpCallbacks $ scParams state'
	pkt <- recvPacket istate handle
	let hs = case pkt of
		Right (Handshake h) -> h
		Left err            -> error ("error received: " ++ show err)
		Right x             -> error ("unexpected packet received, expecting handshake " ++ show x)
	case hs of
		ServerHello ver _ _ cipher _ _ -> do
			case find ((==) ver) allowedvers of
				Nothing -> error ("received version which is not allowed: " ++ show ver)
				Just _  -> do
					state <- readIORef istate
					let st = state { scTLSState = (scTLSState state) { stVersion = ver } }
					writeIORef istate st

			case find ((==) cipher . cipherID) ciphers of
				Nothing -> error "no cipher in common with the server"
				Just c  -> do
					state <- readIORef istate
					let st = state { scTLSState = (scTLSState state) { stCipher = Just c } }
					writeIORef istate st
			recvServerHello istate handle
		CertRequest _ _ _  -> do
			sc <- readIORef istate
			writeIORef istate sc { scCertRequested = True }
			recvServerHello istate handle
		Certificates certs -> do
			valid <- maybe (return True) (\cb -> cb certs) (cbCertificates callbacks)
			unless valid $ error "certificates received deemed invalid by user"
			recvServerHello istate handle
		ServerHelloDone    -> return ()
		_                  -> error "unexpected handshake message received in server hello messages"

connectSendClientHello :: IState -> Handle -> ClientRandom -> IO ()
connectSendClientHello istate handle crand = do
	state <- readIORef istate
	let ver = cpConnectVersion $ scParams state
	let ciphers = cpCiphers $ scParams state
	sendPacket istate handle $ Handshake (ClientHello ver crand (Session Nothing) (map cipherID ciphers) [ 0 ] Nothing)

connectSendClientCertificate :: IState -> Handle -> IO ()
connectSendClientCertificate istate handle = do
	certRequested <- scCertRequested <$> readIORef istate
	when certRequested $ do
		clientCert <- cpCertificate . scParams <$> readIORef istate
		sendPacket istate handle $ Handshake (Certificates $ maybe [] (:[]) clientCert)

connectSendClientKeyXchg :: IState -> Handle -> ClientKeyData -> IO ()
connectSendClientKeyXchg istate handle prerand = do
	ver <- cpConnectVersion . scParams <$> readIORef istate
	sendPacket istate handle $ Handshake (ClientKeyXchg ver prerand)

connectSendFinish :: IState -> Handle -> IO ()
connectSendFinish istate handle = do
	cf <- runIStateWrapper (getHandshakeDigest True) istate
	sendPacket istate handle (Handshake $ Finished $ B.unpack cf)

{- | connect through a handle as a new TLS connection. -}
connect :: IState -> Handle -> ClientRandom -> ClientKeyData -> IO ()
connect istate handle crand premasterRandom = do
	connectSendClientHello istate handle crand
	recvServerHello istate handle
	connectSendClientCertificate istate handle

	connectSendClientKeyXchg istate handle premasterRandom

	{- maybe send certificateVerify -}
	{- FIXME not implemented yet -}

	sendPacket istate handle (ChangeCipherSpec)
	hFlush handle

	{- send Finished -}
	connectSendFinish istate handle
	
	{- receive changeCipherSpec -}
	pktCCS <- recvPacket istate handle
	case pktCCS of
		Right ChangeCipherSpec -> return ()
		x                      -> error ("unexpected reply. expecting change cipher spec  " ++ show x)

	{- receive Finished -}
	pktFin <- recvPacket istate handle
	case pktFin of
		Right (Handshake (Finished _)) -> return ()
		x                              -> error ("unexpected reply. expecting finished " ++ show x)

	return ()

sendDataChunk :: IState -> Handle -> B.ByteString -> IO ()
sendDataChunk istate handle d =
	if B.length d > 16384
		then do
			let (sending, remain) = B.splitAt 16384 d
			sendPacket istate handle $ AppData sending
			sendDataChunk istate handle remain
		else
			sendPacket istate handle $ AppData d

{- | sendData sends a bunch of data -}
sendData :: IState -> Handle -> L.ByteString -> IO ()
sendData istate handle d = mapM_ (sendDataChunk istate handle) (L.toChunks d)

{- | recvData get data out of Data packet, and automatically try to renegociate if
 - a Handshake HelloRequest is received -}
recvData :: IState -> Handle -> IO L.ByteString
recvData istate handle = do
	pkt <- recvPacket istate handle
	case pkt of
		Right (AppData x) -> return $ L.fromChunks [x]
		Right (Handshake HelloRequest) -> do
			-- SECURITY FIXME audit the rng here..
			state <- readIORef istate
			let st = scTLSState state
			let (bytes, rng') = getRandomBytes (stRandomGen st) 32
			let (premaster, rng'') = getRandomBytes rng' 46
			writeIORef istate $ state { scTLSState = st { stRandomGen = rng'' } }
			let crand = fromJust $ clientRandom bytes
			connect istate handle crand (ClientKeyData $ B.pack premaster)
			recvData istate handle
		Left err          -> error ("error received: " ++ show err)
		_                 -> error "unexpected item"

{- | close a TLS connection.
 - note that it doesn't close the handle, but just signal we're going to close
 - the connection to the other side -}
close :: IState -> Handle -> IO ()
close istate handle = do
	sendPacket istate handle $ Alert (AlertLevel_Warning, CloseNotify)

clientEnumSimple
    :: Trans.MonadIO m
    => Handle
    -> (E.Iteratee B.ByteString m () -> E.Enumerator B.ByteString m a -> m b)
    -> m b
clientEnumSimple h f = do
	ranByte <- Trans.liftIO $ B.head <$> AESRand.randBytes 1
	_ <- Trans.liftIO $ AESRand.randBytes (fromIntegral ranByte)
	clientRandom' <- Trans.liftIO $ fromJust . clientRandom . B.unpack <$> AESRand.randBytes 32
	premasterRandom <- Trans.liftIO $ ClientKeyData <$> AESRand.randBytes 46
	seqInit <- Trans.liftIO $ conv . B.unpack <$> AESRand.randBytes 4
	let clientstate = TLSClientParams
		{ cpConnectVersion = TLS10
		, cpAllowedVersions = [ TLS10, TLS11 ]
		, cpSession = Nothing
		, cpCiphers = ciphers
		, cpCertificate = Nothing
		, cpCallbacks = TLSClientCallbacks
			{ cbCertificates = Nothing
			}
		}
	clientEnum clientstate (makeSRandomGen seqInit) h clientRandom' premasterRandom f
  where
    ciphers =
        [ cipher_AES128_SHA1
        , cipher_AES256_SHA1
        , cipher_RC4_128_MD5
        , cipher_RC4_128_SHA1
        ]
    conv l = (a `shiftL` 24) .|. (b `shiftL` 16) .|. (c `shiftL` 8) .|. d
        where
            [a,b,c,d] = map fromIntegral l

clientEnum :: Trans.MonadIO m
           => TLSClientParams -> SRandomGen -> Handle -> ClientRandom -> ClientKeyData
           -> (E.Iteratee B.ByteString m () -> E.Enumerator B.ByteString m a -> m b)
           -> m b
clientEnum tcp srg h cr ckd f = do
    istate <- Trans.liftIO $ newIState tcp srg
    Trans.liftIO $ connect istate h cr ckd
    b <- f (iter istate) (enum istate)
    Trans.liftIO $ close istate h
    return b
  where
    iter :: Trans.MonadIO m => IState -> E.Iteratee B.ByteString m ()
    iter istate =
        E.continue go
      where
        go E.EOF = return ()
        go (E.Chunks xs) = do
            Trans.liftIO $ sendData istate h $ L.fromChunks xs
            E.continue go
    enum :: Trans.MonadIO m => IState -> E.Enumerator B.ByteString m a
    enum istate (E.Continue k) = do
		lbs <- Trans.liftIO $ recvData istate h
		let chunks = E.Chunks $ L.toChunks lbs
		step <- Trans.lift $ E.runIteratee $ k chunks
		enum istate step
    enum _ step = E.returnI step
