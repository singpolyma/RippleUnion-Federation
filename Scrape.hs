module Scrape (scrapeRippleAddress) where

import Control.Applicative
import Control.Monad
import UnexceptionalIO (fromIO, runUnexceptionalIO)
import Control.Exception (fromException)
import Control.Error (EitherT, fmapLT, throwT, runEitherT)
import Network.URI (URI, uriPath)
import Network.Http.Client (withConnection, establishConnection, sendRequest, buildRequest, http, Response, receiveResponse, RequestBuilder, emptyBody, getStatusCode)
import qualified Network.Http.Client as HttpStreams
import Blaze.ByteString.Builder (Builder)
import System.IO.Streams (OutputStream, InputStream)
import System.IO.Streams.Attoparsec (parseFromStream, ParseException(..))
import Network.HTTP.Types.Status (Status)
import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS8 -- eww

scrapeRippleAddress :: URI -> IO (Either Error ByteString)
scrapeRippleAddress uri = get uri (return ()) extractRippleAddress

rippleAlphabet :: [Char]
rippleAlphabet = "rpshnaf39wBUDNEGHJKLM4PQRST7VWXYZ2bcdeCg65jkm8oFqi1tuvAxyz"

extractRippleAddress :: Parser ByteString
extractRippleAddress = do
	_ <- skipFront
	Data.Attoparsec.ByteString.Char8.skipWhile (/='r')
	_ <- Data.Attoparsec.ByteString.Char8.char 'r'
	c <- Data.Attoparsec.ByteString.Char8.peekChar
	case c of
		Just c' -> when (not (inClass rippleAlphabet c')) $ do
			Data.Attoparsec.ByteString.Char8.skipWhile (/='r')
			_ <- Data.Attoparsec.ByteString.Char8.char 'r'
			return ()
		_ -> return ()
	bs <- Data.Attoparsec.ByteString.Char8.takeWhile (inClass rippleAlphabet)
	return $ BS8.singleton 'r' `BS8.append` bs
	where
	skipFront = do
		Data.Attoparsec.ByteString.Char8.skipWhile (/='R')
		string (BS8.pack "Ripple Address:") <|>
			(anyChar >> skipFront)

get :: URI -> RequestBuilder () -> Parser a -> IO (Either Error a)
get uri req parser = oneShotHTTP HttpStreams.GET uri req emptyBody (responseHandler parser)

data Error = ParseError | RequestError Status | OtherError
	deriving (Show, Eq)

responseHandler :: Parser a -> Response -> InputStream ByteString -> IO (Either Error a)
responseHandler parser resp i = runUnexceptionalIO $ runEitherT $ do
	case getStatusCode resp of
		code | code >= 200 && code < 300 -> return ()
		code -> throwT $ RequestError $ toEnum code
	fmapLT (handle . fromException) $ fromIO $ parseFromStream parser i
	where
	handle (Just (ParseException _)) = ParseError
	handle _ = OtherError

oneShotHTTP :: HttpStreams.Method -> URI -> RequestBuilder () -> (OutputStream Builder -> IO ()) -> (Response -> InputStream ByteString -> IO b) -> IO b
oneShotHTTP method uri req body handler = do
	req' <- buildRequest $ do
		http method (BS8.pack $ uriPath uri)
		req
	withConnection (establishConnection url) $ \conn -> do
		sendRequest conn req' body
		receiveResponse conn handler
	where
	url = BS8.pack $ show uri -- URI can only have ASCII, so should be safe
