module Main (main) where

import Prelude hiding (FilePath)
import System.Environment (getArgs)
import Network.URI (parseAbsoluteURI, URI(..), URIAuth(..))
import System.IO (hPutStrLn, stderr)
import Filesystem.Path.CurrentOS (FilePath)
import Filesystem (getWorkingDirectory)
import Database.SQLite.Simple (open)
import OpenSSL (withOpenSSL)

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Autohead (autohead)
import Network.Wai.Middleware.Jsonp (jsonp)
import Network.Wai.Middleware.AcceptOverride (acceptOverride)
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)

import Network.Wai.Dispatch
import Records
import Routes

addTrailingSlash :: URI -> URI
addTrailingSlash u@(URI {uriPath = []}) = u {uriPath = "/"}
addTrailingSlash u@(URI {uriPath = p})
	| last p == '/' = u
	| otherwise = u {uriPath = p ++ "/"}

staticRoot :: FilePath -> Application
staticRoot = staticApp . defaultWebAppSettings

app :: URI -> Int -> String -> Maybe String -> IO ()
app root port dbpth portOverride = do
	cwd <- getWorkingDirectory
	db <- open dbpth
	run realPort $
		logStdoutDev $ autohead $ acceptOverride $ jsonp $ -- Middleware
		dispatch (staticRoot cwd) $ routes root db         -- Do routing
	where
	realPort = maybe port read portOverride

main :: IO ()
main = withOpenSSL $ do
	args <- getArgs
	case args of
		[dbp, root, port] ->
			main' (fmap addTrailingSlash $ parseAbsoluteURI root) dbp (Just port)
		[dbp, root] ->
			main' (fmap addTrailingSlash $ parseAbsoluteURI root) dbp Nothing
		[dbp] ->
			main' (parseAbsoluteURI "http://localhost:3000/") dbp Nothing
		_ -> hPutStrLn stderr "Usage: ./Main <db path> <Root URI> [<port>]"
	where
	main' (Just r@(URI {uriAuthority = Just (URIAuth {uriPort = ':':port})})) =
		app r (read port)
	main' (Just r@(URI {uriAuthority = Just (URIAuth {uriPort = ""})})) =
		app r 80
	main' _ = const $ const $
		hPutStrLn stderr "Invalid Root URI given"
