{-# LANGUAGE CPP #-}
module Application (getAlias) where

import Prelude ()
import BasicPrelude
import Control.Error (EitherT(..), eitherT, throwT, noteT, hoistMaybe, fmapLT)
import qualified Data.Text as T

import Network.Wai (Application, queryString)
import Network.HTTP.Types (ok200, badRequest400, notFound404)
import Network.Wai.Util (stringHeaders, json, queryLookup)

import Network.URI (URI(..), parseAbsoluteURI)

import Database.SQLite.Simple (query, Connection, setTrace)

import Scrape
import Records
import MustacheTemplates
#include "PathHelpers.hs"

Just [cors] = stringHeaders [("Access-Control-Allow-Origi", "*")]

listToEitherT :: (Monad m) => e -> [a] -> EitherT e m a
listToEitherT e = noteT' e . listToMaybe

noteT' :: (Monad m) => e -> Maybe a -> EitherT e m a
noteT' e = noteT e . hoistMaybe

getAlias :: URI -> Connection -> Application
getAlias _ db req = eitherT err return $ do
	liftIO $ setTrace db (Just print)
	q <- (,) <$> fromQ "domain" <*> fromQ "destination"
	domain <- listToEitherT nodomain =<< query' "SELECT domain,pattern FROM domains WHERE domain LIKE ? LIMIT 1" [fst q]
	alias <- query' "SELECT alias,domain,ripple,dt FROM aliases WHERE domain LIKE ? AND alias LIKE ? LIMIT 1" q
	a <- case alias of
		(a:_) -> return a
		[] -> case pattern domain of
			Just pat -> do
				uri <- noteT' noalias $ parseAbsoluteURI
					(textToString $ T.replace (s"%s") (snd q) pat)
				result <- fmapLT (const noalias) $ EitherT $
					liftIO $ scrapeRippleAddress uri
				address <- noteT' noalias $ readMay $ decodeUtf8 result
				return Alias {
					alias = snd q,
					domain = fst q,
					ripple = address,
					dt = Nothing
				}
			Nothing -> throwT noalias
	json ok200 [cors] (a :: Alias)
	where
	query' sql = liftIO . query db (s sql)
	nodomain = Error NoSuchDomain "That domain is not served here."
	noalias = Error NoSuchUser "No such alias on that domain."
	err e@(Error NoSuchUser _) = json notFound404 [cors] e
	err e = json badRequest400 [cors] e
	fromQ k = noteT' (Error InvalidParams ("No " ++ k ++ " provided.")) $
		queryLookup k (queryString req)
