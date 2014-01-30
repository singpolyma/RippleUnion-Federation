{-# LANGUAGE CPP #-}
module Application (getAlias) where

import Prelude ()
import BasicPrelude
import Control.Error (throwT, eitherT)

import Network.Wai (Application, queryString)
import Network.HTTP.Types (ok200, badRequest400, notFound404)
import Network.Wai.Util (stringHeaders, json, queryLookup)

import Network.URI (URI(..))

import Database.SQLite.Simple (query, Connection, setTrace)

import Records
import MustacheTemplates
#include "PathHelpers.hs"

Just [cors] = stringHeaders [("Access-Control-Allow-Origi", "*")]

-- TODO: NoSuchDomain

getAlias :: URI -> Connection -> Application
getAlias _ db req = eitherT err return $ do
	liftIO $ setTrace db (Just print)
	q <- (,) <$> fromQ "domain" <*> fromQ "destination"
	alias <- fmap listToMaybe $ liftIO $ query db (s"SELECT alias,domain,ripple,dt FROM aliases WHERE domain=? AND alias=? LIMIT 1") q
	case alias of
		Just a -> json ok200 [cors] (a :: Alias) -- TODO: CORS
		Nothing -> throwT $ Error NoSuchUser "No such alias on that domain."
	where
	err e@(Error NoSuchUser _) = json notFound404 [cors] e
	err e = json badRequest400 [cors] e
	fromQ k =
		maybe (throwT $ Error InvalidParams ("No " ++ k ++ " provided.")) return $
			queryLookup k (queryString req)
