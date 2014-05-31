module Records where

import Prelude ()
import BasicPrelude

import Text.Blaze.Internal (MarkupM)
import Network.URI (URI, parseAbsoluteURI)
import Data.Base58Address (RippleAddress)

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Text.Buildable
import Database.SQLite.Simple (SQLData(SQLText,SQLNull))
import Database.SQLite.Simple.FromRow (FromRow(..), field, fieldWith)
import Database.SQLite.Simple.ToRow (ToRow(..))
import Database.SQLite.Simple.ToField (ToField(..), toField)
import Database.SQLite.Simple.FromField (fieldData, ResultError(ConversionFailed))
import Database.SQLite.Simple.Ok (Ok(Ok, Errors))
import Text.Blaze.Html.Renderer.Text (renderHtmlBuilder)

s :: (IsString s) => String -> s
s = fromString

instance Buildable (MarkupM a) where
	build = renderHtmlBuilder . fmap (const ())

instance Buildable URI where
	build = build . show

data Alias = Alias {
		alias :: Text,
		domain :: Text,
		ripple :: RippleAddress,
		dt :: Maybe Word32
	}

instance ToRow Alias where
	toRow (Alias alias domain ripple dt) =
		[toField alias, toField domain, toField (show ripple), toField dt]

instance FromRow Alias where
	fromRow = Alias <$> field <*> field <*> fieldWith rippleF <*> fmap (fmap fromi64) field
		where
		fromi64 = fromIntegral :: Int64 -> Word32

		rippleF f = case fieldData f of
			(SQLText t) -> case readMay t of
				Nothing -> Errors [toException $ ConversionFailed "TEXT" "RippleAddress" "invalid"]
				Just ripple -> Ok ripple
			_ -> Errors [toException $ ConversionFailed "TEXT" "RippleAddress" "need a text"]

instance ToJSON Alias where
	toJSON (Alias alias domain ripple dt) = object [
		s"federation_json" .= (object $ [
				s"type" .= "federation_record",
				s"destination" .= alias,
				s"domain" .= domain,
				s"destination_address" .= show ripple
			] ++ maybe [] (\x -> [s"dt" .= x]) dt)
		]

instance ToJSON Error where
	toJSON (Error typ message) = object [
			s"result" .= "error",
			s"error" .= typ,
			s"error_message" .= message
		]

instance ToJSON ErrorType where
	toJSON NoSuchUser = toJSON "noSuchUser"
	toJSON NoSupported = toJSON "noSupported"
	toJSON NoSuchDomain = toJSON "noSuchDomain"
	toJSON InvalidParams = toJSON "invalidParams"
	toJSON Unavailable = toJSON "unavailable"

data Domain = Domain {
		name :: Text,
		pattern :: Maybe Text,
		proxy :: Maybe URI
	}

instance FromRow Domain where
	fromRow = Domain <$> field <*> field <*> fieldWith fromUri
		where
		fromUri f = case fieldData f of
			(SQLText t) -> case parseAbsoluteURI (textToString t) of
				Nothing -> Errors [toException $ ConversionFailed "TEXT" "URI" "invalid"]
				Just uri -> Ok (Just uri)
			SQLNull -> Ok Nothing
			_ -> Errors [toException $ ConversionFailed "TEXT" "URI" "need a text"]

data Home = Home {
	}

data ErrorType = NoSuchUser | NoSupported | NoSuchDomain | InvalidParams | Unavailable

data Error = Error {
		errorType :: ErrorType,
		errorMessage :: String
	}

data Header = Header {
	}

instance Monoid Header where
	mempty = Header
	mappend _ _ = Header

instance Eq Header where
	_ == _ = False

header :: Header
header = Header
