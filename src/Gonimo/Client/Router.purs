module Gonimo.Client.Router where

import Prelude
import Data.Argonaut.Generic.Aeson as Aeson
import Control.Alt ((<|>))
import Control.Apply ((<*), (*>))
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(Right, Left))
import Data.Functor ((<$))
import Data.Maybe (fromMaybe)
import Global (decodeURIComponent)
import Gonimo.Types (Secret(Secret))
import Partial.Unsafe (unsafeCrashWith)
import Prelude (($), (<$>))
import Pux.Router (param, router, lit, int, end)
import Servant.PureScript.Settings (gDefaultEncodeURLPiece)
import Debug.Trace (trace)
import Data.Maybe (Maybe(..))
import Data.Array (index)
import Data.Either (fromRight, either)
import Partial.Unsafe (unsafePartial)

import Data.String.Regex as R


data Route = Home | AcceptInvitation Secret

render (AcceptInvitation secret) = "acceptInvitation=" <> gDefaultEncodeURLPiece secret
render Home = ""

-- Use simple plain old regex for matching route - pux routing does not work reliably!
match :: String -> Route
-- match' url =trace ("Routing: " <> url) $ \_ -> (fromMaybe (DecodingFailedP $ "No such route: " <> url) $ router url $
--        AcceptInvitationP <$> (lit "index.html" *> param "acceptInvitation") <* end
--    <|> HomeP <$ lit "index.html" <* end
-- )
match url = let
    matcher = unsafePartial $ fromRight $ R.regex ".*acceptInvitation=([^&]*)" R.noFlags
    matched = R.match matcher url
    decode :: String -> Either String Secret
    decode = Aeson.decodeJson <=< (jsonParser <<< decodeURIComponent)
    decodeJust :: String -> Maybe Secret
    decodeJust = either (const Nothing) Just <<< decode
  in
     fromMaybe Home $ do
       arr <- matched
       element <- index arr 1
       decoded <- decodeJust =<< element
       pure $ AcceptInvitation decoded

