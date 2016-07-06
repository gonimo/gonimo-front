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

data Route = Home | AcceptInvitation Secret | DecodingFailed String

data RouteP = HomeP | AcceptInvitationP String | DecodingFailedP String

render (AcceptInvitation secret) = "acceptInvitation=" <> gDefaultEncodeURLPiece secret
render Home = ""

match' :: String -> RouteP
match' url = fromMaybe (DecodingFailedP $ "No such route: " <> url) $ router url $
       AcceptInvitationP <$> (lit "index.html" *> param "acceptInvitation") <* end
   <|> HomeP <$ lit "index.html" <* end


match :: String -> Route
match url = let r' = match' url
            in case r' of
              HomeP -> Home
              DecodingFailedP err -> DecodingFailed err
              AcceptInvitationP str ->
                let eVal = Aeson.decodeJson =<< (jsonParser <<< decodeURIComponent) str
                in case eVal of
                  Left err  -> DecodingFailed err
                  Right val -> AcceptInvitation val
