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
import Data.Either (fromRight)
import Partial.Unsafe (unsafePartial)

import Data.String.Regex as R


data Route = Home | AcceptInvitation Secret | DecodingFailed String

data RouteP = HomeP | AcceptInvitationP String | DecodingFailedP String

render (AcceptInvitation secret) = "acceptInvitation=" <> gDefaultEncodeURLPiece secret
render Home = ""

match' :: String -> RouteP
-- match' url =trace ("Routing: " <> url) $ \_ -> (fromMaybe (DecodingFailedP $ "No such route: " <> url) $ router url $
--        AcceptInvitationP <$> (lit "index.html" *> param "acceptInvitation") <* end
--    <|> HomeP <$ lit "index.html" <* end
-- )
match' url = let
    matcher = unsafePartial $ fromRight $ R.regex ".*acceptInvitation=([^&]*)" R.noFlags
    matched = R.match matcher url
  in
     case matched of
       Nothing -> HomeP
       Just arr -> case index arr 1 of
         Nothing -> HomeP
         (Just Nothing) -> HomeP
         (Just (Just val)) -> AcceptInvitationP val

match :: String -> Route
match url = let r' = match' url
            in case r' of
              HomeP -> trace ("Routing: " <> url <> ", to Home") $ \_ -> Home
              DecodingFailedP err -> trace ("Routing:" <> url <> ", to DecodingFailed") $ \_ -> DecodingFailed err
              AcceptInvitationP str -> trace ("Routing:" <> url <> ", to AcceptInvitationP, match: " <> str) $ \_ ->
                let eVal = Aeson.decodeJson =<< (jsonParser <<< decodeURIComponent) str
                in case eVal of
                  Left err  -> DecodingFailed err
                  Right val -> AcceptInvitation val
