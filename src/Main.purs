module Main where

import Prelude
import Gonimo.Client.Types as Client
import Gonimo.LocalStorage as Key
import Pux.Html.Attributes as A
import Browser.LocalStorage (STORAGE, localStorage)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..))
import Gonimo.Client.Types (Settings)
import Gonimo.WebAPI (postAccounts)
import Gonimo.WebAPI.Types (AuthData(AuthData))
import Pux (renderToDOM, fromSimple, start)
import Pux.Html (text, span, Html, img, div)

data State = Loading
           | Loaded {
             authData :: AuthData
             , settings :: Settings
             }

data Action = LoadState

update :: Action -> State -> State
update LoadState = id

view :: State -> Html Action
view state =
  div [ A.id_ "site" ]
    [ div [ A.id_ "main"]
        [ img [ A.id_ "logo", A.src "pix/gonimo-21.jpg", A.alt "Gonimo Logo"] []
        , span [] [ text "Loading your gonimo, stay tight ..."]
        ]
    ]

getAuthData :: forall eff. Client.Effects(storage :: STORAGE | eff) AuthData
getAuthData = do
  md <- liftEff $ localStorage.getItem Key.authData
  case md of
    Nothing -> postAccounts
    Just d  -> pure d

main = do
  app <- start $
    { initialState: Loading
    , update: fromSimple update
    , view: view
    , inputs: []
    }

  renderToDOM "#app" app.html
  
