-- | Html view elements which are generically usefull:
module Gonimo.Client.Html where

import Pux.Html (Html, img, div)
import Pux.Html.Attributes as A

viewLogo :: forall action. Html action -> Html action
viewLogo inner =
  div [ A.id_ "site" ]
    [ div [ A.id_ "main"]
        [ img [ A.id_ "logo", A.src "pix/gonimo-21.jpg", A.alt "Gonimo Logo"] []
        , inner
        ]
    ]
