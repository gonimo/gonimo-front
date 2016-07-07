-- | Html view elements which are generically usefull:
module Gonimo.UI.Html where

import Prelude
import Pux.Html.Attributes as A
import Pux.Html (text, span, Html, img, div)

viewLogo :: forall action. Html action -> Html action
viewLogo inner =
  div [ A.id_ "site" ]
    [ div [ A.id_ "main"]
        [ img [ A.id_ "logo", A.src "pix/gonimo-21.jpg", A.alt "Gonimo Logo"] []
        , inner
        ]
    ]

viewLoading :: forall action. String -> Html action
viewLoading inner = viewLogo $ span [] [text inner]
