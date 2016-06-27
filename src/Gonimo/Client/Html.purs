-- | Html view elements which are generically usefull:
module Gonimo.Client.Html where

import Pux.Html (text, span, Html, img, div)

viewLogo :: forall action. Html action -> Html action
viewLogo state inner =
  div [ A.id_ "site" ]
    [ div [ A.id_ "main"]
        [ img [ A.id_ "logo", A.src "pix/gonimo-21.jpg", A.alt "Gonimo Logo"] []
        , inner
        ]
    ]
