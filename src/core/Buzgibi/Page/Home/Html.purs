module Buzgibi.Page.Home.Html (html) where

import Prelude

import Buzgibi.Data.Route as Route
import Buzgibi.Component.HTML.Utils (css, safeHref)
import Buzgibi.Page.Home.Anchors as Anchors

import Halogen.HTML.Properties.Extended as HPExt
import Halogen.Html.Raw.Render as H
import Data.Maybe
import Halogen as H
import Halogen.HTML as HH
import Data.Map as Map
import Undefined

type Card = { card :: String, img :: String, pic :: String, src :: String, h4 :: String, p :: String }

cards =
  [ { card: "card1"
    , img: "boximg1-container"
    , pic: "picture1"
    , src: "images/timemanage.png"
    , h4: "Time management"
    , p: "It talks to customers with smart calls that are under your control. It's up to you to enjoy your time"
    }
  , { card: "card2"
    , img: "img2-container"
    , pic: "picture2"
    , src: "images/controlonyou.png"
    , h4: "You have the control"
    , p: "Buzgibi creates a method for you to voice over and make calls. Starting a customer survey is completely under your control."
    }
  , { card: "card3"
    , img: "img3-container"
    , pic: "picture3"
    , src: "images/understandemotions.png"
    , h4: "AI understands the conversations"
    , p: "Buzgibi understands the context and parses it. It only takes a while to realise who is a genuine buyer of you product"
    }
  , { card: "card4"
    , img: "4img-container"
    , pic: "picture4"
    , src: "images/fastresponse.svg"
    , h4: "Quick response"
    , p: "Buzgibi asks a question, gets an answer and that’s all. This happens so fast and it's amazing. Join Buzgibi"
    }
  , { card: "card5"
    , img: "img5-container"
    , pic: "picture5"
    , src: "images/notspam.svg"
    , h4: "Not a spam, just like a human"
    , p: "Our smart system is designed to distinguish on spam calls. No spam will ever reach your customer"
    }
  , { card: "card6"
    , img: "img5-img6-container"
    , pic: "picture6"
    , src: "images/ads.png"
    , h4: "Ads are now more optimized"
    , p: "Ads is more targeted now. Buzgibi approach to advs strikes a cord with your customers"
    }
  ]

html =
  HH.div_
    [ HH.header_
        [ HH.nav [ css "navbar" ]
            [ HH.div [ css "container" ]
                [ HH.div [ css "logo" ] [ HH.img [ HPExt.src "images/logo-gradient.png" ] ]
                , HH.div_ [ HH.slot_ Anchors.proxy unit Anchors.component unit ]
                , HH.div [ css "nav-auth" ]
                    [ HH.a [ safeHref Route.SignIn, css "nav-link-signin" ] [ HH.text "Sign In" ]
                    , HH.a [ safeHref Route.SignUp, css "nav-link-signin" ] [ HH.text "Sign Up" ]
                    ]
                ]
            ]
        ]
    , HH.section [ css "hero-container" ]
        [ HH.div [ HPExt.id (Anchors.mkPrintAnchor Anchors.Home) ] []
        , HH.div [ css "heroheader" ]
            [ HH.h1_
                [ HH.span [ css "text-gradient", HPExt.style "color: linear-gradient(102deg, #5D3DF8 0%, #DF3ECB 100%)" ]
                    [ HH.text "Conduct surveys" ]
                , HH.br_
                , HH.text "without hassle"
                , HH.br_
                , HH.span [ css "text-gradient" ] [ HH.text "Fast and Easy" ]
                ]
            ]
        ]
    , HH.section [ css "herocontent" ]
        [ HH.b_ [ HH.text "Make a one-touch call, you don't even need to talk!|" ]
        , HH.br_
        , HH.text "All what you need is to reach out to your customer is to write a question."
        , HH.br_
        , HH.text "Yes, Buzgibi is like a magic.."
        ]
    , HH.div [ css "CTA-container" ]
        [ HH.div [ css "cta-button" ]
            [ HH.a [ safeHref Route.SignUp, css "cta-button" ] [ HH.text "let's begin" ]
            ]
        ]
    , HH.div [ HPExt.id (Anchors.mkPrintAnchor Anchors.Advanteges) ] []
    , HH.div [ css "advantages-container" ]
        [ HH.div [ css "titleCapital" ] [ HH.h4_ [ HH.text "ADVANTAGES OF BUZGİBİ" ] ]
        , HH.div [ css "titleOtomatic" ] [ HH.text "Making calls is not a routine now" ]
        , HH.div [ css "cards-container" ] $
            cards <#> \{ card, img, pic, src, h4, p } ->
              HH.div [ css card ]
                [ HH.div [ css img ]
                    [ HH.div [ css pic ] [ HH.img [ HPExt.src src ] ] ]
                , HH.h4_ [ HH.text h4 ]
                , HH.p_ [ HH.text p ]
                ]
        ]
    , HH.div [ HPExt.id (Anchors.mkPrintAnchor Anchors.HowItWorks) ] []
    , HH.section [ css "hiw" ]
        [ HH.div [ css "howitworks-container" ]
            [ HH.div [ css "mockup" ] [ HH.img [ HPExt.src "images/mockupen.png" ] ]
            , HH.div [ css "howitworkscontent" ]
                [ HH.div [ css "titleGray" ] [ HH.h4_ [ HH.text "HOW IT WORKS?" ] ]
                , HH.div [ css "signupcontent" ]
                    [ HH.div [ css "boldtitle" ] [ HH.h5_ [ HH.text "Sign Up!" ] ]
                    , HH.p_ [ HH.text "Buzgibi is an application that allows you to manage your calls. You can start a search by typing with ice cold. But first, you need to create an account for this.!" ]
                    ]
                ]
            ]
        ]
    , HH.div [ css "achievements-container" ]
        [ HH.div [ css "box-achievement" ]
            [ HH.div [ css "box1" ]
                [ HH.img [ HPExt.src "images/people.png" ]
                , HH.h5_ [ HH.text "100,000+" ]
                , HH.p_ [ HH.text "Cold Calls Conducted" ]
                ]
            , HH.div [ css "box2" ]
                [ HH.img [ HPExt.src "images/botwithq.png" ]
                , HH.h5_ [ HH.text "5M+" ]
                , HH.p_ [ HH.text "Generated realistic voice" ]
                ]
            , HH.div [ css "box3" ]
                [ HH.img [ HPExt.src "images/globe.png" ]
                , HH.h5_ [ HH.text "100+" ]
                , HH.p_ [ HH.text "Countries you can call" ]
                ]
            ]
        ]
    , HH.section [ css "reach-container" ]
        [ HH.div [ css "reachcontent" ]
            [ HH.div [ css "threewords" ] [ HH.h4_ [ HH.text "Fast, Automated, Analysis-based" ] ]
            , HH.div [ css "heroheader" ] [ HH.h1_ [ HH.span [ css "text-gradient" ] [ HH.text "Reach your customers with Buzgibi!" ] ] ]
            , HH.div [ HPExt.id "CTAabovefooter", css "CTA-container" ]
                [ HH.div [ css "cta-button" ]
                    [ HH.a [ safeHref Route.SignUp, css "cta-button" ] [ HH.text "let's begin" ]
                    ]
                ]
            ]
        , HH.div [ css "mascot" ] [ HH.img [ HPExt.src "images/cta_area.png" ] ]
        ]
    ]