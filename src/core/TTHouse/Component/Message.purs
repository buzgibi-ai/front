-- https://codepen.io/fclaw/pen/dyQGMPN
module TTHouse.Component.Message (component, proxy) where

import Prelude

import TTHouse.Component.HTML.Utils (css, whenElem)
import Halogen.HTML.Properties.Extended as HPExt
import TTHouse.Api.Foreign.Scaffold as Scaffold 
import TTHouse.Capability.LogMessages (logError, logDebug)
import TTHouse.Api.Foreign.Request as Request
import TTHouse.Component.Async as Async
import TTHouse.Api.Foreign.Request.Handler (onFailure, withError) 
import TTHouse.Component.Utils ( withCaptcha )
import TTHouse.Component.Utils (initTranslation)
import TTHouse.Component.Subscription.Translation as Translation
import TTHouse.Data.Config

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))
import Data.Maybe
import Web.Event.Event (preventDefault, Event)
import Effect.Aff (try)
import Data.Either
import Data.Function.Uncurried (runFn1, runFn2)
import Data.Array.NonEmpty (singleton)
import Data.Argonaut.Encode
import Data.Argonaut.Core (stringify)
import Halogen.Store.Monad (getStore)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Data.Array.NonEmpty hiding (length)
import Data.Validation.Semigroup
import Data.String
import Data.String.Pattern
import Effect.Console (logShow)
import Effect.AVar as Async
import Effect.Exception as Excep
import Data.Map as Map

import Undefined

proxy = Proxy :: _ "message"

loc = "TTHouse.Component.Message"

data Action = 
       Initialize
     | MakeRequest Event 
     | FillName String 
     | FillEmail String 
     | FillEnquiry String
     | RollBack
     | LangChange String (Map.Map String String)

type State = 
    { name :: Maybe String
    , email :: Maybe String
    , enquiry :: Maybe String
    , error :: Maybe (NonEmptyArray String) 
    , serverError :: Maybe String
    , isSent :: Boolean
    , isClick :: Boolean
    , texts :: Map.Map String String
    , hash :: String
    }

type RequestBody = { email :: String, name :: String, enquiry :: String }

defState =
   { name: Nothing
   , email: Nothing
   , enquiry: Nothing
   , error: Nothing
   , isSent: false
   , isClick: false
   , serverError: Nothing
   , texts: Map.empty
   , hash: mempty }

component =
  H.mkComponent
    { initialState: const defState
    , render: render
    , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = pure Initialize }
    }
    where 
      handleAction Initialize = do 
        void $ initTranslation loc \hash translation -> 
          H.modify_ _ { 
              hash = hash
            , texts = Scaffold.getTranslationMessenger translation }
        Translation.load loc $ \hash translation -> 
          handleAction $ LangChange hash $ Scaffold.getTranslationMessenger translation
        {texts, hash} <- H.get
        logDebug $ loc <> " ---> " <> show texts
        logDebug $ loc <> " hash: ---> " <> hash  
      handleAction (LangChange hash xs) = do 
        logDebug $ loc <> " ---> " <> show xs
        logDebug $ loc <> " hash: ---> " <> hash
        H.modify_ _ { hash = hash, texts = xs }
      handleAction (MakeRequest ev) = do 
        H.liftEffect $ preventDefault ev
        { config: Config {scaffoldHost: host, isCaptcha}, async } <- getStore
        let ok = do
              H.modify_ _ { 
                  isSent = true
                , email = Nothing
                , name = Nothing
                , enquiry = Nothing
                , error = Nothing
                , isClick = false
                , serverError = Nothing }
              Async.send $ Async.mkOrdinary "Thank you for submitting the enquiry" Async.Success Nothing
              handleAction RollBack    
            failure e = do
              H.modify_ _ { 
                  isSent = true
                , serverError = pure (show e)
                , email = Nothing
                , name = Nothing
                , enquiry = Nothing
                , isClick = false }
              logError $ show e
        withCaptcha isCaptcha
          (do Async.send $ Async.mkOrdinary "captcha verification failure" Async.Warning Nothing
              H.modify_ _ {
                  isSent = true
                , serverError = Nothing
                , email = Nothing
                , name = Nothing
                , enquiry = Nothing
                , isClick = false }
              handleAction RollBack) 
          (do state@{name, email, enquiry} <- H.get
              let res = toEither $ validate name email enquiry
              case res of 
                Right { email, name, enquiry } -> do
                  req <- H.liftEffect $ 
                          runFn1 Scaffold.mkSendGridSendMailRequest
                          { from: email
                          , personalization: name
                          , subject: "enquiry"
                          , body: enquiry }
                  -- logDebug $ show state       
                  resp <- Request.make host Scaffold.mkForeignApi $ runFn2 Scaffold.send req
                  onFailure resp failure (const ok) 
                Left xs -> H.modify_ _ { isSent = false, error = pure $ xs, isClick = true })
      handleAction (FillName v) = 
        if length v > 0 then 
           H.modify_ \s -> s { name = Just v, error = join $ map (fromArray <<< delete "name") (_.error s) }
        else H.modify_ _ { name = Nothing }
      handleAction (FillEmail v) =
        if length v > 0 then 
           H.modify_ \s -> s { email = Just v, error = join $ map (fromArray <<< delete "email") (_.error s) }
        else H.modify_ _ { email = Nothing }
      handleAction (FillEnquiry v) =
        if length v > 0 then 
           H.modify_ \s -> s { enquiry = Just v, error = join $ map (fromArray <<< delete "enquiry") (_.error s) }
        else H.modify_ _ { enquiry = Nothing }
      handleAction RollBack = do
        liftAff $ delay $ Milliseconds 3000.0
        H.modify_ _ { isSent = false }

validate nameM emailM enquiryM =
  (\n e enq -> { email: e, name: n, enquiry: enq })
  <$> (maybe (invalid (singleton "name")) pure nameM)
  <*> (maybe (invalid (singleton "email")) pure emailM)
  <*> (maybe (invalid (singleton "enquiry")) pure enquiryM)

-- https://codepen.io/fclaw/pen/BaGyKpB

imgUrl = "https://lh3.googleusercontent.com/-LvTWzTOL4c0/V2yhfueroyI/AAAAAAAAGZM/Ebwt4EO4YlIc03tw8wVsGrgoOFGgAsu4wCEw/w140-h140-p/43bf8578-86b8-4c1c-86a6-a556af8fba13"

render {error, isSent, isClick, texts } =
  HH.div [css "container"] 
  [
      HH.div [css "row"]
      [
          HH.div [css "nb-form"]
          [
            whenElem (not isSent) $
              HH.div_ 
              [
                  HH.p [css "title"] [HH.text (tranalate "headline" texts)]
              ,   HH.img [HPExt.src imgUrl, css "user-icon"]
              ,   HH.div [HPExt.style "padding-top:20px"] [form error isClick texts]
              ]
          ] 
      ]
  ]

form xs isClick texts = 
  HH.form [ HE.onSubmit MakeRequest, css "needs-validation"]
  [ 
      HH.div [css "form-group"]
      [
          HH.input  
          [ HPExt.placeholder (tranalate "identity" texts)
          , HPExt.type_ HPExt.InputText
          , HE.onValueInput FillName
          , addClass isClick "name" xs
          ]
      ]    
  ,   HH.div [css "form-group"] 
      [   
          HH.input 
          [ HPExt.placeholder (tranalate "email" texts)
          , HPExt.type_ HPExt.InputEmail
          , HE.onValueInput FillEmail
          , addClass isClick "email" xs
          ]
      ]
  ,   HH.div [css "form-group"] 
      [ 
          HH.textarea
          [ HPExt.placeholder (tranalate "body" texts)
          , HE.onValueInput FillEnquiry
          , HPExt.rows 10
          , HPExt.style "resize:none"
          , addClass isClick "enquiry" xs
          ]
      ]
  ,   HH.div [css "form-group"] 
      [   
          HH.input 
          [ HPExt.type_ HPExt.InputSubmit
          , HPExt.value (tranalate "button" texts)
          , css "btn form-control"
          ]
      ]
  ]

searchForError el xs = join $ map (find (contains (Pattern el))) xs

addClass true field xs =
  case searchForError field xs of
    Just e -> css "form-control is-invalid"
    Nothing -> css "form-control"
addClass _ _ _ = css "form-control"

tranalate key = fromMaybe "item cannot be translated" <<< Map.lookup key
