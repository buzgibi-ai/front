-- https://github.com/sendgrid/sendgrid-oai
module Buzgibi.Api.Foreign.BuzgibiBack
  ( ForeignApi
  , ReCaptcha
  , ReCaptchaApi
  , ResponseReCaptcha
  , SendGridSendMailRequest
  , SendGridSendMailRequestBody
  , getSuccessReCaptcha
  , goReCaptcha
  , mkForeignApi
  , mkReCaptchaApi
  , mkSendGridSendMailRequest
  , module Common
  , module FrontApi
  , module AuthApi
  , module UserApi
  , module FileApi
  , send
  ) where

import Prelude

import Buzgibi.Api.Foreign.Common as Common
import Buzgibi.Api.Foreign.Common
import Buzgibi.Api.Foreign.Front.Api as FrontApi
import Buzgibi.Api.Foreign.Auth.Api as AuthApi
import Buzgibi.Api.Foreign.User.Api as UserApi
import Buzgibi.Api.Foreign.File.Api as FileApi

import Data.Function.Uncurried (Fn1, Fn2, Fn3, runFn3)
import Effect (Effect)
import Effect.Aff.Compat as AC
import Foreign.Object (Object)
import Foreign (Foreign)
import Data.Either (Either)
import Effect.Exception as E

foreign import data ForeignApi :: Type
foreign import data SendGridSendMailRequest :: Type
foreign import data ReCaptchaApi :: Type
foreign import data ResponseReCaptcha :: Type
foreign import data ReCaptcha :: Type

foreign import mkForeignApi :: Fn1 ApiClient (Effect ForeignApi)

type SendGridSendMailRequestBody = { from :: String, personalization :: String, subject :: String, body :: String }

foreign import mkSendGridSendMailRequest :: Fn1 SendGridSendMailRequestBody (Effect SendGridSendMailRequest)

foreign import send :: forall a. Fn2 SendGridSendMailRequest ForeignApi (AC.EffectFnAff (Object (Response a)))

foreign import mkReCaptchaApi :: Fn1 ApiClient (Effect ReCaptchaApi)

foreign import _goReCaptcha :: Fn3 (forall a. Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) String ReCaptchaApi (AC.EffectFnAff (Object (ResponseReCaptcha)))

goReCaptcha :: String -> ReCaptchaApi -> AC.EffectFnAff (Object (ResponseReCaptcha))
goReCaptcha = runFn3 _goReCaptcha Common.withError

foreign import getSuccessReCaptcha :: ReCaptcha -> Boolean

instance Show ReCaptcha where
  show x = "{ success: " <> show (getSuccessReCaptcha x) <> " }"
