module TTHouse.Api.Scaffold.FrontApi
  ( Cookie
  , FrontApi
  , FrontendLogRequest
  , Init
  , MapMenuText
  , MapMessengerText
  , MapPageText
  , Meta
  , MetaPage(..)
  , ResponseCookie
  , ResponseMeta
  , ResponseTranslation
  , Translation
  , getCookies
  , getCookiesInit
  , getIsCaptcha
  , getLogLevel
  , getMeta
  , getMetaDescription
  , getShaCSSCommit
  , getShaCommit
  , getToTelegram
  , getTranslationCopyright
  , getTranslationMenu
  , getTranslationMessenger
  , getTranslationPage
  , init
  , loadTranslation
  , mkFrontApi
  , mkLogReq
  , sendLog
  )
  where

import Prelude

import TTHouse.Api.Foreign.Common
import TTHouse.Component.Lang.Data (Lang)
import Store.Types (LogLevel, readLogLevel)

import Data.Function.Uncurried (Fn2, Fn1, Fn3, runFn2, runFn3)
import Effect.Aff.Compat as AC
import Foreign.Object (Object)
import Data.Argonaut.Encode (encodeJson, class EncodeJson)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode.Combinators
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Tuple (Tuple (..))
import Data.Maybe (Maybe (..), fromMaybe)
import Undefined
import Foreign (Foreign)
import Effect (Effect)
import Data.Map as Map
import Data.Either (Either)
import Effect.Exception as E

foreign import data MapMenuText :: Type
foreign import data MapPageText :: Type
foreign import data FrontApi :: Type
foreign import data Translation :: Type
foreign import data FrontendLogRequest :: Type
foreign import data Cookie :: Type
foreign import data ResponseCookie :: Type
foreign import data ResponseMeta :: Type
foreign import data Meta :: Type
foreign import data Init :: Type
foreign import data ResponseTranslation :: Type
foreign import data MapMessengerText :: Type

foreign import mkFrontApi :: Fn1 ApiClient (Effect FrontApi)

foreign import _init :: Fn2 (forall a . Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) FrontApi (AC.EffectFnAff (Object (Response Init)))

init :: FrontApi -> AC.EffectFnAff (Object (Response Init))
init = runFn2 _init withError 

instance Show Init where
  show = _showInit

foreign import _showInit :: Init -> String

foreign import getShaCommit :: Init -> String
foreign import getShaCSSCommit :: Init -> String
foreign import getCookiesInit :: Init -> Array String
foreign import _getIsCaptcha :: Maybe Boolean -> (Boolean -> Maybe Boolean) -> Init -> Maybe Boolean
foreign import _getToTelegram :: Maybe Boolean -> (Boolean -> Maybe Boolean) -> Init -> Maybe Boolean
foreign import _getLogLevel :: Init -> String

getLogLevel :: Init -> Maybe LogLevel
getLogLevel = readLogLevel <<< _getLogLevel

getIsCaptcha :: Init -> Maybe Boolean
getIsCaptcha = _getIsCaptcha Nothing Just

getToTelegram :: Init -> Maybe Boolean
getToTelegram = _getToTelegram Nothing Just

foreign import _loadTranslation :: Fn3 (forall a . Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) Json FrontApi (AC.EffectFnAff (Object ResponseTranslation))

foreign import mkLogReq :: Fn2 String Foreign (Effect FrontendLogRequest)

foreign import _sendLog :: Fn3 (forall a . Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) FrontendLogRequest FrontApi (AC.EffectFnAff (Object (Response Unit)))

sendLog :: FrontendLogRequest -> FrontApi -> AC.EffectFnAff (Object (Response Unit))
sendLog = runFn3 _sendLog withError

instance Show Cookie where 
  show = _showCookie

foreign import _showCookie :: Cookie -> String

foreign import _getCookies :: Fn2 (forall a . Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) FrontApi (AC.EffectFnAff (Object ResponseCookie))

getCookies :: FrontApi -> AC.EffectFnAff (Object ResponseCookie)
getCookies = runFn2 _getCookies withError

newtype MetaPage = MetaPage String

instance EncodeJson MetaPage where
  encodeJson (MetaPage page) = "page" := page ~> jsonEmptyObject

foreign import _getMeta :: Fn3 (forall a . Foreign -> (Foreign -> Either E.Error a) -> Either E.Error a) Json FrontApi (AC.EffectFnAff (Object ResponseMeta))

getMeta :: Maybe MetaPage -> FrontApi -> AC.EffectFnAff (Object ResponseMeta)
getMeta page = runFn3 _getMeta withError (encodeJson (fromMaybe undefined page))

foreign import getMetaDescription :: Meta -> String

loadTranslation :: Lang -> FrontApi -> (AC.EffectFnAff (Object ResponseTranslation))
loadTranslation lang = runFn3 _loadTranslation withError (encodeJson lang)

foreign import _showTranslation :: Translation -> String

instance Show Translation where
  show = _showTranslation

foreign import _getKeyText :: forall a . a -> String
foreign import _getValText :: forall a . a -> String

foreign import _getTranslationMenu :: Translation -> Array MapMenuText

getTranslationMenu :: Translation -> Map.Map String String
getTranslationMenu = Map.fromFoldable <<< map toTpl <<< _getTranslationMenu
  where toTpl x = Tuple (_getKeyText x) (_getValText x) 

foreign import _showMapMenuText :: MapMenuText -> String

instance Show MapMenuText where
  show = _showMapMenuText

foreign import _getTranslationPage :: Translation -> Array MapPageText

getTranslationPage :: Translation -> Map.Map String String
getTranslationPage = Map.fromFoldable <<< map toTpl <<< _getTranslationPage
  where toTpl x = Tuple (_getKeyText x) (_getValText x)

foreign import getTranslationCopyright :: Translation -> String

foreign import _getTranslationMessenger :: Translation -> Array MapMessengerText

getTranslationMessenger :: Translation -> Map.Map String String
getTranslationMessenger = Map.fromFoldable <<< map toTpl <<< _getTranslationMessenger
  where toTpl x = Tuple (_getKeyText x) (_getValText x)