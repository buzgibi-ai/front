module Statistics (sendComponentTime) where

import Prelude

import TTHouse.Capability.LogMessages (logDebug)
import TTHouse.Api.Foreign.Request (makeWithResp)
import TTHouse.Api.Foreign.Scaffold as Scaffold
import TTHouse.Data.Config

import Effect
import Halogen as H
import Data.Argonaut.Encode.Combinators
import Data.Argonaut.Core (jsonEmptyObject)
import Halogen.Store.Monad (getStore)
import Data.Function.Uncurried (runFn2)
import Foreign (unsafeToForeign)
import Data.Either
import Effect.Exception (message)

sendComponentTime start end component = do 
  logDebug "sendComponentTime enter .."
  { config: Config {scaffoldHost, sha256Commit} } <- getStore
  let payload =  
            "component" := component
         ~> "totalTime" := (end - start)
         ~> jsonEmptyObject
  req <- H.liftEffect $ runFn2 Scaffold.mkLogReq sha256Commit (unsafeToForeign payload)
  resp <- makeWithResp scaffoldHost Scaffold.mkFrontApi $ Scaffold.sendLog req
  case resp of 
    Right _ -> logDebug "ok"
    Left err -> logDebug $ "errr- >" <> message err