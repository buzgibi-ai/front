module Crypto.Jwt
  ( JwtClaims
  , JwtUser
  , parse
  )
  where

import Prelude

import Effect (Effect)

-- {
--   "ident": 5,
--   "jwtClaims": {
--     "exp": 1689663223.369714,
--     "iat": 1688799223.369714
--   }
-- }
type JwtClaims = { exp :: Int, iat :: Int }

type JwtUser = { ident :: Int, jwtClaims :: JwtClaims }

foreign import parse :: String -> Effect JwtUser
