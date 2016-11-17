{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module App where

import           Data.Aeson
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Language.Javascript.JQuery
import           Network.HTTP.Client        (defaultManagerSettings,
                                             newManager)
import           Servant.API
import           Servant.Client
import           Servant.JS

-- * api

type API =  "api" :> "v2" :> "pokemon" :> Capture "id" Int :> Get '[JSON] Pokemon
      :<|>  "api" :> "v2" :> "pokemon" :> Capture "name" String :> Get '[JSON] Pokemon


data Pokemon = Pokemon
  { id              :: Int
  , name            :: String
  , base_experience :: Int
  , height          :: Int
  , is_default      :: Bool
  , order           :: Int
  , weight          :: Int
  } deriving (Show, Generic)

instance FromJSON Pokemon

api :: Proxy API
api = Proxy

getPokemonId :: Int -> ClientM Pokemon
getPokemonName :: String -> ClientM Pokemon
getPokemonId :<|> getPokemonName = client api

apiJS :: Text
apiJS = jsForAPI api jquery

run :: IO ()
run = print $ apiJS
