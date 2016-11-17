{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module App where

import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.List
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO

-- * api

type API = "users" :> Get '[JSON] [User]
        :<|> Capture "name" String :> Get '[JSON] User

-- * Users

data User = User
  { name :: String
  , age :: Int
  , email :: String
  } deriving (Eq, Show, Generic)

instance FromJSON User
instance ToJSON User

users1 :: [User]
users1 =
  [ User "Richard Davies" 14 "xXDarkHeart17Xx@aol.com"
  , User "Ada Lovelace" 100 "og_ada@gmail.com"
  ]

-- * app

run :: IO ()
run = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< app1

server1 :: Server API
server1 =
    getUsers :<|>
    getUserByName

userAPI :: Proxy API
userAPI = Proxy

app1 :: IO Application
app1 = return $ serve userAPI server1

type Handler = ExceptT ServantErr IO

getUsers :: Handler [User]
getUsers = return users1

getUserByName :: String -> Handler User
getUserByName s = case find (\n -> s == (name n)) users1 of
                    Just user -> return user
                    Nothing -> throwE err404
