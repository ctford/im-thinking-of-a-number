module Lib
    ( startApp
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

type API = Get '[PlainText] String

api :: Proxy API
api = Proxy

server :: Server API
server = return "Hello World!"

app :: Application
app = serve api server

startApp :: IO ()
startApp = run 8080 app