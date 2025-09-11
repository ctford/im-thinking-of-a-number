{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
module Lib
    ( startApp
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API.Raw
import Network.Wai.Application.Static
import WaiAppStatic.Types
import Data.Text (pack)

-- Basic Grade type - we'll start with just Safe
data Grade = Safe deriving (Show, Eq)

-- Simple indexed monad for effects
newtype IxApp i j a = IxApp { runIxApp :: IO a }

instance Functor (IxApp i j) where
    fmap f (IxApp x) = IxApp (f <$> x)

instance Applicative (IxApp i i) where
    pure = IxApp . pure
    IxApp f <*> IxApp x = IxApp (f <*> x)

instance Monad (IxApp i i) where
    IxApp x >>= f = IxApp (x >>= runIxApp . f)

-- For now, serve static HTML at root
type API = Raw

api :: Proxy API
api = Proxy

-- Serve static files from the static directory
server :: Server API
server = serveDirectoryWith $ (defaultWebAppSettings "static")
    { ssRedirectToIndex = True
    , ssIndices = [unsafeToPiece (pack "index.html")]
    }

app :: Application
app = serve api server

startApp :: IO ()
startApp = do
    putStrLn "Starting Haskell server on port 8080..."
    putStrLn "Serving static files from ./static/"
    run 8080 app