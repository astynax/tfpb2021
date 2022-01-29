{-# LANGUAGE DeriveGeneric #-}

module App (run) where

import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai
import Network.Wai.Middleware.RequestLogger (logStdout)
import Web.Scotty.Trans
import Lucid (renderText, HtmlT)
import Lucid.Html5
import Data.String (IsString(fromString))
import Data.IORef
import Control.Monad.IO.Class
import Data.Aeson hiding (json)
import GHC.Generics
import Control.Monad.Reader
import Data.Text.Lazy

data ApiCounter =
  ApiCounter { visits :: Int }
  deriving (Generic)

data AppState = AppState
  { asVisits :: IORef Int
  }

instance ToJSON ApiCounter

makeApp :: IO Application
makeApp = do
  v <- newIORef (0 :: Int)
  let state = AppState
        { asVisits = v
        }
  scottyAppT (flip runReaderT state) $ do
    pages
    api

type AppM a = ScottyT Text (ReaderT AppState IO) a

pages :: AppM ()
pages = do
  get "/" $ do
    c <- asVisits <$> ask
    liftIO $ incCounter c
    html $ renderText $ page "Index" $ do
      h1_ "Hello world!"
      div_ [class_ "block"] "Press me!"
  where
    incCounter c = atomicModifyIORef' c (\x -> (x + 1, ()))

api :: AppM ()
api = do
  get "/api/v1/visits" $ do
    c <- asVisits <$> ask
    v <- ApiCounter <$> liftIO (readIORef c)
    json v

run :: IO ()
run = do
  putStrLn "Started on 8000..."
  app <- makeApp
  Warp.run 8000 $ logStdout app

page :: Monad m => String -> HtmlT m a -> HtmlT m a
page title content = doctypehtml_ $ do
  head_ $ do
    link_
      [ rel_ "stylesheet"
      , href_ "https://unpkg.com/blocks.css/dist/blocks.min.css"]
    title_ $ fromString title
  body_ content
