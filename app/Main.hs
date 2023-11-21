{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Amazonka
import Amazonka.S3 (BucketName (..), ObjectKey (..))
import Amazonka.S3.GetObject (GetObject, GetObjectResponse, getObjectResponse_body, newGetObject)
import Conduit (ConduitM, MonadTrans (lift))
import Control.Lens (view, (<&>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ResourceT)
import Data.ByteString (ByteString)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Conduit ()
import System.IO (stdout)

type API = "getfile" :> StreamGet NoFraming OctetStream (SourceIO ByteString)

server :: Handler (SourceIO ByteString)
server = liftIO $ do
    let reg = NorthVirginia
        bucketName = BucketName "pi-staging-artifact"
        objectKey = ObjectKey "2697001/file.csv"
    lgr <- newLogger Debug stdout
    env <- newEnv discover <&> \e -> e{logger = lgr, region = reg}

    let req :: GetObject
        req = newGetObject bucketName objectKey

        respBodyConduit :: GetObjectResponse -> ConduitM () ByteString (ResourceT IO) ()
        respBodyConduit = view (getObjectResponse_body . _ResponseBody)

    pure $ toSourceIO $ do
        resp <- lift $ send env req
        respBodyConduit resp

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = do
    putStrLn "serving on 8081"
    run 8081 app