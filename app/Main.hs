{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Amazonka
import Amazonka.S3 (BucketName (..), ObjectKey (..))
import Amazonka.S3.GetObject (GetObject, GetObjectResponse, getObjectResponse_body, newGetObject)
import Conduit (ConduitM)
import Control.Lens (view, (<&>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ResourceT)
import Data.ByteString (ByteString)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Conduit ()
import System.IO (stdout)

type API = "getfile" :> StreamGet NoFraming OctetStream (SourceIO ByteString)

server :: ResourceT IO (SourceIO ByteString)
server = do
    let reg = NorthVirginia
        bucketName = BucketName "pi-staging-artifact"
        objectKey = ObjectKey "2697001/file.csv"
    lgr <- newLogger Debug stdout
    env <- newEnv discover <&> \e -> e{logger = lgr, region = reg}

    let req :: GetObject
        req = newGetObject bucketName objectKey

        respBodyConduit :: GetObjectResponse -> ConduitM () ByteString (ResourceT IO) ()
        respBodyConduit = view (getObjectResponse_body . _ResponseBody)

    -- The commented out variant leads to this error:
    -- \$ curl http://localhost:8081/getfile -v --no-buffer
    -- \*   Trying 127.0.0.1:8081...
    -- \* Connected to localhost (127.0.0.1) port 8081 (#0)
    -- > GET /getfile HTTP/1.1
    -- > Host: localhost:8081
    -- > User-Agent: curl/7.82.0
    -- > Accept: */*
    -- >
    -- \* Empty reply from server
    -- \* Closing connection 0
    -- curl: (52) Empty reply from server

    resp <- send env req
    pure $ toSourceIO $ respBodyConduit resp

-- st <- createInternalState
-- resp <- runInternalState (send env req) st
-- pure $ toSourceIO (respBodyConduit resp >> closeInternalState st)

api :: Proxy API
api = Proxy

app :: Application
app =
    serve api $
        hoistServer api (liftIO . runResourceT) server

main :: IO ()
main = do
    putStrLn "serving on 8081"
    run 8081 app