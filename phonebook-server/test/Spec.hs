import API.Phonebook (Login (Login), mkApp, password, token, username)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as Text
import Network.HTTP.Client (Response (responseStatus))
import Network.HTTP.Req
import Network.HTTP.Types (status404, status409)
import Network.HTTP.Types.Status (Status)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setHost,
    setPort,
  )
import Test.Hspec
import Types

main :: IO ()
main = hspec $ beforeAll startServer $ do
  describe "Phonebook API" $ do
    let ac1 = AddContact {name = "kaka", number = "23589"}
        ac2 = AddContact {name = "mama", number = "8924"}
        c1 = Contact {id = 1, name = ac1.name, number = ac1.number}
        c2 = Contact {id = 2, name = ac2.name, number = ac2.number}
    it "returns no contacts when there are none in the DB" $ const $ do
      res <- callAPI Nothing GET personsUrl NoReqBody
      shouldSatisfy (responseBody res :: [Contact]) null
    it "can create a new contact" $ \token -> do
      res <- callAPI (Just token) POST personsUrl (ReqBodyJson ac1)
      responseBody res `shouldBe` c1
    it "can create another contact" $ \token -> do
      res <- callAPI (Just token) POST personsUrl (ReqBodyJson ac2)
      responseBody res `shouldBe` c2
    it "can get all contacts without token" $ const $ do
      res <- callAPI Nothing GET personsUrl NoReqBody
      responseBody res `shouldBe` [c1, c2]
    it "refuses to add a duplicate contact with same name" $ \token ->
      callAPIIgnoreResponse (Just token) POST personsUrl (ReqBodyJson ac1)
        `shouldThrowStatusCodeException` status409
    it "can find a given contact by its ID without token" $ const $ do
      res <- callAPI Nothing GET (personsUrl /: Text.pack (show c1.id)) NoReqBody
      responseBody res `shouldBe` c1
    it "fails with 404 to find a contact that does not exist" $ const $ do
      callAPIIgnoreResponse Nothing GET (personsUrl /: "3") NoReqBody
        `shouldThrowStatusCodeException` status404
    it "fails with 404 to delete a contact that does not exist" $ \token -> do
      callAPIIgnoreResponse (Just token) DELETE (personsUrl /: "3") NoReqBody
        `shouldThrowStatusCodeException` status404
    it "can delete an existing contact" $ \token -> do
      res <- callAPI (Just token) DELETE (personsUrl /: Text.pack (show c1.id)) NoReqBody
      responseBody res `shouldBe` c1
    it "does not return a deleted contact" $ const $ do
      res <- callAPI Nothing GET personsUrl NoReqBody
      responseBody res `shouldBe` [c2]

shouldThrowStatusCodeException :: IO a -> Status -> Expectation
shouldThrowStatusCodeException call status = call `shouldThrow` checkStatus
  where
    checkStatus e =
      maybe False ((== status) . responseStatus) (isStatusCodeException e)

personsUrl :: Url 'Http
personsUrl = http "localhost" /: "api" /: "persons"

type Token = ByteString

callAPI ::
  ( HttpBodyAllowed (AllowsBody method) (ProvidesBody body),
    MonadIO m,
    HttpMethod method,
    HttpBody body,
    FromJSON a
  ) =>
  Maybe Token ->
  method ->
  Url scheme ->
  body ->
  m (JsonResponse a)
callAPI token method url body =
  runReq defaultHttpConfig $
    req
      method
      url
      body
      jsonResponse
      (maybe Prelude.id ((<>) . authTokenOpt) token $ port testPort)

callAPIIgnoreResponse ::
  ( HttpBodyAllowed (AllowsBody method) (ProvidesBody body),
    MonadIO m,
    HttpMethod method,
    HttpBody body
  ) =>
  Maybe Token ->
  method ->
  Url scheme ->
  body ->
  m IgnoreResponse
callAPIIgnoreResponse token method url body =
  runReq defaultHttpConfig $
    req
      method
      url
      body
      ignoreResponse
      (maybe Prelude.id ((<>) . authTokenOpt) token $ port testPort)

authTokenOpt :: ByteString -> Option scheme
authTokenOpt token = header "Authorization" ("Bearer " <> token)

testPort :: Int
testPort = 3589

startServer :: IO ByteString
startServer = do
  let settings = setPort testPort . setHost "*" $ defaultSettings
  app <- mkApp ""
  _ <- forkIO $ runSettings settings app
  threadDelay (3 * 1000 * 1000) -- wait for server to start
  let creds = Login {username = "kaka", password = "nana"}
  fmap (BSC.pack . token . responseBody)
    . runReq defaultHttpConfig
    $ req
      POST
      (http "localhost" /: "login")
      (ReqBodyJson creds)
      jsonResponse
      (port testPort)
