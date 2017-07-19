{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Control.Exception
import           Control.Lens                               (set, view)
import           Control.Retry
import           Data.Aeson
import           Data.Monoid
import qualified Data.Proxy                                 as P
import qualified Data.Text                                  as T
import           Data.Time.Clock.POSIX
#if MIN_VERSION_bloodhound(0, 13, 0)
import           Database.V1.Bloodhound
#else
import           Database.Bloodhound
#endif
import           Network.AWS
import           Network.AWS.Env
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           System.IO
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Options
-------------------------------------------------------------------------------
import           Database.Bloodhound.Auth.Amazonka.Internal
-------------------------------------------------------------------------------


main :: IO ()
main = defaultMainWithIngredients ings (askOption tests)
  where
    ings = includingOptions [Option (P.Proxy :: P.Proxy IntegrationServer)]:defaultIngredients


-------------------------------------------------------------------------------
tests :: IntegrationServer -> TestTree
tests (IntegrationServer mServer) = testGroup "bloodhound-amazonka-auth" (sharedTests <> integrationTests')
  where
    sharedTests = [amazonkaAuthHookTests]
    integrationTests' = case mServer of
      Just server -> [integrationTests server]
      _           -> []


-------------------------------------------------------------------------------
integrationTests :: Server -> TestTree
integrationTests server = withResource setup teardown $ \mkEnv -> testGroup "integration"
  [
    testCase "authenticates request" $ do
      env <- mkEnv
      exists <- runBH env $ indexExists testIndex
      exists @?= True
  -- Index patterns add a * to the path, which can wreak havoc if not
  -- accounted for in AWS V4 signing.
  , testCase "authenticates when using index patterns" $ do
      env <- mkEnv
      let search = Search {
            queryBody = Nothing
          , filterBody = Nothing
          , sortBody = Nothing
          , aggBody = Nothing
          , highlight = Nothing
          , trackSortScores = False
          , from = From 0
          , size = Size 0
          , searchType = SearchTypeQueryThenFetch
          , fields = Nothing
          , source = Nothing
          }
      res <- parseEsResponse =<< runBH env (searchByIndex testIndexSplat search)
      case (res :: Either EsError (SearchResult Value)) of
        Right _ -> return ()
        Left e  -> assertFailure (show e)
  ]
  where
    testIndex = IndexName "bloodhound-amazonka-auth-test"
    testIndexSplat = IndexName "bloodhound-amazonka-auth-test*"
    ixs = IndexSettings (ShardCount 1) (ReplicaCount 0)
    setup = do
      mgr <- newManager tlsManagerSettings
      lgr <- newLogger Debug stdout
      env <- set envRegion region . set envLogger lgr<$> newEnvWith Discover Nothing mgr
      let auth = view envAuth env
      let hook req = withAuth auth $ \authEnv -> either throwIO return =<< amazonkaAuthHook authEnv region req
      let bhe = (mkBHEnv server mgr) { bhRequestHook = hook }
      _ <- runBH bhe (createIndex ixs testIndex)
      True <- retrying (constantDelay 5000 <> limitRetries 5) (\_ exists -> return (not exists)) (\_ -> runBH bhe (indexExists testIndex))
      return bhe
    -- could make this customizable if we cared to
    region = NorthVirginia
    teardown bhe = either (\(_ :: SomeException) -> ()) (const ()) <$> try (runBH bhe (deleteIndex testIndex))


-------------------------------------------------------------------------------
amazonkaAuthHookTests :: TestTree
amazonkaAuthHookTests = testGroup "amazonkaAuthHook"
  [
    testCase "does not mangle query parameters" $ do
      req <- parseUrlThrow "http://localhost:9200/foo/foo/_search?scroll=1m&search_type=scan"
      let ae = AuthEnv (AccessKey "access key") (SecretKey "secret key") Nothing Nothing
      let now = posixSecondsToUTCTime 0
      let Right res = amazonkaAuthHook' ae NorthVirginia req now
      secure res @?= False
      host res @?= "localhost"
      port res @?= 9200
      path res @?= "/foo/foo/_search"
      queryString res @?= "?scroll=1m&search_type=scan"
  ]


-------------------------------------------------------------------------------
newtype IntegrationServer = IntegrationServer (Maybe Server)
                          deriving (Show, Eq)


instance IsOption IntegrationServer where
  defaultValue = IntegrationServer Nothing
  parseValue "" = return (IntegrationServer Nothing)
  parseValue s  = return (IntegrationServer (Just (Server (T.pack s))))
  optionName = return "integration-server"
  optionHelp = return "If supplied, tests against a real AWS ES cluster (fees may apply). Uses the standard amazonka methods for discovering credentials."
