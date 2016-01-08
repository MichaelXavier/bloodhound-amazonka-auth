{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Data.Time.Clock.POSIX
import           Network.AWS.Types
import           Network.HTTP.Client
import           Test.Tasty
import           Test.Tasty.HUnit
-------------------------------------------------------------------------------
import           Database.Bloodhound.Auth.Amazonka.Internal
-------------------------------------------------------------------------------


main :: IO ()
main = defaultMain tests


-------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "bloodhound-amazonka-auth"
  [
    amazonkaAuthHookTests
  ]


-------------------------------------------------------------------------------
amazonkaAuthHookTests :: TestTree
amazonkaAuthHookTests = testGroup "amazonkaAuthHook"
  [
    testCase "does not mangle query parameters" $ do
      req <- parseUrl "http://localhost:9200/foo/foo/_search?scroll=1m&search_type=scan"
      let ae = AuthEnv (AccessKey "access key") (SecretKey "secret key") Nothing Nothing
      let now = posixSecondsToUTCTime 0
      let Right res = amazonkaAuthHook' ae NorthVirginia req now
      secure res @?= False
      host res @?= "localhost"
      port res @?= 9200
      path res @?= "/foo/foo/_search"
      queryString res @?= "?scroll=1m&search_type=scan"
  ]
