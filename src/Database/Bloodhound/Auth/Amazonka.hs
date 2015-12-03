{-# LANGUAGE DeriveDataTypeable #-}
module Database.Bloodhound.Auth.Amazonka
    ( amazonkaAuthHook
    , EsAmazonkaAuthError(..)
    ) where


-------------------------------------------------------------------------------
import           Control.Exception
import           Data.Bifunctor
import           Data.Time.Clock
import           Data.Typeable
import           Database.Bloodhound.Types
import           Network.AWS.Data.Body
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Path
import           Network.AWS.Data.Query
import           Network.AWS.ElasticSearch   (elasticSearch)
import           Network.AWS.Types           (AuthEnv, Region, sgRequest,
                                              sgSign)
import qualified Network.AWS.Types           as A
import           Network.HTTP.Client
import           Network.HTTP.Types.Method   (parseMethod)
-------------------------------------------------------------------------------


-- | Request hook to install into your 'BHEnv'. Does not handle
-- streaming request bodies, which should not be an issue for
-- Bloodhound. The exception cases handled by 'EsAmazonkaAuthError'
-- are truly exceptional and should probably be thrown.
--
-- @
--    env <- newEnv region Discover
--    let auth = env ^. envAuth
--    let hook req = withAuth auth $ \ae ->
--                     either (liftIO . throwIO) return =<< amazonkaAuthHook ae region req
--    mgr <- newManager tlsManagerSettings
--    let bhe = (mkBHEnv server mgr) { bhRequestHook = hook }
-- @
amazonkaAuthHook :: AuthEnv -> Region -> Request -> IO (Either EsAmazonkaAuthError Request)
amazonkaAuthHook ae reg req = do
  now <- getCurrentTime
  case toAwsRequest req reg of
    Right req' -> return (Right (sgRequest (algo req' ae reg now)))
    Left e -> return (Left e)
  where algo = sgSign (A._svcSigner elasticSearch)


-------------------------------------------------------------------------------
toAwsRequest :: Request -> Region -> Either EsAmazonkaAuthError (A.Request a)
toAwsRequest r reg = do
  meth <- first (const badMethod) (parseMethod bsMeth)
  rqb <- toRQBody (requestBody r)
  return (A.Request { A._rqService = svc
                    , A._rqMethod = meth
                    , A._rqPath = rawPath (path r)
                    , A._rqQuery = toQuery (queryString r)
                    , A._rqHeaders = requestHeaders r
                    , A._rqBody = rqb})
  where bsMeth = method r
        badMethod = InvalidStdMethod bsMeth
        svc = elasticSearch { A._svcEndpoint = const endpoint}
        endpoint = requestEndpoint r reg


-------------------------------------------------------------------------------
requestEndpoint :: Request -> Region -> A.Endpoint
requestEndpoint r reg = A.Endpoint { A._endpointHost = host r
                                   , A._endpointSecure = secure r
                                   , A._endpointPort = port r
                                   , A._endpointScope = toBS reg}

-------------------------------------------------------------------------------
-- | These edge cases shouldn't come up in normal operation. The best
-- course of action is probably to throw these as an exception.
data EsAmazonkaAuthError = InvalidStdMethod Method
                         | StreamingBodyNotSupported deriving (Show, Eq, Typeable)


instance Exception EsAmazonkaAuthError

-------------------------------------------------------------------------------
toRQBody :: RequestBody -> Either EsAmazonkaAuthError RqBody
toRQBody (RequestBodyLBS b) = Right (toBody b)
toRQBody (RequestBodyBS b) = Right (toBody b)
toRQBody _ = Left StreamingBodyNotSupported
