{-# LANGUAGE DeriveDataTypeable #-}
module Database.Bloodhound.Auth.Amazonka.Internal where


-------------------------------------------------------------------------------
import           Control.Applicative         as A
import           Control.Exception
import           Data.Time.Clock
import           Data.Typeable
import           Database.Bloodhound.Types
import           Network.AWS.Data.Body
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Path
import           Network.AWS.Data.Query
import qualified Network.AWS.Data.Query      as A
import           Network.AWS.ElasticSearch   (elasticSearch)
import           Network.AWS.Types           (AuthEnv, Region, sgRequest,
                                              sgSign)
import qualified Network.AWS.Types           as A
import           Network.HTTP.Client
import           Network.HTTP.Types.Method   (parseMethod)
import           URI.ByteString
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
amazonkaAuthHook ae reg req = amazonkaAuthHook' ae reg req A.<$> getCurrentTime


-------------------------------------------------------------------------------
amazonkaAuthHook' :: AuthEnv -> Region -> Request -> UTCTime -> Either EsAmazonkaAuthError Request
amazonkaAuthHook' ae reg req now = toReq <$> toAwsRequest req reg
  where algo = sgSign (A._svcSigner elasticSearch)
        toReq req' = restoreTimeout (decodePath (sgRequest (algo req' ae reg now)))
        -- We decode the path because for some reason AWS ES actually
        -- doesn't want the path url encoded. If you do, it will
        -- expect double-encoding for the canonical uri. If you
        -- double, it will expect triple and so-on.
        decodePath x = x { path = urlDecode True (path x)}
        restoreTimeout x = x { responseTimeout = responseTimeout req }


-------------------------------------------------------------------------------
toAwsRequest :: Request -> Region -> Either EsAmazonkaAuthError (A.Request a)
toAwsRequest r reg = do
  meth <- either (const (Left badMethod)) Right (parseMethod bsMeth)
  rqb <- toRQBody (requestBody r)
  q <- toQS (queryString r)
  return (A.Request { A._rqService = svc
                    , A._rqMethod = meth
                    , A._rqPath = rawPath (path r)
                    , A._rqQuery = q
                    , A._rqHeaders = requestHeaders r
                    , A._rqBody = rqb})
  where bsMeth = method r
        badMethod = InvalidStdMethod bsMeth
        svc = elasticSearch { A._svcEndpoint = const endpoint}
        endpoint = requestEndpoint r reg


-------------------------------------------------------------------------------
toQS :: ByteString -> Either EsAmazonkaAuthError A.QueryString
toQS bs = case parseRelativeRef laxURIParserOptions bs of
            Right rr -> Right (go (rrQuery rr))
            Left _ -> Left MalformedQueryString
  where go q = QList [ QPair k (QValue (Just v)) | (k, v) <- queryPairs q]

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
                         | StreamingBodyNotSupported
                         | MalformedQueryString deriving (Show, Eq, Typeable)


instance Exception EsAmazonkaAuthError

-------------------------------------------------------------------------------
toRQBody :: RequestBody -> Either EsAmazonkaAuthError RqBody
toRQBody (RequestBodyLBS b) = Right (toBody b)
toRQBody (RequestBodyBS b) = Right (toBody b)
toRQBody _ = Left StreamingBodyNotSupported
