# bloodhound-amazonka-auth
[![Build Status](https://travis-ci.org/MichaelXavier/bloodhound-amazonka-auth.svg?branch=master)](https://travis-ci.org/MichaelXavier/bloodhound-amazonka-auth)

Adds convenient Amazon ElasticSearch Service authentication to
Bloodhound.


# Usage

```haskell
env <- newEnv region Discover
let auth = env ^. envAuth
let hook req = withAuth auth $ ae ->
                 either (liftIO . throwIO) return =<< amazonkaAuthHook ae region req
mgr <- newManager tlsManagerSettings
let bhe = (mkBHEnv server mgr) { bhRequestHook = hook }
```
