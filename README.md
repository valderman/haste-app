Haste.App
=========

A framework for n-way distributed web applications.


Requirements
------------
* [Haste](http://haste-lang.org) version 0.6+
* `haste-prim` and `haste-lib`, version 0.6+
* GHC version 7.10


Usage
-----

An example application:

```haskell
{-# LANGUAGE StaticPointers, TypeFamilies, OverloadedStrings #-}
import Haste
import Haste.App
import qualified Haste.JSString as JSS

instance Node Server where
  type ClientOf Server = Client
  endpoint _           = Endpoint "localhost" 24601
  invoke               = invokeServer

hello :: String -> Client String
hello = remote $ static (import_ $ \name -> do
    annotate :: RunsOn Server
    liftIO $ putStrLn $ name ++ " says hello"
    return $ "Why, hello to you too, " ++ name ++ "!"
  )

main = runApp [endpoint (Proxy :: Proxy Server)] $ do
  name <- prompt "What's your name?"
  reply <- hello (JSS.unpack name)
  alert (JSS.pack reply)
```

Compile the program once with `hastec --output-html`, and once with
`ghc --make`. You will now have one binary and one HTML document generated from
the same Haste.App program. Start the binary and open the document in a web
browser. Congratulations, you've written, compiled and run your first Haste.App
application!

The `import_` function imports a computation on some server node to the client.
This import can then be turned into a client-side function, using the `static`
keyword and the `remote` function, which when called cause the computation to
be executed on its home node, and the result to be returned to the client.

Server nodes are defined by creating an appropriate monad -- in this case the
build-in `Server` monad -- and defining a `Node` instance for it.
A node is defined by which other node it is attached to (`ClientOf`, where that
node can reach it over the network (`endpoint`), and how to perform
computations on the node (`invoke`).

The `runApp` function takes two arguments: the nodes which should be started
by the server-side binary, and the client-side program to execute in the
browser. Any number of applications may run concurrently on the same web page,
by calling `runApp` multiple times.


TODO
----
* Port haste-standalone and add standalone-configured endpoints.
* Generalise `remote` to import functions to any node, not just `Client`.
* Specify which endpoints to start using annotations.
* Add REST service local call endpoint constructor and EDSL for describing how
  to translate from function calls to such services.
* Add long-polling simulation of server-triggered events?
