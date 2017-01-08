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
{-# LANGUAGE StaticPointers, OverloadedStrings #-}
import Haste.App.Simple
import qualified Haste.JSString as JSS

hello :: String -> Client String
hello = remote $ static (import_ $ \name -> do
    annotate :: RunsOn Server
    liftIO $ putStrLn $ name ++ " says hello"
    return $ "Why, hello to you too, " ++ name ++ "!"
  )

main = runSimpleApp $ do
  name <- prompt "What's your name?"
  reply <- hello (JSS.unpack name)
  alert (JSS.pack reply)
```

Compile the program once with `hastec your_app.hs`, and once with
`ghc --make your_app.hs`. You will now have one binary and one JavaScript file
document generated from the same Haste.App program. Then merge the two by
running `./your_app -e your_app.js`. Now and point your web browser to the URL
it prints. Congratulations, you've written, compiled and run your first
Haste.App application!

The `import_` function imports a computation on some server node to the client.
This import can then be turned into a client-side function, using the `static`
keyword and the `remote` function, which when called cause the computation to
be executed on its home node, and the result to be returned to the client.
In this example, the server node used is called `Server` and is defined by
Haste.App as a minimal base from which to build more complicated server nodes.

Server nodes are defined by creating an appropriate monad -- in this case the
build-in `Server` monad -- and defining a `Node` instance for it. This example
uses `Haste.App.Simple` to avoid this boilerplate, but for programs with more
nodes you will need to define such instances yourself.
A node is defined by which other node it is attached to (`ClientOf`, where that
node can reach it over the network (`endpoint`), and how to perform
computations on the node (`invoke`).

This example uses the `runSimpleApp` function, as it uses no other nodes than
`Server`, but for more complex programs the `runApp` function is used.
`runApp` takes two arguments: the nodes which should be started
by the server-side binary, and the client-side program to execute in the
browser. Any number of applications may run concurrently on the same web page,
by calling `runApp` multiple times.

For more information, build and read the Haddock docs: `cabal haddock`.


TODO
----
* Generalise `remote` to import functions to any node, not just `Client`.
* Specify which endpoints to start using annotations.
* Add REST service local call endpoint constructor and EDSL for describing how
  to translate from function calls to such services.
* Add long-polling simulation of server-triggered events?
* HTTPS support for files served using Haste.App.Standalone.
* Handle disconnects between non-server nodes.
* Add commands to build tool: setup.
* Give build tool capability to download and install Haste, GHC and Cabal
  locally when missing.
* Add flags to build tool for passing flags to cabal, printing help, changing
  paths, etc.
* Add short tutorial for build tool.
* Let build tool config file contain information about all endpoints, so they
  can all be built at the same time.
* Have build tool pass a cabal flag indicating the name of the endpoint
  currently being built.
* Write cabal output to log files instead of terminal.
* Make info prints cleaner and more consistent.
