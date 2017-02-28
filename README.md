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

See `examples` subdirectory for example usage.

For more information, build and read the Haddock docs: `cabal haddock`.


TODO
----
* Specify which endpoints to start using annotations?
* Add long-polling simulation of server-triggered events?
* HTTPS support for files served using Haste.App.Standalone.
* Guarantee that hops via local nodes are handled correctly.
* Give build tool capability to download and install GHC and Cabal
  locally when missing.
* Add flags to build tool for passing flags to cabal, changing paths, etc.
* Add short tutorial for build tool.
* Let build tool config file contain information about all endpoints, so they
  can all be built at the same time.
* Have build tool pass a cabal flag indicating the name of the endpoint
  currently being built.
