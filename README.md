# BorderPatrol

[![Coverage Status](https://coveralls.io/repos/lookout/borderpatrol/badge.png)](https://coveralls.io/r/lookout/borderpatrol)

BorderPatrol is an authentication and session management service that
lives at the border of your network.

This version is a port from nginx+lua to finagle. For more information see
[ngx_borderpatrol](https://www.github.com/lookout/ngx_borderpatrol)

## Developing/Contributing

### Installation and running

Border Patrol is based on TwitterServer and all dependencies are managed by SBT
(Maven may or may not work and is experimental).

On OSX:

    * brew install sbt
    * cd borderpatrol
    * ./sbt
    * project borderpatrol-core
    * run

This will get you a dumb little prototype responding differently to /b and /d routes

### IRC

Join `#borderpatrol` on the [Freenode](http://freenode.net)

## TODO

  * Fix Maven builds
  * Implement Auth Token client
  * Routing
  * much, much more
