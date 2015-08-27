Border Patrol is a type-safe, immutable, functional Scala library built on top of [Finagle](https://finagle.github.io/)
that provides modular components useful for session management and authentication. This library is used at
[Lookout](http://lookout.com) for single sign on with support for multiple authentication backends.

The original version (as a server) can be found here (nginx+lua): [ngx_borderpatrol](https://www.github.com/lookout/ngx_borderpatrol)

Badges
------

[![Join the chat at https://gitter.im/lookout/borderpatrol](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/lookout/borderpatrol?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/lookout/borderpatrol.png)](https://travis-ci.org/lookout/borderpatrol)
[![Coverage Status](https://img.shields.io/codecov/c/github/lookout/borderpatrol/master.svg)](https://codecov.io/github/lookout/borderpatrol)


Modules
-------

Border Patrol uses a multi-project structure and contains the following _modules_:

* [`core`](core) - the core classes/functions
* [`example`](example) - the demo app showing sessions and authentication for multiple
services
* [`auth`](auth) - different authentication plugins for core auth
* [`security`](security) - different security plugins, e.g. CSRF protection
* [`server`](server) - a server composing these modules that can be configured

Installation
------------

Every Border Patrol module is published at Bintray and SNAPSHOT builds are published to JFrog.

* _stable_ release (not *officially* available yet):

```scala
libraryDependencies ++= Seq(
  "com.lookout.borderpatrol" %% "[borderpatrol-module]" % "0.1.0"
)
```

* `SNAPSHOT` release:

```scala
libraryDependencies ++= Seq(
  "com.lookout.borderpatrol" %% "[borderpatrol-module]" % "0.1.0-SNAPSHOT" changing()
)
```

Building Border Patrol
----------------------

To build Border Patrol you should have [sbt](http://www.scala-sbt.org/0.13/tutorial/Setup.html)
installed (prefer v0.13.8+). Run `sbt`, and then use any of the following commands:

 * `compile`: compile the code
 * `project [project]`: to switch projects, e.g. "project example"
 * `console`: launch a REPL
 * `test`: run the tests
 * `unidoc`: generate the documentation
 * `scalastyle`: run the style-checker on the code
 * `validate`: run tests, style-checker, and doc generation

Running the example
-------------------

```
$ sbt
> project example
> run
```

Documentation
-------------

* Scaladoc is available at [http://lookout.github.io/borderpatrol/docs](http://hackers.lookout.com/borderpatrol/docs/#com.lookout.borderpatrol.package)
* Markdown documents are available [here](https://github.com/lookout/borderpatrol/tree/master/docs/src/main/tut).  The code examples are fully runnable in a Scala REPL verified with [tut](https://github.com/tpolecat/tut).  Use `sbt tut` to compile example code in markdown (`docs/src/main/tut`) which outputs to `target/scala-N.NN/tut`

Contributing
------------

We would love to make this better, so please help us!

* [Submit a PR](CONTRIBUTING.md) including an issue label ["easy"](https://github.com/lookout/borderpatrol/issues?q=is%3Aopen+is%3Aissue+label%3Aeasy)
* Write ScalaDoc comments
* Write tutorials and examples
* Improve tests
* Help with code review
* Give it a star
* Join us on IRC `#borderpatrol` on [Freenode](http://freenode.net)

License
-------

We use the MIT License [License](LICENSE)
