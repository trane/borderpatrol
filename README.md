Border Patrol is a type-safe, immutable, functional Scala library built on top of [Finagle](https://finagle.github.io/)
that provides modular components useful for session management and authentication. This library is used at
[Lookout](http://lookout.com) for single sign on with support for multiple authentication backends.

The original version (as a server) can be found here (nginx+lua): [ngx_borderpatrol](https://www.github.com/lookout/ngx_borderpatrol)

Badges
------
[![Build Status](https://travis-ci.org/lookout/borderpatrol.png)](https://travis-ci.org/lookout/borderpatrol)
[![Coverage Status](https://coveralls.io/repos/lookout/borderpatrol/badge.png)](https://coveralls.io/r/lookout/borderpatrol)

Modules
-------

Border Patrol uses a multi-project structure and contains the following _modules_:

* [`borderpatrol-core`](borderpatrol-core) - the core classes/functions
* [`borderpatrol-example`](borderpatrol-example) - the demo app showing sessions and authentication for multiple
services
* [`borderpatrol-auth`](borderpatrol-auth) - different authentication plugins for core auth
* [`borderpatrol-security`](borderpatrol-security) - different security plugins, e.g. CSRF protection

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


Contributing
------------

We would love to make this better, so please help us!

* [Submit a PR](CONTRIBUTING.md) including an issue label ["easy"](https://github.com/lookout/borderpatrol/issues?q=is%3Aopen+is%3Aissue+label%3Aeasy)
* Give it a star
* Join us on IRC `#borderpatrol` on [Freenode](http://freenode.net)

License
-------

We use the MIT License [License](LICENSE)
