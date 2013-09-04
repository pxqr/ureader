### Description

`ureader` is minimalistic command line RSS reader with unicode and
color support. Everything it does is fetch RSS documents, merge them
according to specified options, format and flush resulting feed to
stdout. So `ureader` could be used with pagers like `more(1)` or in
linux terminal.

UReader aims to be:

* simple - no complex configs, no DB, minimum of external state,
  etc. Everything you need to know to use it is `--help` flag and the
  `~/.ureader` directory;
* usable - customizable feed;
* pretty - preserve as much formatting as posssible. Currently ureader
  support some frequently used subset of HTML.

### Examples

The sample produced by `$ ureader feed --unread | more`:

![sample][sample]

### Documentation

For help see `$ ureader --help` and `$ ureader COMMAND --help`.

For usage see [USAGE][USAGE.md] document.

### Install

Recommended way to install the package in Debian is:

```bash
$ apt-get install libghc-text-prof libghc-network-prof libghc-http-prof libghc-optparse-applicative-prof libghc-ansi-wl-pprint-prof libghc-xml-prof libghc-feed-prof libghc-tagsoup-prof
$ cabal install implicit-params parallel-io
$ git clone git://github.com/pxqr/ureader.git && cd ureader && cabal install
```

and add `~/.cabal/bin` to the `PATH`, if not already.

### Build Status [![Build Status][travis-img]][travis-log]

### Maintainer <pxqr.sta@gmail.com>

You can report any issues at [Issue tracker][issues].



[sample]: https://raw.github.com/wiki/pxqr/ureader/sample-output.png
[issues]: https://github.com/pxqr/ureader/issues
[travis-img]: https://travis-ci.org/pxqr/ureader.png
[travis-log]: https://travis-ci.org/pxqr/ureader
