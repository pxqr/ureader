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

For usage see [USAGE](USAGE.md) document.

### Install

Recommended way to install the package is:

* Optional: Debian and Gentoo users could install dependecies using
system package manager, though this step is strictly optional. For a
complete dependencies list see ureader [hackage][hackage] page.
* `$ cabal install ureader`
* Optional: add `~/.cabal/bin` to the `PATH`, if not already.

### Build Status [![Build Status][travis-img]][travis-log]

### Maintainer <pxqr.sta@gmail.com>

You can report any issues at [Issue tracker][issues].



[sample]: https://raw.github.com/wiki/pxqr/ureader/sample-output.png
[issues]: https://github.com/pxqr/ureader/issues
[hackage]: http://hackage.haskell.org/package/ureader
[travis-img]: https://travis-ci.org/pxqr/ureader.png
[travis-log]: https://travis-ci.org/pxqr/ureader
