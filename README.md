### Description

`ureader` is minimalistic command line RSS reader with unicode and
color support. Everything it does is fetch RSS documents, merge them
according to specified options, format and flush resulting feed to
stdout. So `ureader` could be used with pagers like `more(1)`, in
linux terminal or from scripts.

### [Documentation][wiki]

For help and usage info see [Wiki pages][wiki].

For brief info see `$ ureader --help` and `$ ureader COMMAND --help`.

### Examples

The sample produced by `$ ureader feed --unread | more`:

![sample][sample]

### Build Status [![Build Status][travis-img]][travis-log]

### Maintainer <pxqr.sta@gmail.com>

You can report any issues at [Issue tracker][issues].

[wiki]:   https://github.com/pxqr/ureader/wiki
[sample]: https://raw.github.com/wiki/pxqr/ureader/sample-output.png
[issues]: https://github.com/pxqr/ureader/issues
[travis-img]: https://travis-ci.org/pxqr/ureader.png
[travis-log]: https://travis-ci.org/pxqr/ureader
