### Getting started

This step by step guide show some common `ureader` usages.
Commands appear in their usage order.

First you need to see at feed to decide if you like it:

```bash
$ ureader view http://hackage.haskell.org/packages/archive/recent.rss
```

If feed seems interesting to you then you can add it to your feed
list:

```bash
$ ureader add http://hackage.haskell.org/packages/archive/recent.rss
```

The `add` command is the same as `$ echo $URI >> feed.list`.

After you added sufficient amount of links using the `view` and `add`
commands you have a choice - you can view feed using either _batch_ or
_stream mode_. To show your feed in _batch mode_ use the `feed`
command:

```bash
$ ureader feed | $PAGER
```

where the `$PAGER` could be:

* [more(1)][more] — colored, underlined and bold text;
* [less(1)][less] — ignore colors, clutter because of ESC characters;
* [most(1)][most] — some colors, cluttering again.

The `feed` have some formatting options, use `ureader feed --help` to
get help about command. The _stream mode_ enabled by the `stream`
command:

```bash
$ ureader stream
```

[more]: http://linux.die.net/man/1/more
[less]: http://linux.die.net/man/1/less
[most]: http://linux.die.net/man/1/most

### Feed list

Feed list is just a file containing a bunch of URIs separated by
newlines. Default location of feed list is
`~/.ureader/feeds`. `ureader` will ignore lines which don't seems to
be an URI. For example you can comment and group your feeds like this:

```
Haskell
=======
http://planet.haskell.org/rss20.xml
http://hackage.haskell.org/packages/archive/recent.rss

Debian
=====
http://planet.debian.org/rss20.xml

Linux
=====
# http://feeds.feedburner.com/org/LOR
```

This means that URI in shell-style comment ignored completely.

```
http://some.ignored/feed  # wrong comment
```

### Local feeds

`ureader` support only URIs as feed source location. However it's
possible to show feed from a file using file URI scheme:

```bash
$ curl http://bramcohen.com/feed > /var/cohen.rss # get RSS document sample
$ ureader view file://localhost/var/cohen.rss     # show feed from a file
```

You could also add the file URI to your feed list just like any other
URI:

```bash
$ ureader add file://localhost/var/cohen.rss
```

### Feed groups

`ureader` do not support any kind of grouping. However you could use
filesystem for that. Suppose you have two feed lists: one for work and
one for home:

```bash
$ ureader feed --feeds ~/.ureader/feed/home # home feeds
$ ureader feed --feeds ~/.ureader/feed/work # work feeds
```

To join feeds use [cat(1)][cat].

```bash
$ cat ~/.ureader/feed/* > ~.ureader/feeds
$ ureader feed # all feeds
```

This way you could make any feed hierarhy you want.

[cat]: http://linux.die.net/man/1/cat
