Scala Glob
==========

This is a library to perform shell-style pattern matching on arbitrary strings.

While there are ways to use globbing directly on pathnames, not everything which
is conceptually a file is actually backed by a file on disk. It could be records
in a database, or a object graph in memory.

Inevitably I find myself having to search through by name, and while a good
regular expression will get the job done, using the familiar glob syntax is much
more pleasant.

Sample Usage
============

Imports
-------
Unless you're hacking on the primitives (by all means do, and send feedback my
way), you should generally only need one import.

```scala
import com.peschke.scalaglob.api._
```

Building a Glob
---------------
```scala
assert( Glob("") == Failure("""(? | * | characterClass | literal):1:1 ...""""") )

val Success(reportPages) = Glob("report_[0-9].???")
```

Testing input
-------------
For simplicity, these examples use the `reportPages` glob defined in the previous example
```scala
assert( reportPages.test("report_0.txt") )
assert( reportPages.test("report_0.jpg") )

assert( reportPages.test("report_1.txt") )
assert( reportPages.test("report_2.jpg") )

assert( !reportPages.test("pdf_1.txt") )
assert( !reportPages.test("report_12.jpg") )
```

Feature Roadmap
===============

Globbing syntax and capabilities are nonstandard, so for the sake of simplicity
I'm mostly following the [Man Page](http://man7.org/linux/man-pages/man7/glob.7.html),
though I plan to eventually implement the goodies in the [Bash Manual](https://www.gnu.org/software/bash/manual/html_node/Pattern-Matching.html).

Base Functionality
------------------
- [x] Matches literal strings
- [x] Basic wildcards (`?` and `*`)
- [X] Simple character classes (ex: `[abc]`)
- [X] Ranges in character classes (ex `[a-c]`)
- [X] Support negation of character classes (ex: `[!0-9]` matches non-digit characters)
- [ ] Named character classes (ex: `[[:digit:]]`)

POSIX Compatibility
-------------------
- [ ] Support configuration of regex-style character class negation (ex: `[^0-9]`)
- [ ] Support configuration of POSIX-style handling of invalid globs
- [ ] Match leading '.' only when the glob contains a leading literal '.'
- [ ] Handle path separators in a glob in a compliant manner

Bash Compatibility ([`extglob`](http://www.linuxjournal.com/content/bash-extended-globbing))
==================
- [ ] Optional subgroups (ex: `a?(bc|de)f`)
- [ ] Optional repeated subgroups (ex: `a*(bc|de)f`)
- [ ] Required repeated subgroups (ex: `a+(bc|de)f`)
- [ ] Alternative patterns (ex: `a@(bc|de)f`)
- [ ] Pattern negation (ex: `a!(bc|de)f`)
