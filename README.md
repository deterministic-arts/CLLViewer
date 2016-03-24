# CLLViewer

This (not-quite-so) tiny program is an indexer and viewer for old 
newsgroup dumps. It was written for (and primarily tested with) the
dump of `comp.lang.lisp` available at http://barzilay.org/misc/cll.txt.bz2

# Getting started

Grab a dump (eg. the one mentioned above) and uncompress it (if it was
compressed). Load the code into your lisp by issuing a `(load "loadup")`
on the REPL. No, there is no proper ASDF system definition for this 
project. And no: there probably never will be.

First, you will have to produce an index file (which is simply an SQLite 
database). Do

    > (cll-indexer::generate-index-database :source-file #P"/wherever/you/placed/the/dump.txt")

This will scan the dump file, and produce an SQLite database, which will
have the same name as the dump-file, but an extension of `.db`, in the 
example `#P"/wherever/you/placed/the/dump.db"`. Note, that the indexer
process can take quite some time.

You may need to grab a few additional systems from this github account
directly (things like `darts.lib.annotatable` and `darts.lib.trivia` 
are not (yet) accessible via quicklisp)

# Viewing

The CLIM UI can be started with

    > (cll-viewer::run-listener)

Once started, use the `Open Store` command to open the index database
and the underlying dump file. The command expects to be given the path
to the dump file (!) and it will assume, that the index database can be
found at the same location (by simply exchanging the filename extension
as described above).

# Support

None. None whatsoever. I wrote this to support my personal curiosity. If
you find it helpful -- great! If it doesn't work for you -- that's life.
