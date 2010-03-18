SLIME
=====

    Author:   Mark Evenson 
    Created:  16-MAR-2010
    Modified: 18-MAR-2010

SLIME is divided conceptually in two parts: the "swank" server process
which runs in the native Lisp and the "slime" client process running
in Emacs Lisp.  These instructions were were written to accompany
ABCL, but there is nothing ABCL specific in the instructions

## Obtaining SLIME

SLIME does not follow a release process in the standard, so you are
best off with obtaining the [latest version from CVS][1]. [Daily
snapshots as gzipped tarballs are also available][2].  Your local OS
packaging system (i.e. MacPorts on OSX) may have a version as well.

[1]: http://common-lisp.net/project/slime/#downloading
[2]: http://common-lisp.net/project/slime/snapshots/slime-current.tgz

## Starting SLIME

One first locates the SLIME directory on the filesystem.  In the code
that follows, the SLIME top level directory is assumed to be
`"~/work/slime"`, so adjust this value to your local value as you see
fit.

Then one configures Emacs with the proper initialization hooks by
adding code something like the following to "~/.emacs":

    :::common-lisp
    (add-to-list 'load-path "~/work/slime")
    (setq slime-lisp-implementations 
      '((abcl ("~/work/abcl/abcl"))
        (abcl.svn ("~/work/abcl.svn/abcl"))
        (sbcl ("/opt/local/bin/sbcl"))))
    (require 'slime)
    (slime-setup '(slime-fancy slime-asdf slime-banner))

One further need to customize the setting of
`SLIME-LISP-IMPLEMENTATIONS` to the location(s) of the Lisp(s) you wish to
invoke via SLIME.  The value is list of lists of the form

   (SYMBOL ("/path/to/lisp"))

where SYMBOL is a mnemonic for the Lisp implementation, and the string
`"/path/to/lisp"` is the absolute path of the Lisp implementation that
SLIME will associate with this symbol.  In the example above, I have
defined three implementations, the main abcl implementation, a version
that corresponds to the latest version from SVN invoked by
`"~/work/abcl.svn/abcl"`, and a version of SBCL.

To start SLIME one simply issues `M-x slime` from Emacs.  This will
start the first entry in the SLIME-LISP-IMPLEMENTATIONS list.  If you
wish to start a subsequent Lisp, prefix the Emacs invocation with a
negative argument (i.e. `C-- M-x slime`).  This will present an
interactive chooser over all symbols contained in
`SLIME-LISP-IMPLEMENTATIONS`.

After you invoke SLIME, you'll see a buffer open up named
`*inferior-lisp*` where the Lisp image is started up, the required swank
code is complied and then loaded, finally, you'll see the "flying
letters" resolving itself to a `"CL-USER>"` prompt with an inspiration
message in the minibuffer.  Your initiation to SLIME has begun...


## Starting swank on its own

In debugging, one may wish to start the swank server by itself without
connection to Emacs.  The following code will both load and start the swank server
from a Lisp image.  One merely needs to change *SLIME-DIRECTORY* to
point to the top directory of the server process.

    :::commmon-lisp
    (defvar *slime-directory* #p"~/work/slime/") ;; Don't forget trailing slash
    (load (merge-pathnames "swank-loader.lisp" *slime-directory*) :verbose t)
    (swank-loader:init)
    (swank:start-server "/tmp/swank.port") ;; remove if you don't want
                                          ;; swank to start listening for connections.

When this code finishes executing, an integer representing the port on
which the server starts will be written to `'/tmp/swank.port'` and also
returned as the result of evaluating `SWANK:START-SERVER`.  One may
connect to this port via issuing `M-x slime-connect` in Emacs.


