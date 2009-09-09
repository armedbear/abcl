#!/bin/sh
srcdir=$PWD
tmpdir=/tmp/$$

mkdir $tmpdir

cd $tmpdir

unzip $srcdir/foo.abcl

cp $srcdir/bar.abcl .

cp $srcdir/eek.lisp .

jar cfv $srcdir/baz.jar *

rm -rf $tmpdir

