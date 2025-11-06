#!/usr/bin/env bash

uri="https://gitlab.common-lisp.net/ansi-test/ansi-test"
commitish="master"

topdir="$(dirname ${ABCL_ROOT:-/tmp}/..)"
if [ ! -d "${topdir}" ]; then
  mkdir -p "${topdir}"
fi 

pushd "${topdir}" && echo "Cloning <${uri}> under <file://${topdir}>."
if [[ ! -r ansi-test ]]; then
    git clone "${uri}"
else
    pushd ansi-test
    if [[ -r .hg ]]; then
        hg pull -u
    else
        git fetch
    fi
    popd
fi

pushd ansi-test
# DEPRECATED pin known working version
# <https://gitlab.common-lisp.net/ansi-test/ansi-test/-/issues/31>
# git checkout 1c832cf
echo "Explicitly updating <file://${topdir}> worktree to <commit:${commitish}>." \
    && git checkout  "$commitish"
echo "References to <commit:${commitish}>: " && git show-ref "$commitish"
popd 

popd 
