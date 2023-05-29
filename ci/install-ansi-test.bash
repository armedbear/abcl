#!/usr/bin/env bash

pushd ${ABCL_ROOT}/..

if [[ ! -r ansi-test ]]; then
    git clone https://gitlab.common-lisp.net/ansi-test/ansi-test
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
# pin known working version
# <https://gitlab.common-lisp.net/ansi-test/ansi-test/-/issues/31>
git checkout 1c832cf
git show-ref
git rev-parse
popd

popd
