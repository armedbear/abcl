#!/usr/bin/env bash

pushd ${TRAVIS_BUILD_DIR}/..

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
git show-ref
git rev-parse
popd

popd
