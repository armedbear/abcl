#!/usr/bin/env bash
#
# Until <https://github.com/cl-plus-ssl/cl-plus-ssl/pull/97> is
# resolved, we need to use our patched version.

mkdir -p ${HOME}/quicklisp/local-projects
pushd ${HOME}/quicklisp/local-projects

git clone https://github.com/armedbear/cl-plus-ssl

pushd cl-plus-ssl
git show-ref
git rev-parse
popd

popd
