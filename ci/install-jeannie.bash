#!/usr/bin/env bash

dir="jeannie"
uri="https://github.com/easye/${dir}"
root="${HOME}/common-lisp"
tag="master"

mkdir -p ${root}
pushd ${root}

if [[ ! -d ${dir} ]]; then 
    git clone ${uri} ${dir}
fi

pushd ${dir}
if [[ -d .hg ]]; then
    hg pull
    hg update -r $tag
    hg sum -v
else
    git pull
    git checkout $tag
    git show-ref
    git rev-parse
fi
popd

popd
