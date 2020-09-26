#!/usr/bin/env bash

dir="cffi"
uri="https://github.com/armedbear/${dir}"
root="${HOME}/common-lisp"
tag="abcl/easye-20200602a"

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
