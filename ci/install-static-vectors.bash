#!/usr/bin/env bash

dir="static-vectors"
uri="https://github.com/armedbear/${dir}"
root="${HOME}/quicklisp/common-lisp"
tag="abcl/easye-20200603a"

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
