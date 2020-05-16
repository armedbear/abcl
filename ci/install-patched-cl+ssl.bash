#!/usr/bin/env bash
#
# Until <https://github.com/cl-plus-ssl/cl-plus-ssl/pull/97> is
# resolved, we need to use our patched version.

root="${HOME}/quicklisp/local-projects"
dir="cl-plus-ssl"
tag="easye/stream-fd"

mkdir -p ${root}
pushd ${root}

if [[ ! -d ${dir} ]]; then 
    git clone https://github.com/armedbear/cl-plus-ssl ${dir}
fi

pushd ${dir}
if [[ -d .hg ]]; then
    hg update -r $tag
    hg sum -v
else
    git checkout $tag
    git show-ref
    git rev-parse
fi
popd

popd
