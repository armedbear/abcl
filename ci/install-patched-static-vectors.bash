#!/usr/bin/env bash
#
# Until <https://github.com/sionescu/static-vectors/pull/23>
# resolved, we need to use our patched version.

root="${HOME}/quicklisp/local-projects"
dir="static-vectors"
tag="easye/abcl"

mkdir -p ${root}
pushd ${root}

if [[ ! -d ${dir} ]]; then 
    git clone https://github.com/armedbear/${dir} ${dir}
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
