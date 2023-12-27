#!/usr/bin/env bash

dir="nontrivial-gray-streams"
uri="https://github.com/yitzchak/${dir}"
root="${HOME}/common-lisp"
tag="main"

mkdir -p ${root}
pushd ${root}

if [[ ! -d ${dir} ]]; then 
    git clone ${uri} ${dir}
fi

{
  pushd ${dir}
  git pull 
  git checkout $tag
  git show-ref
  git rev-parse
  popd
}

popd
