#!/usr/bin/env bash
DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

target=~/.jenv

if [[ ! -r "${target}" ]]; then 
    git clone https://github.com/jenv/jenv.git "${target}"
fi

. ${DIR}/ensure-jenv-is-present.bash

jenv enable-plugin ant
jenv enable-plugin maven
jenv enable-plugin export 

jenv doctor


