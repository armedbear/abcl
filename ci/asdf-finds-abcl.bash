#!/usr/bin/env bash
DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

dest=$HOME/.config/common-lisp/source-registry.conf.d

mkdir -p $dest

echo "(:tree \""${ABCL_ROOT}"\")" > ${dest}/abcl.conf

