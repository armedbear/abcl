#!/usr/bin/env bash

target=~/.jenv

if [[ ! -r "${target}" ]]; then 
    git clone https://github.com/jenv/jenv.git "${target}"
fi

# FIXME: don't always run the init routines?
profile=~/.bash_profile
echo 'export PATH="$HOME/.jenv/bin:$PATH"' >> ${profile}
echo 'eval "$(jenv init -)"' >> ${profile}
. ${profile}

jenv doctor


