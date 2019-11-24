if [[ $(echo $PATH | grep -c .jenv) -eq 0 ]]; then
   export PATH="$HOME/.jenv/bin:$PATH"
fi

eval "$(jenv init -)"

