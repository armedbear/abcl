export JENV_ROOT=$HOME/.jenv                                                    
if [[ $(echo $PATH | grep -c .jenv) -eq 0 ]]; then
   export PATH="$JENV_ROOT/bin:$PATH"
fi

eval "$(jenv init -)"
eval "$(jenv enable-plugin export)"   
