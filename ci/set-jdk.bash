function set_jdk() {
    abcl_jdk=$1
    if [[ -z ${abcl_jdk} ]]; then
        abcl_jdk=openjdk8
    fi

    echo Configuring JDK from ${abcl_jdk}

    dir=$2
    if [[ -z ${dir} ]]; then
        dir=${ABCL_ROOT}
    fi

    if [[ -z ${ABCL_ROOT} ]]; then
        dir=/var/tmp/set-jdk.$$/
    fi

    . ${DIR}/ensure-jenv-is-present.bash

    echo Available jenv versions $(jenv versions)

    case ${abcl_jdk} in
        openjdk8)
            version=$(jenv versions | grep ^\ *1\.8\.[0-9] | tail -1 | sed s/*//)
            ;;
        openjdk11)
            version=$(jenv versions | grep ^\ *11\.[0-9] | tail -1 | sed s/*//)
            ;;
        openjdk17)
            version=$(jenv versions | grep ^\ *17\.[0-9] | tail -1 | sed s/*//)
            ;;
        openjdk17)
            version=$(jenv versions | grep ^\ *19\.[0-9] | tail -1 | sed s/*//)
            ;;
        openjdk20)
            version=$(jenv versions | grep ^\ *20\.[0-9] | tail -1 | sed s/*//)
            ;;
        *)
            echo Failed to find an available JDK matching ${abcl_jdk}
            echo   in $(jenv versions)
            echo
            echo Falling back to grabbing the last one listed
            version=$(jenv versions | tail -1 | sed s/*//)
    esac

    if [[ ! -d ${dir} ]]; then
        echo Creating directory ${dir}
        mkdir ${dir}
    fi

    pushd ${dir}

    if [[ 0 -ne $(jenv local ${version}) ]]; then 
       echo Failed to set local JDK to ${version}
    fi 
    # but practically we guard every invocation of jenv this way
    if [[ 0 -ne $(jenv global ${version}) ]]; then 
       echo Failed to set global JDK to ${version}
    fi 

    echo Local version to set $(jenv version)

    popd
}

set_jdk ${ABCL_JDK} ${ABCL_ROOT}
