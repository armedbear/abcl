function set_jdk() {
    abcl_jdk=$1
    if [[ -z ${abcl_jdk} ]]; then
        abcl_jdk=openjdk8
    fi

    dir=$2
    if [[ -z ${dir} ]]; then
        dir=${TRAVIS_BUILD_DIR}
    fi

    . ${DIR}/ensure-jenv-is-present.bash

    jenv versions

    case ${abcl_jdk} in
        openjdk8)
            version=$(jenv versions | grep openjdk | grep 1.8 | tail -1 | sed s/*//)
            ;;
        openjdk11)
            version=$(jenv versions | grep openjdk | grep 11.0 | tail -1 | sed s/*//)
            ;;
    esac

    if [[ -z ${version} ]]; then
        version=$(jenv versions | tail -1 | sed s/*//)
    fi

    if [[ -z ${version} ]]; then
        version=1.8
    fi
    
    pushd ${dir}

    jenv local ${version}
    # but practically we guard every invocation of jenv this way
    jenv global ${version}

    jenv version

    popd
}

set_jdk ${ABCL_JDK} ${TRAVIS_BUILD_DIR}
