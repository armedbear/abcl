function set_jdk() {
    . ${DIR}/ensure-jenv-is-present.bash
    jenv versions

    case ${ABCL_JDK} in
        openjdk8)
            version=$(jenv versions | grep openjdk | grep 1.8 | tail -1 | sed s/*//)
            ;;
        openjdk11)
            version=$(jenv versions | grep openjdk | grep 11.0 | tail -1 | sed s/*//)
            ;;
    esac

    if [[ -z ${version} ]]; then
        version=${jenv versions | tail -1 | sed s/*//)
    fi

    pushd ${TRAVIS_BUILD_DIR}

    jenv local ${version}
    # but practically we guard every invocation of jenv this way
    jenv global ${version}

    jenv version

    popd
}

set_jdk
