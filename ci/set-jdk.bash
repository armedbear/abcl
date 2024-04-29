#!/usr/bin/env bash

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
            version=$(jenv versions | grep ^..1\.8\.[0-9] | tail -1 | sed s/*// | awk '{print $1}')
            ;;
        openjdk11)
            version=$(jenv versions | grep ^..11\.[0-9] | tail -1 | sed s/*// | awk '{print $1}')
            ;;
        openjdk17)
            version=$(jenv versions | grep ^..17\.[0-9] | tail -1 | sed s/*// | awk '{print $1}')
            ;;
        openjdk17)
            version=$(jenv versions | grep ^..19\.[0-9] | tail -1 | sed s/*// | awk '{print $1}')
            ;;
        openjdk20)
            version=$(jenv versions | grep ^..20\.[0-9] | tail -1 | sed s/*// | awk '{print $1}')
            ;;
        openjdk21)
            version=$(jenv versions | grep ^..21\.[0-9] | tail -1 | sed s/*// | awk '{print $1}')
            ;;
        openjdk22)
            version=$(jenv versions | grep ^..22\.[0-9] | tail -1 | sed s/*// | awk '{print $1}')
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

    jenv local ${version}
    if [[ 0 -ne $? ]]; then
        echo Failed to set local JDK in ${dir} to ${version}
    fi

    jenv global ${version}
    if [[ 0 -ne $? ]]; then
        echo Failed to set global JDK to ${version}
    fi

    popd
}

set_jdk $1 $2
