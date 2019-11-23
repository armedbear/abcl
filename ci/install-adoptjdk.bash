#!/usr/bin/env bash
DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

source ${DIR}/install-jenv.bash

jdk=$1

# empty variables are not necessary, but a hint that these are not
# lexically scoped in their modification.
topdir=
dist=
function determine_adoptjdk() {
    case $(uname) in
        Darwin)
            case $jdk in
                openjdk8)
                    topdir=jdk8u232-b09
                    dist="https://github.com/AdoptOpenJDK/openjdk8-binaries/releases/download/jdk8u232-b09/OpenJDK8U-jdk_x64_mac_hotspot_8u232b09.tar.gz"
                    ;;
                openjdk11)
                    topdir=jdk-11.0.5+10
                    dist="https://github.com/AdoptOpenJDK/openjdk11-binaries/releases/download/jdk-11.0.5%2B10/OpenJDK11U-jdk_x64_mac_hotspot_11.0.5_10.tar.gz"
                    ;;
            esac
            ;;
        Linux)
            case $jdk in
                openjdk8)
                    topdir=jdk8u232-b09
                    dist="https://github.com/AdoptOpenJDK/openjdk8-binaries/releases/download/${topdir}/OpenJDK8U-jdk_x64_linux_hotspot_8u232b09.tar.gz"
                    ;;
                openjdk11)
                    topdir=jdk-11.0.5+10
                    dist="https://github.com/AdoptOpenJDK/openjdk11-binaries/releases/download/jdk-11.0.5%2B10/OpenJDK11U-jdk_x64_linux_hotspot_11.0.5_10.tar.gz"
                    ;;
            esac
            ;;
        *)
            echo No known dist for $(uname)
    esac
}

tmpdir=/var/tmp

function download_and_extract() {
    pushd ${tmpdir} && wget --continue ${dist}
    
    tar xvz -f $(basename ${dist})

    popd
}

function add_jdk() {
    echo $dist
    echo $tmpdir
    case $(uname) in
        Darwin)
            jenv add ${tmpdir}/${topdir}/Contents/Home
            ;;
        Linux)
            jenv add ${tmpdir}/${topdir}
            ;;
    esac
}

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

    pushd ${TRAVIS_BUILD_DIR}

    jenv local ${version}
    # but practically we guard every invocation of jenv this way
    jenv global ${version}

    popd
}

determine_adoptjdk
download_and_extract
add_jdk
set_jdk




