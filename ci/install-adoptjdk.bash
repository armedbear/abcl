#!/usr/bin/env bash
DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

. ${DIR}/install-jenv.bash

jdk=$1
if [[ -z $jdk ]]; then
    jdk=openjdk8
fi

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

determine_adoptjdk
download_and_extract
add_jdk

. ${DIR}/set-jdk.bash

jenv doctor







