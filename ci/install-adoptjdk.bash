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
                    topdir=jdk8u242-b08
                    dist="https://github.com/AdoptOpenJDK/openjdk8-binaries/releases/download/jdk8u242-b08/OpenJDK8U-jdk_x64_mac_hotspot_8u242b08.tar.gz"
                    ;;
                openjdk11)
                    topdir=jdk-11.0.6+10
                    dist="https://github.com/AdoptOpenJDK/openjdk11-binaries/releases/download/jdk-11.0.6%2B10/OpenJDK11U-jdk_x64_mac_hotspot_11.0.6_10.tar.gz"
                    ;;
                openjdk14)
                    topdir=jdk-14.0.1+7
                    dist="https://github.com/AdoptOpenJDK/openjdk14-binaries/releases/download/jdk-14.0.1%2B7/OpenJDK14U-jdk_x64_mac_hotspot_14.0.1_7.tar.gz"
                    ;;
            esac
            ;;
        Linux)
            case $jdk in
                openjdk8)
                    topdir=jdk8u242-b08
                    dist="https://github.com/AdoptOpenJDK/openjdk8-binaries/releases/download/jdk8u242-b08/OpenJDK8U-jdk_x64_linux_hotspot_8u242b08.tar.gz"
                    ;;
                openjdk11)
                    topdir=jdk-11.0.6+10
                    dist="https://github.com/AdoptOpenJDK/openjdk11-binaries/releases/download/jdk-11.0.6%2B10/OpenJDK11U-jdk_x64_linux_hotspot_11.0.6_10.tar.gz"
                    ;;
                openjdk14)
                    topdir=jdk-14.0.1+7
                    dist="https://github.com/AdoptOpenJDK/openjdk14-binaries/releases/download/jdk-14.0.1%2B7/OpenJDK14U-jdk_x64_linux_hotspot_14.0.1_7.tar.gz"
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







