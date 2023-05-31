#!/usr/bin/env bash
DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

. ${DIR}/install-jenv.bash

jdk=$1
if [[ -z $jdk ]]; then
    jdk=openjdk8
fi

uname=$2
if [[ -z $uname ]]; then
    uname=$(uname)
fi

# empty variables are not necessary, but a hint that these are not
# lexically scoped in their modification.
topdir=
dist=
function determine_openjdk() {
    case $uname in
        # just x86_64 for now.  We've got Rosseta2 c'est nes pas?
        Darwin)
            case $jdk in
                openjdk8)
                    v=372
                    build=b07
                    version=1.8.0.${v}
                    topdir=jdk8u${v}-${build}
                    dist="https://github.com/adoptium/temurin8-binaries/releases/download/jdk8u${v}-${build}/OpenJDK8U-jdk_x64_mac_hotspot_8u${v}${build}.tar.gz"
                    ;;
                openjdk11)
                    version=11.0.19
                    build=7
                    topdir=jdk-${version}+${build}
                    dist="https://github.com/adoptium/temurin11-binaries/releases/download/jdk-${version}%2B${build}/OpenJDK11U-jdk_x64_mac_hotspot_${version}_${build}.tar.gz"
                    ;;
                # deprecated
                openjdk14)  # Need version from adoptium
                    topdir=jdk-14.0.2+12
                    dist="https://github.com/AdoptOpenJDK/openjdk14-binaries/releases/download/jdk-14.0.2%2B12/OpenJDK14U-jdk_x64_mac_hotspot_14.0.2_12.tar.gz"
                    ;;
                # deprecated
                openjdk15) # Need version from adoptium
                    topdir=jdk-15+36
                    dist="https://github.com/AdoptOpenJDK/openjdk15-binaries/releases/download/jdk-15%2B36/OpenJDK15U-jdk_x64_mac_hotspot_15_36.tar.gz"
                    ;;
                # deprecated
                openjdk16)
                    topdir=jdk-16.0.2+7
                    dist="https://github.com/adoptium/temurin16-binaries/releases/download/jdk-16.0.2%2B7/OpenJDK16U-jdk_x64_mac_hotspot_16.0.2_7.tar.gz"
                    ;;
                openjdk17)
                    version=17.0.7
                    build=7
                    topdir="jdk-${version}+${build}"
                    dist="https://github.com/adoptium/temurin17-binaries/releases/download/jdk-${version}%2B${build}/OpenJDK17U-jdk_x64_mac_hotspot_${version}_${build}.tar.gz"
                    ;;
                # deprecated
                openjdk19)
                    v="19"
                    id="${v}.0.2"
                    rev="7"
                    arch="jdk_x64_mac_hotspot"
                    topdir="jdk-${id}+${rev}"
                    dist="https://github.com/adoptium/temurin${v}-binaries/releases/download/jdk-${id}%2B${rev}/OpenJDK${v}U-${arch}_${id}_${rev}.tar.gz"
                    ;;
                openjdk20)
                    v="20"
                    id="${v}.0.1"
                    rev="9"
                    arch="jdk_x64_mac_hotspot"
                    topdir="jdk-${id}+${rev}"
                    dist="https://github.com/adoptium/temurin${v}-binaries/releases/download/jdk-${id}%2B${rev}/OpenJDK${v}U-${arch}_${id}_${rev}.tar.gz"
                    ;;
            esac
            ;;
        Linux)
            case $jdk in
                openjdk8)
                    version=u372
                    build=b07
                    topdir=jdk8${version}-${build}
                    dist="https://github.com/adoptium/temurin8-binaries/releases/download/jdk8${version}-${build}/OpenJDK8U-jdk_x64_linux_hotspot_8${version}${build}.tar.gz"
                    ;;
                openjdk11)
                    version=11.0.19
                    build=7
                    topdir=jdk-${version}+${build}
                    dist="https://github.com/adoptium/temurin11-binaries/releases/download/jdk-${version}%2B${build}/OpenJDK11U-jdk_x64_linux_hotspot_${version}_${build}.tar.gz"
                    ;;
                # deprecated
                openjdk14) # Need version from adoptium
                    topdir=jdk-14.0.2+12
                    dist="https://github.com/AdoptOpenJDK/openjdk14-binaries/releases/download/jdk-14.0.2%2B12/OpenJDK14U-jdk_x64_linux_hotspot_14.0.2_12.tar.gz"
                    ;;
                # deprecated
                openjdk15) # Need version from adoptium
                    topdir=jdk-15+36
                    dist="https://github.com/AdoptOpenJDK/openjdk15-binaries/releases/download/jdk-15%2B36/OpenJDK15U-jdk_x64_linux_hotspot_15_36.tar.gz"
                    ;;
                # deprecated
                openjdk16)
                    topdir=jdk-16.0.2+7
                    dist="https://github.com/adoptium/temurin16-binaries/releases/download/jdk-16.0.2%2B7/OpenJDK16U-jdk_x64_linux_hotspot_16.0.2_7.tar.gz"
                    ;;
                openjdk17)
                    version=17.0.7
                    build=7
                    topdir="jdk-${version}+${build}"
                    dist="https://github.com/adoptium/temurin17-binaries/releases/download/jdk-${version}%2B${build}/OpenJDK17U-jdk_x64_linux_hotspot_${version}_${build}.tar.gz"
                    ;;
                # deprecated
                openjdk19)
                    v="19"
                    id="${v}.0.2"
                    rev="7"
                    arch="jdk_x64_linux_hotspot"
                    topdir="jdk-${id}+${rev}"
                    dist="https://github.com/adoptium/temurin${v}-binaries/releases/download/jdk-${id}%2B${rev}/OpenJDK${v}U-${arch}_${id}_${rev}.tar.gz"
                    ;;
                openjdk20)
                    v="20"
                    id="${v}.0.1"
                    rev="9"
                    arch="jdk_x64_linux_hotspot"
                    topdir="jdk-${id}+${rev}"
                    dist="https://github.com/adoptium/temurin${v}-binaries/releases/download/jdk-${id}%2B${rev}/OpenJDK${v}U-${arch}_${id}_${rev}.tar.gz"
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

determine_openjdk
download_and_extract
add_jdk

. ${DIR}/set-jdk.bash ${jdk}

jenv doctor
