#!/usr/bin/env bash


tmpdir=/tmp
pushd "${tmpdir}"
ant_base=apache-ant-1.10.14
wget https://www-eu.apache.org/dist/ant/binaries/${ant_base}-bin.zip
unzip ${tmpdir}/${ant_base}-bin.zip
popd 

install_dir="$HOME/.local/share/java"
mkdir -p "${install_dir}"
mv ${tmpdir}/${ant_base} "${install_dir}/apache-ant"

echo Ant binary installed in "${install_dir}/apache-ant/bin"



