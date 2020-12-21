#!/usr/bin/env bash
DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

jdk=$1
if [[ -z $jdk ]]; then
    jdk=openjdk8
fi

root="${DIR}/.."
prop_in="${root}/abcl.properties.in"
prop_out="${root}/abcl.properties"
echo "Configuring for $jdk from <${prop_in}>."

# Unused
# zgc="-XX:+UnlockExperimentalVMOptions -XX:+UseZGC -Xmx<size> -Xlog:gc"

abcl_javac_source=1.8
case $jdk in
    6|openjdk6)
        options="-d64 -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=1g -XX:+UseConcMarkSweepGC"
        ant_build_javac_target=1.6
	ant_build_javac_source=1.6
        ;;
    7|openjdk7)
	options="-d64 -XX:+UseG1GC"
        ant_build_javac_target=1.7
	ant_build_javac_source=1.7
	;;
    8|openjdk8)
        options="-XX:+UseG1GC -XX:+AggressiveOpts -XX:CompileThreshold=10"
	ant_build_javac_target=1.8
	ant_build_javac_source=1.8
        ;;
    11|openjdk11)
        options="-XX:CompileThreshold=10"
	ant_build_javac_target=11
	ant_build_javac_source=1.8
        ;;
    # untested: weakly unsupported 
    12|openjdk12)
        options="-XX:CompileThreshold=10"
	ant_build_javac_target=12
	ant_build_javac_source=1.8
        ;;
    13|openjdk13)
        options="-XX:CompileThreshold=10"
	ant_build_javac_target=13
	ant_build_javac_source=1.8
        ;;
    14|openjdk14)
        options="-XX:CompileThreshold=10 ${zgc}"
	ant_build_javac_target=14
	ant_build_javac_source=1.8
        ;;
    15|openjdk15)
        options="-XX:CompileThreshold=10 ${zgc}"
	ant_build_javac_target=15
	ant_build_javac_source=1.8
        ;;
    16|openjdk16)
        options="-XX:CompileThreshold=10 ${zgc}"
	ant_build_javac_target=16
	ant_build_javac_source=1.8
        ;;
esac

cat ${root}/abcl.properties.in \
    | awk -F = \
	  -v options="$options" \
	  -v target="$ant_build_javac_target" \
	  -v source="$ant_build_javac_source" \
       -f ${DIR}/create-abcl-properties.awk \
  > ${root}/abcl.properties

echo "Finished configuring for $jdk into <${prop_out}>."
