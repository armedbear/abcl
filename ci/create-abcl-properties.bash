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

case $jdk in
    6|openjdk6)
        options="-d64 -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=1g -XX:+UseConcMarkSweepGC"
	abcl_javac_source=1.6
        ;;
    7|openjdk7)
	options="-d64 -XX:+UseG1GC"
	abcl_javac_source=1.6
	;;
    8|openjdk8)
        options="-XX:+UseG1GC -XX:+AggressiveOpts -XX:CompileThreshold=10"
        ;;
    11|openjdk11)
        options="-XX:CompileThreshold=10"
        ;;
    # untested: weakly unsupported 
    12|openjdk12)
        options="-XX:CompileThreshold=10"
        ;;
    13|openjdk13)
        options="-XX:CompileThreshold=10"
        ;;
    14|openjdk14)
        options="-XX:CompileThreshold=10 ${zgc}"
        ;;
esac

cat ${root}/abcl.properties.in | awk -F = -v options="$options" -v source="$abcl_javac_source" '/^java.options/ {print $0 " " options; next}; /^abcl.javac.source/ {print "abcl.javac.source=" source; next}; {print $0}' > ${root}/abcl.properties

echo "Finished configuring for $jdk into <${prop_out}>."
