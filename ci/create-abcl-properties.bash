#!/usr/bin/env bash
DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

jdk=$1
if [[ -z $jdk ]]; then
    jdk=openjdk8
fi

root=${DIR}/..
echo Configuring ${root}/abcl.properties for $jdk

case $jdk in
    openjdk6)
        options="-d64 -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=1g -XX:+UseConcMarkSweepGC"
	abcl_javac_source=1.6
        ;;
    openjdk7)
	options="-d64 -XX:+UseG1GC"
	abcl_javac_source=1.6
	;;
    openjdk8)
        options="-XX:+UseG1GC -XX:+AggressiveOpts -XX:CompileThreshold=10"
	abcl_javac_source=1.8
        ;;
    openjdk11)
        options="-XX:CompileThreshold=10"
	abcl_javac_source=1.8
        ;;
    openjdk12|openjdk13|openjdk14)
	options="-XX:CompileThreshold=10"
	abcl_javac_source=1.8
	;;
    *)
	echo Unrecognized platform ${jdk}
esac

cat ${root}/abcl.properties.in | awk -F = -v options="$options" -v source="$abcl_javac_source" '/^java.options/ {print $0 " " options; next}; /^abcl.javac.source/ {print "abcl.javac.source=" source; next}; {print $0}' > ${root}/abcl.properties

