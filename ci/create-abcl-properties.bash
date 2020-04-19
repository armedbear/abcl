#!/usr/bin/env bash

jdk=$1
if [[ -z $jdk ]]; then
    jdk=openjdk8
fi

case $jdk in
    openjdk8)
        options="-XX:+UseG1GC -XX:+AggressiveOpts -XX:CompileThreshold=10"
        ;;
    openjdk11)
        options="-XX:CompileThreshold=10"
        ;;
esac

cat abcl.properties.in | awk -F = -v options="$options" '/^java.options/ {print $0 " " options; next}; {print $0}' > abcl.properties

