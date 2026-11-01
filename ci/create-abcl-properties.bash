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

# be very permissive when using the post-openjdk8 module system
opens="--add-opens java.base/sun.nio.ch=ALL-UNNAMED\
 --add-opens java.base/java.io=ALL-UNNAMED\
 --add-opens java.base/java.lang=ALL-UNNAMED\
 --enable-native-access=ALL-UNNAMED"

# --add-opens app=ALL-UNNAMED\

# <https://wiki.openjdk.org/spaces/shenandoah/pages/25002018/Main>
# Use -Xlog:gc+ergo or -Xlog:gc+stats for diagnostics
#  -XX:+UseLargePages not supported on arm64-macos-openjdk25
gc_shenandoah="-XX:+UseShenandoahGC -Xlog:gc+ergo -XX:+AlwaysPreTouch"

abcl_javac_source=1.8
case $jdk in
    release)
	options=""
	;;	
    6|openjdk6)
        options="-d64 -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=1g -XX:+UseConcMarkSweepGC"
        ant_build_javac_target=1.6
        ;;
    7|openjdk7)
	options="-d64 -XX:+UseG1GC"
        ant_build_javac_target=1.7
	;;
    8|openjdk8)
        options="-XX:+UseG1GC -XX:+AggressiveOpts -XX:CompileThreshold=10"
	ant_build_javac_target=1.8
        ;;
    11|openjdk11)
        options="-XX:CompileThreshold=10"
	ant_build_javac_target=11
        ;;
    # untested: weakly unsupported 
    12|openjdk12)
        options="-XX:CompileThreshold=10"
	ant_build_javac_target=12
        ;;
    13|openjdk13)
        options="-XX:CompileThreshold=10"
	ant_build_javac_target=13
        ;;
    14|openjdk14)
        options="-XX:CompileThreshold=10 ${zgc}"
	ant_build_javac_target=14
        ;;
    15|openjdk15)
        options="-XX:CompileThreshold=10 ${zgc}"
	ant_build_javac_target=15
        ;;
    16|openjdk16)
        options="-XX:CompileThreshold=10 ${zgc}"
	ant_build_javac_target=16
        ;;
    17|openjdk17)
        options="-XX:CompileThreshold=10 ${opens}"
	ant_build_javac_target=17
        ;;
    18|openjdk18)
        options="-XX:CompileThreshold=10 ${opens}"
	ant_build_javac_target=18
        ;;
    19|openjdk19)
        options="-XX:CompileThreshold=10 ${opens}"
	ant_build_javac_target=19
        ;;
    21|openjdk21)
        options="-XX:CompileThreshold=10 ${opens}"
	ant_build_javac_target=21
        ;;
    22|openjdk22)
        options="-XX:CompileThreshold=10 ${opens}"
	ant_build_javac_target=22
        ;;
    23|openjdk23)
        options="-XX:CompileThreshold=10 ${opens}"
	ant_build_javac_target=23
        ;;
    25|openjdk25|*)
        options="-XX:CompileThreshold=10 ${opens}"
	ant_build_javac_target=25
        ;;
esac

cat ${root}/abcl.properties.in \
    | awk -F = \
	  -v options="$options" \
	  -v target="$ant_build_javac_target" \
       -f ${DIR}/create-abcl-properties.awk \
  > ${root}/abcl.properties

# cat ${root}/abcl.properties.in \
#     | awk -F = \
# 	  -v options="$options" \
# 	  -v target="$ant_build_javac_target" \
# 	  -v source="$ant_build_javac_source" \
#        -f ${DIR}/create-abcl-properties.awk \
#   > ${root}/abcl.properties

echo "Finished configuring for $jdk into <${prop_out}>."
