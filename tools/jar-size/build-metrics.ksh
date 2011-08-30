#!/bin/ksh -x

results=build-metrics.out

ANT=/usr/bin/ant

build () {
    cmd="$ANT abcl.clean abcl.jar"
    /usr/bin/time -p $cmd 2>>$results
}

for (( rev=1 ; $rev<1635 ; rev+=10 )) ; do
    hg update -C -r $rev
    printf "-----" >> $results
    rm -f dist/abcl.jar
    if build ; then
        hg log --template 'changeset: {rev}:{node}\ndate: {date|isodate}\nsvn: r{svnrev}\ndescription: {desc|firstline}\n' -r $rev >> $results
        size=`/usr/gnu/bin/du --bytes dist/abcl.jar | awk '{ print $1 }'`
        echo "abcl.jar-size: $size" >> $results
    else 
        echo "changeset $rev failed to build." >> $results
    fi
done


    