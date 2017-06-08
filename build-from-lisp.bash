#!/usr/bin/env bash 
# $Id$
#
# Build ABCL from a supported Lisp

usage()
{
    echo
    echo "USAGE:"
        echo "$0 <implementation>"
#    echo "$0 <implementation> [[ --clean=T | --full=T | --batch=NIL ]]"
}

if [ -z "$1" ]; then
    usage
    exit 1
fi

check_boolean()
{
    case "$1" in
        [Tt]|[Nn][Ii][Ll])
            :;;
        *)
            usage
            echo "Error: Argument \`$1' is neither \"nil\" nor \"t\"."
            exit 1
            ;;
    esac
}

IMPL="$1"
TEMP=$(getopt --long clean:,full:,batch: -n "$0" -- "$@") 

if [ $? != 0 ] ; then 
    usage 
    exit 1
fi
eval set -- "$TEMP"

# TODO all ignored
CLEAN="t"
FULL="t"
BATCH="t"

# TODO all ignored
while true ; do
    case "$1" in
        --clean) 
            check_boolean "$2"
            CLEAN="$2" 
            shift 2 
            ;;
        --full)  
            check_boolean "$2"
            FULL="$2"
            shift 2 
            ;;
        --batch) 
            check_boolean "$2"
            BATCH="$2" 
            shift 2 
            ;;
        --) shift; break ;;
        *)  echo "Internal error!" ; exit 1 ;;
        esac
done

FORM="(abcl/build:abcl/build)"
FILE="src/org/abcl/lisp/build/build-abcl.lisp"

abcl()
{
    exec "$1" --load "$2" --eval "(progn $3 (ext:quit))"
}

ecl()
{
    exec "$1" -norc -load "$2" -eval "(progn $3 (ext:quit))"
}

clisp()
{ 
    exec "$1" -ansi -q -norc -i "$2" -x "(progn $3 (ext:quit))"
}

sbcl()
{
    exec "$1" --no-userinit --load "$2" --eval "(progn $3 (sb-ext:quit))"
}

cmucl()
{
    exec "$1" -noinit -load "$2" -eval '(setq *load-verbose* nil)' \
                                 -eval "(progn $3 (ext:quit))"
}

ccl()
{
    exec "$1" -Q --no-init --load "$2" --eval "(progn $3 (ccl:quit))"
}

notimplemented()
{
    usage
    echo "Error: The build script does not currently support $1."
    echo "It's easy to change, though. Look at $0, and send a patch!"
    exit 1
}



# We pass along and execute "$1" so users can pass "sbcl-cvs"
# etc. instead of "sbcl".

case "$IMPL" in
    abcl*)
        abcl  "$IMPL" "$FILE" "$FORM"          ;;
    clisp*)
        clisp "$IMPL" "$FILE" "$FORM"          ;;
    sbcl*)
        sbcl  "$IMPL" "$FILE" "$FORM"          ;;
    lisp)
        cmucl "$IMPL" "$FILE" "$FORM"          ;;   
    ccl*)
        ccl   "$IMPL" "$FILE" "$FORM"          ;;
    gcl*)
        notimplemented "$IMPL" "$FILE" "$FORM" ;;
    ecl*)
        ecl   "$IMPL" "$FILE" "$FORM"          ;;
    alisp*)
        notimplemented "$IMPL" "$FILE" "$FORM" ;;
    *)
        usage; 
        echo "Error: Unrecognized implementation: $IMPL"
        exit 1 
        ;;
esac
