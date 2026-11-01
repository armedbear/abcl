/^java.options/ {print $0 " " options; next}
/ant.build.javac.target/ {print "ant.build.javac.target=" target; next}
{print $0}
