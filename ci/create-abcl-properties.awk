/^java.options/ {print $0 " " options; next}
/ant.build.javac.target/ {print "ant.build.javac.target=" target; next}
/ant.build.javac.source/ {print "ant.build.javac.source=" source; next}
{print $0}
