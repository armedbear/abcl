/^java.options/ {print $0 " " options; next}
/^abcl.javac.target/ {print "abcl.javac.target=" target; next}
/^abcl.javac.source/ {print "abcl.javac.source=" source; next}
{print $0}
