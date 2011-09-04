ABCL Build Metrics
==================

Automated ksh build script to retrieve ABCL versions by Mercurial tag,
recording both the time for build and the size of abcl.jar.

Primarily of interest for developers of ABCL.

From the files produced by build-metrics.lisp, gnuplot is easily used
to plot relationships:

   gnuplot> plot "build-metrics.data" using 1:4
   

   

