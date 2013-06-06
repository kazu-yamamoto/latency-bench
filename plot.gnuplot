set terminal png
set output "data/latency.png"
set xrange [0:100]
plot "data/c.data"                     u 2:(1./100000.) smooth cumulative title "C", \
     "data/ghc-7.6.3-nonthreaded.data" u 2:(1./100000.) smooth cumulative title "GHC 7.6.3", \
     "data/ghc-7.6.3-threaded.data"    u 2:(1./100000.) smooth cumulative title "GHC 7.6.3 -threaded", \
     "data/ghc-7.7-nonthreaded.data"   u 2:(1./100000.) smooth cumulative title "GHC 7.7", \
     "data/ghc-7.7-threaded.data"   u 2:(1./100000.) smooth cumulative title "GHC 7.7 -threaded"
