set term png
set output "P4-22-23-fig2.png"

set ylabel "error"
set xlabel "h"

set logscale y

set key right top
plot "P4-22-23-res3.dat" using 1:(abs($2-$3)) notitle with lines