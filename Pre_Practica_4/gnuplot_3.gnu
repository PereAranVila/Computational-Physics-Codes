set term png
set output "P4-22-23-fig3.png"

set ylabel "error"
set xlabel "h"

set logscale y
set xrange[0:0.05]

set key right top
plot "P4-22-23-res3.dat" using 1:(abs($2-$3)) title "x" with lines, "P4-22-23-res4.dat" using 1:(abs($2-$3)) title "x=Lsin(t)" with lines