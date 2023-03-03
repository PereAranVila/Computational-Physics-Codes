set term png
set output "P4-22-23-c-fig1.png"

set ylabel "error"
set xlabel "h"

set logscale y

set key right top
plot "P4-22-23-c-res1.dat" using 1:(abs($2-0.10438601865254E+18)) title "Trapezis" with lines, "P4-22-23-c-res1.dat" using 1:(abs($3-0.10438601865254E+18)) title "Simpson" with lines