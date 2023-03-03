set term png
set term png
set output "P4-22-23-fig2.png"

set ylabel "error Area (km^{2})"
set xlabel "h (km)"

set logscale y

set key right bottom
plot "P4-22-23-res.dat" index 1 using 1:(abs($2-0.10438601865254E+18)) title "aprox superior Trapezis" with lines, "P4-22-23-res.dat" index 0 using 1:(abs($3-0.10438601865254E+18)) title "Simpson" with lines