set term png
set output "plot-primer-eigenvalue.png"

set xlabel "x"
set ylabel "phi"
set title "shcro"

set key right bottom
plot "P8-22-23-res.dat" index 0 using 1:3 title "E=4.43" with lines