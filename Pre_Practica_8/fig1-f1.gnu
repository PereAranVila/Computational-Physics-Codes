set term png
set output "RK4-FIG1.png"

set xlabel "x"
set ylabel "y"


set key right top
plot "valors-RK4.dat" index 0 using 1:2 title "RK4 f1" with lines