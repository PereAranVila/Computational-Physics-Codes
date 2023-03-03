set term png
set output "RK4-FIG4.png"

set xlabel "t"
set ylabel "posicions (x)"
set title "Osiciladors Acoblats"

set key right bottom
plot "valors-RK4.dat" index 3 using 1:4 title "x1 (RK4)" with lines, "valors-RK4.dat" index 3 using 1:5 title "x2 (RK4)" with lines