set term png
set output "RK4-FIG5.png"

set xlabel "t"
set ylabel "velocitats (v)"
set title "Osiciladors Acoblats"

set key right bottom
plot "valors-RK4.dat" index 3 using 1:2 title "v1 (RK4)" with lines, "valors-RK4.dat" index 3 using 1:3 title "v2 (RK4)" with lines