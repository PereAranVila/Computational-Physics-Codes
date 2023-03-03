set term png
set output "RK4-FIG2.png"

set xlabel "t"
set ylabel "v"


set key right bottom
plot "valors-RK4.dat" index 1 using 1:2 title "caiguda lliure amb friccio" with lines