set term png
set output "repr-tir.png"

set xlabel "x"
set ylabel "phi"
set title "shcro"

set key right bottom
plot "E1.dat" index 0 using 1:3 title "E1" with lines, "E2.dat" index 0 using 1:3 title "E2" with lines