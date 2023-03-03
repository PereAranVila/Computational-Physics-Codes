set term png
set output "repr-tir2.png"

set xlabel "x"
set ylabel "phi"
set title "shcro"

set key right bottom
plot "tir-rk4.dat" index 0 using 1:3 title "E1" with lines