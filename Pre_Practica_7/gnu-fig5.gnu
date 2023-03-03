set term png
set output "P7-22-23-fig5.png"

set xlabel "t (s)"
set ylabel "phi (rad)"


set key right top
plot "P7-22-23-res.dat" index 8 using 1:2 with lines title "+0.05 rad", "P7-22-23-res.dat" index 9 using 1:2 with lines title "-0.05 rad"