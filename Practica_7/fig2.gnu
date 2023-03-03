set term png
set output "P7-22-23-fig2.png"

set xlabel "t (s)"
set ylabel "phi (rad)"


set key right top
plot "P7-22-23-res.dat" index 2 using 1:2 with lines title "Euler"
