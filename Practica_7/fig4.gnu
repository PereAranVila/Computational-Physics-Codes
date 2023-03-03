set term png
set output "P7-22-23-fig4.png"

set xlabel "t (s)"


set key right top
plot "P7-22-23-res.dat" index 3 using 1:4 with lines title "Euler V(t)+K(t)", "P7-22-23-res.dat" index 3 using 1:5 with lines title "Euler K(t)"
