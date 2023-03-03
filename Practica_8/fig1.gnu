set term png
set output "P8-22-23-b1-fig1.png"

set xlabel "x (Amstrongs)"
set ylabel "phi(Amstrongs^(-1/2))"

set xrange[-8:4]

set key right top
plot "P8-22-23-res-1.dat" index 0 using 1:3 title "E1" with lines, "P8-22-23-res-1.dat" index 1 using 1:3 title "E2" with lines, "P8-22-23-res-1.dat" index 2 using 1:3 title "E3" with lines, "P8-22-23-res-1.dat" index 3 using 1:3 title "E4" with lines