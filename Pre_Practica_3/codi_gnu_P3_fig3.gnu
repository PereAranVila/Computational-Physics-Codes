set term png
set output "P3-22-23-fig3.png"

set ylabel "f'"
set xlabel "E"

plot "P3-22-23-res3-n34.dat" using 1:3, "P3-22-23-res3-n34.dat" using 1:4, "P3-22-23-res3-n420.dat" using 1:3, "P3-22-23-res3-n420.dat" using 1:4