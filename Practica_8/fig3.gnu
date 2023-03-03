set term png
set output "P8-22-23-b1-fig3.png"

set xlabel "x (Amstrongs)"
set ylabel "phi(Amstrongs^(-1/2))"

set xrange[-8:8]

set key right top
plot "P8-22-23-res-2.dat" index 0 using 1:3 title "1r VAP" with lines, "P8-22-23-res-2.dat" index 1 using 1:3 title "2n VAP" with lines, "P8-22-23-res-2.dat" index 2 using 1:3 title "3r VAP" with lines