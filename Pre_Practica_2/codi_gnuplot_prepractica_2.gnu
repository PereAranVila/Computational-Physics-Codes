set term png
set output "P2-18P-fig1.png"

set xlabel "x (cm)"
set ylabel "t (s)"

set key right top

plot "P2-18P-res1.dat" using 1:2, "P2-18P-res1.dat" using 1:3, "P2-18P-res1.dat" using 1:4, "P2-18P-res1.dat" using 1:5