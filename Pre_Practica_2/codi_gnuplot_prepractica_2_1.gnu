set term png
set output "P2-18P-fig2.png"

set xlabel "$x_{2} (cm)$"
set ylabel "x (cm)"

set key right top

plot "P2-18P-res1.dat" using 3:4, "P2-18P-res1.dat" using 3:5
