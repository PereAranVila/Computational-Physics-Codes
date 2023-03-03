set term png
set output "P2-22-23-fig2-b.png"

set ylabel "x(t) (cm)"
set xlabel "x_{1} (cm)"

set key right top

plot "P2-22-23-res1-b.dat" using 2:3, "P2-22-23-res1-b.dat" using 2:6
