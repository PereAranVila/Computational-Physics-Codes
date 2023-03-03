set term png
set output "P2-22-23-fig1-b.png"

set ylabel "x(t) (cm)"
set xlabel "t(s)"

set key right top

plot "P2-22-23-res1-b.dat" using 1:2, "P2-22-23-res1-b.dat" using 1:3, "P2-22-23-res1-b.dat" using 1:7