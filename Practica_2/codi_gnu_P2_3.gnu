set term png
set output "P2-22-23-fig3-b.png"

set ylabel "x_{3} (cm)"
set xlabel "t(s)"

set key right top

set xrange[0:3]

plot "P2-22-23-res1-b.dat" using 1:4, "P2-22-23-res2-b.dat" using 1:2, "P2-22-23-res2-b.dat" using 1:3