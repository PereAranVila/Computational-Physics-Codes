set term png
set output "P2-18P-fig3.png"

set ylabel "x_{3} (cm)"
set xlabel "t(s)"

set key right top

set xrange[0:3]

plot "P2-18P-res1.dat" using 1:4, "P2-18P-res2.dat" using 1:2
