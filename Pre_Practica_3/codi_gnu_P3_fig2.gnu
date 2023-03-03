set term png
set output "P3-22-23-fig2.png"

set xlabel "E_{0}"
set ylabel "N"

set key right top
set xrange[0:3]
plot "P3-22-23-res.dat" using 1:2 