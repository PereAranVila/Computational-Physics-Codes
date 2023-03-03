set term png
set output "P3-22-23-fig1.png"

set xlabel "E"

set key right top

set yrange[-10:60000]
set xrange[0:2*pi]

plot "P3-valors-E-F-dF.dat" using 1:2 with lines, "P3-valors-E-F-dF.dat" using 1:3 with lines