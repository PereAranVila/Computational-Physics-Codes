set term png
set output "P5-22-23-fig2.png"

set xlabel "x"

set xrange[0:3]

set key right top

plot "P5-22-23-res.dat" index 1 using 1:2:3 with yerrorbars title "histograma", [0:3] ((pi/4.)*exp((-pi/4.)*x)) title "p(x)" with lines 