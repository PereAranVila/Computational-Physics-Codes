set term png
set output "P5-22-23-fig1.png"

set xlabel "x"

set xrange[-4.*pi:4.*pi]


set key right top
plot "P5-22-23-res.dat" index 0 using 1:2:3 with yerrorbars title "histograma", [-4.*pi:4.*pi] (sin((x/4.)))**2/(4.*pi) title "p(x)" with lines