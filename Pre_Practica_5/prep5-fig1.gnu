set term png
set output "P5-22-23-fig1.png"

set xlabel "x"

set xrange[0:pi]


set key right top
plot "P5-22-23-res.dat" index 0 using 1:2:3 with yerrorbars title "histograma", [0:pi] (9./(2.*pi*(3.*(pi**2) -20.)))*x**3 * (sin(x))**3 title "p(x)" with lines