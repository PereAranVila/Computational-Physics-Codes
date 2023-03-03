set term png
set output "P5-22-23-fig2.png"

set xlabel "x"

set xrange[-4.*3.:4.*3.]


set key right top
plot "P5-22-23-res.dat" index 2 using 1:2:3 with yerrorbars title "histograma", [-4.*3.:4.*3.] (exp(-x**2/(2.*3.**2)))/(sqrt(2.*pi*3.**2))  title "g(x)" with lines