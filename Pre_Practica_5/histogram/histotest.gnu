file1="uniform.dat"
file2="gauss.dat"

set xlabel "x"
set ylabel "p(x)"
set yrange[0:2]

plot file1 u 1:2:4 w yerrorbars t"Montecarlo","" u 1:2 w histeps t"PDF",1 t"exact"

pause -1
set term png
set output "uniformtest.png"
replot

set term x11

set yrange[0:0.5]
plot file2 u 1:2:4 w yerrorbars t"Montecarlo", "" u 1:2 w histeps t"PDF",exp(-x**2/2.)/sqrt(2*pi) t"exact"
pause -1
set term png
set output "gausstest.png"
replot


 
