set term png
set output "P9-22-23-fig5.png"


set xrange[0:33.5]
set yrange[0:45.5]

set xlabel "x(cm)"
set ylabel "y(cm)"
set view 0,0
splot "P9-22-23-res.dat" index 13 u 1:2:3 with pm3d t"Mapa de temperatures Gauss (T=10, inicial)" 
