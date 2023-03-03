set term png
set output "mapa-temp-sobrerelaxacio.png"


set xrange[0:32.5]
set yrange[0:16.5]

set xlabel "x(cm)"
set ylabel "y(cm)"
set view 0,0
splot "P9-22-23-res.dat" index 5 u 1:2:3 with pm3d t"Mapa de temperatures 2D Sobrerelaxacio"