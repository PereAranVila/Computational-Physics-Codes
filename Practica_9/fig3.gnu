set term png
set output "P9-22-23-fig3.png"


set xlabel "numero iteracions"
set ylabel "T (graus centigrads) "

unset xrange
unset yrange

plot "P9-22-23-res.dat" index 8 u 1:2  title "Gauss-Seidel" with lines, "P9-22-23-res.dat" index 10 u 1:2 title "sobrerelaxacio" with lines