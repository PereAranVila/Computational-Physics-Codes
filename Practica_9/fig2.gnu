set term png
set output "P9-22-23-fig2.png"


set xlabel "numero iteracions"
set ylabel "T (graus centigrads) "

unset xrange
unset yrange

plot "P9-22-23-res.dat" index 4 u 1:2  title "Gauss-Seidel" with lines, "P9-22-23-res.dat" index 6 u 1:2 title "sobrerelaxacio" with lines