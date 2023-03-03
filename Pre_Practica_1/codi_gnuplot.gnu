set term png
set output "prova_prac.png"

set xlabel "N_2"
set ylabel "y"
set logscale y
set key left top
set style line 2 lt rgb "red" 



plot "P1-22-23-res1.dat" title "valors", "P1-22-23-res1.dat" using 1:($1**3/6) title "assimptòtics" with linespoints ls 2
