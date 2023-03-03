set term png
set output "P1-22-23-fig1.png"

set xlabel "N"
set ylabel "S"
set logscale y
set key left top


plot "P1-22-23-res1.dat" title "calculats", "P1-22-23-res1.dat" using 1:(($1)**3)*(5./9.) title "assimptota" with lines