set term png
set output "P1-22-23-fig2.png"

set xlabel "N"
set ylabel "S/S_{asim}"
set key right top
unset logscale y

plot "P1-22-23-res1.dat" using 1:(9*$2/(5*$1**3)) notitle"