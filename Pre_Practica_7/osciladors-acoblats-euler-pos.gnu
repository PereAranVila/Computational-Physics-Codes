set term png
set output "euler-osciladors-acoblats-posicions.png"

set xlabel "t"
set ylabel "x"


set key right bottom
plot "valors-euler.dat" index 1 using 1:4 with lines title "x1-Euler", "valors-euler.dat" index 1 using 1:5 with lines title "x2-Euler" 