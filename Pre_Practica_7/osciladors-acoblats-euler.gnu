set term png
set output "euler-osciladors-acoblats.png"

set xlabel "t"
set ylabel "v"


set key right bottom
plot "valors-euler.dat" index 1 using 1:2 with lines title "v1-Euler", "valors-euler.dat" index 1 using 1:3 with lines title "v2-Euler" 