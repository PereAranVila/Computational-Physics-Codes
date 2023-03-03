set term png
set output "fig1-euler-caiguda-amb-friccio.png"

set xlabel "t (s)"
set ylabel "v(s)"


set key right bottom
plot "valors-euler.dat" index 0 using 1:2 with lines title "Euler-caiguda-amb-friccio"