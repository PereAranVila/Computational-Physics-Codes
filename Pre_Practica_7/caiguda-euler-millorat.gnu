set term png
set output "euler-millorat-caiguda-amb-friccio.png"

set xlabel "t (s)"
set ylabel "v(s)"


set key right bottom
plot "valors-euler-millorat.dat" index 0 using 1:2 with lines title "Euler millorat caiguda amb friccio"