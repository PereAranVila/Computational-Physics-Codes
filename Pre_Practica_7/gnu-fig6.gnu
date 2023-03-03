set term png
set output "P7-22-23-fig6.png"

set xlabel "t (s)"


set key right top
plot "P7-22-23-res.dat" index 10 using 1:4 with lines title "E_total (400)", "P7-22-23-res.dat" index 11 using 1:4 with lines title "E_total (1100)", "P7-22-23-res.dat" index 12 using 1:4 with lines title "E_total (2000)", "P7-22-23-res.dat" index 13 using 1:4 with lines title "E_total (16000)"