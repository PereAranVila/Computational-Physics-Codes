set term png
set output "P8-22-23-b1-fig2.png"

set xlabel "n (nombre d'iteracions)"
set ylabel "E (eV)"


set key right bottom
plot "convergencia-ex-2.dat" index 0 using 1:2 title "1r VAP", "convergencia-ex-2.dat" index 1 using 1:2 title "2n VAP" , "convergencia-ex-2.dat" index 2 using 1:2 title "3r VAP"