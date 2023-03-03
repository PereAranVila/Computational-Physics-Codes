set term png
set output "P7-22-23-fig4.png"

set xlabel "t (s)"

set key right top


plot "P7-22-23-res.dat" index 4 using 1:4 with lines title "K (1r cas) Euler", "P7-22-23-res.dat" index 4 using 1:5 with lines title "V (1r cas) Euler", "P7-22-23-res.dat" index 5 using 1:4 with lines title "V (1r cas) Euler millorat", "P7-22-23-res.dat" index 5 using 1:5 with lines title "K (1r cas) Euler millorat", "P7-22-23-res.dat" index 6 using 1:4 with lines title "K (2n cas) Euler", "P7-22-23-res.dat" index 6 using 1:5 with lines title "V (2n cas) Euler", "P7-22-23-res.dat" index 7 using 1:4 with lines title "K (2n cas) Euler millorat", "P7-22-23-res.dat" index 7 using 1:5 with lines title "V (2n cas) Euler millorat"