set term png
set output "P7-22-23-fig3.png"

set xlabel "t (s)"
set ylabel "dphi/dt (rad/s)"



set key right top
plot "P7-22-23-res.dat" index 3 using 1:2 with lines title "Phi Euler millorat", "P7-22-23-res.dat" index 3 using 1:3 with lines title "dphi/dt Euler millorat", "P7-22-23-res.dat" index 2 using 1:2 with lines title "Phi Euler", "P7-22-23-res.dat" index 2 using 1:3 with lines title "dphi/dt Euler"