set term png
set output "P7-22-23-fig1.png"

set xlabel "t (s)"
set ylabel "dphi/dt (rad/s)"



set key right top
plot "P7-22-23-res.dat" index 2 using 1:3 with lines title "Euler", "P7-22-23-res.dat" index 3 using 1:3 with lines title "Euler millorat"