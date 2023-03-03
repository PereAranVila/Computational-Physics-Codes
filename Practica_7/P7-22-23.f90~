
program practica7

implicit none

! funcions
double precision eq_w_aprox, eq_phi, Ecine, Epoten


double precision t0,phi0,dphi0,phi,w,t
double precision delta_t
double precision l, g, TN, wN
integer h, n
double precision Et, Ec, Ep
! variables per RK2
double precision k1, k2



! common blocks
common/constants/wN

! constants del problema
l = 1.07d0
g = 10.44d0
wN = sqrt(g/l)
TN = (2.d0*4.d0*atan(1.d0))/wN

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! APARTAT A) PETITES OSCILACIONS

! constants inicials del problema (Euler)
t0 = 0.d0
phi0 = 0.025d0
dphi0 = 0.d0

! passos de temps
h = 1500
delta_t = (7.d0*TN-t0)/dble(h)


! guardem totes les dades en un fitxer
open(1,file='P7-22-23-res.dat')

! Metode de Euler
do n=1,h
	w = dphi0 + delta_t*eq_w_aprox(phi0)
	phi = phi0 + delta_t*eq_phi(w)
	t = t0 + n*delta_t
	write(1,*)t, phi, w

	dphi0 = w
	phi0 = phi
end do

write(1,*)
write(1,*)

! constants inicials del problema (RK2)
t0 = 0.d0
phi0 = 0.025d0
dphi0 = 0.d0

! NOTA*: No m'en he ensortit alhora de programar el metode RK2. He contnuitat fent
! la practica perque sino em quedava aqui.

! Metode RK2
do n=1,h
	k1 = eq_w_aprox(dphi0)

	! Euler per calcular k2
	k2 = eq_w_aprox(dphi0) + delta_t*(g/l)

	w = dphi0 + (delta_t/2.d0)*(k1+k2) 

	k1 = eq_phi(phi0)
	! Euler per calcular k2
	k2 = eq_phi(phi0) + delta_t*eq_phi(w)
	phi = phi0 + (delta_t/2.d0)*(k1+k2)
	
	write(1,*) t, phi, w

	phi0 = phi
	dphi0 = w
end do

write(1,*)
write(1,*)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! APARTAT B) OSCILACIONS GRANS

! constants inicials del problema
t0 = 0.d0
phi0 = 4.d0*atan(1.d0) -0.15d0
dphi0 = 0.d0

! passos de temps
h = 1500
delta_t = (7.d0*TN-t0)/dble(h)

! Metode de Euler
do n=1,h
	w = dphi0 + delta_t*eq_w_aprox(phi0)
	phi = phi0 + delta_t*eq_phi(w)
	t = t0 + n*delta_t
	write(1,*)t, phi, w

	dphi0 = w
	phi0 = phi
end do

write(1,*)
write(1,*)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! APARTAT C) ENERGIA

! constants inicials del problema
t0 = 0.d0
phi0 = 4.d0*atan(1.d0) -0.025d0
dphi0 = 0.12d0

! passos de temps
h = 1500
delta_t = (7.d0*TN-t0)/dble(h)

! Metode de Euler
do n=1,h
	w = dphi0 + delta_t*eq_w_aprox(phi0)
	phi = phi0 + delta_t*eq_phi(w)
	t = t0 + n*delta_t
	Ep = Epoten(t,phi)
	Ec = Ecine(t,w)
	Et = Ep + Ec
	write(1,*)t, phi, w, Et, Ec, Ep

	dphi0 = w
	phi0 = phi
end do

write(1,*)
write(1,*)

! NOTA*: No entenc com la Energia total no es conserva, entenc que hi deu
! haver-hi algun problema fent Euler pero es que la veritat no he vist
! res. 

end program practica7



double precision function eq_w_aprox(phi)

	implicit none
	double precision WN, phi

	common/constants/wN

	eq_w_aprox = -wN**2 * phi
	return
end function eq_w_aprox

double precision function eq_phi(w)

	implicit none
	double precision w

	eq_phi = w
	return
end function eq_phi


double precision function Ecine(t,dphi)

	implicit none
	double precision t, dphi
	double precision m,l

	! constants del problema
	m = 0.98d0
	l = 1.07d0

	Ecine = (1.d0/2.d0)*m*(dphi**2)*l**2
	return
end function Ecine

double precision function Epoten(t,phi)

	implicit none
	double precision t, phi
	double precision m,g,l

	! constants del problema
	m = 0.98d0
	g = 10.44d0
	l = 1.07d0

	Epoten = -m*g*l*cos(phi)
	return
end function Epoten


