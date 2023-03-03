
program prepractica7

implicit none

! Per els metodes de Euler i Euler millorat es impossible fer una subrutina
! generalistat que resolgui qualsevol edo.

! variables apartat a)
double precision eq_phi, eq_w_aprox, eq_w, Ecine, Epoten
double precision t0,t,delta_t,phi0,dphi0,w,phi,w_n1,phi_n1,t_n1
double precision g,l,TN,wN
integer n,h
double precision K, V, E_tot

! common blocks
common/constants/wN

! constants del problema
l = 0.45d0
g = 1.62d0
wN = sqrt(g/l)
TN = (2.d0*4.d0*atan(1.d0))/wN

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! PROBLEMA A) PETITES OSCILACIONS

! constants inicials del problema
t0 = 0.d0
phi0 = 0.03d0
dphi0 = 0.d0

! passos de temps
h = 1300
delta_t = (6.d0*TN-t0)/dble(h)


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

! Metode de Euler millorat
! constants inicials del problema
t0 = 0.d0
phi0 = 0.03d0
dphi0 = 0.d0

do n=1,h
	if (n.eq.1) then
		! inicialitzem amb Euler normal
		w = dphi0 + delta_t*eq_w_aprox(phi0)
		phi = phi0 + delta_t*eq_phi(w)
		t = t0 + n*delta_t
		write(1,*)t, phi, w
	end if

	! Euler millorat
	if (n.GT.1) then
		w_n1 = dphi0 + 2.d0*delta_t*eq_w_aprox(phi)
		phi_n1 = phi0 + 2.d0*delta_t*eq_phi(w)
		t_n1 = t0 + n*delta_t
		write(1,*)t_n1, phi_n1, w_n1

		dphi0 = w
		phi0 = phi
		w = w_n1
		phi = phi_n1
	end if
end do

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! PROBLEMA B) GRANS OSCILACIONS

! constants inicials del problema
t0 = 0.d0
phi0 = 4.d0*atan(1.d0) -0.03d0
dphi0 = 0.d0

! passos de temps
h = 1800
delta_t = (6.d0*TN-t0)/dble(h)

write(1,*)
write(1,*)

! Metode de Euler
do n=1,h
	w = dphi0 + delta_t*eq_w(phi0)
	phi = phi0 + delta_t*eq_phi(w)
	t = t0 + n*delta_t
	write(1,*)t, phi, w

	dphi0 = w
	phi0 = phi
end do

write(1,*)
write(1,*)

! Metode de Euler millorat
t0 = 0.d0
phi0 = 4.d0*atan(1.d0) -0.03d0
dphi0 = 0.d0

do n=1,h
	if (n.eq.1) then
		! inicialitzem amb Euler normal
		w = dphi0 + delta_t*eq_w(phi0)
		phi = phi0 + delta_t*eq_phi(w)
		t = t0 + n*delta_t
		write(1,*)t, phi, w
	end if

	! Euler millorat
	if (n.GT.1) then
		w_n1 = dphi0 + 2.d0*delta_t*eq_w(phi)
		phi_n1 = phi0 + 2.d0*delta_t*eq_phi(w)
		t_n1 = t0 + n*delta_t
		write(1,*)t_n1, phi_n1, w_n1

		dphi0 = w
		phi0 = phi
		w = w_n1
		phi = phi_n1
	end if
end do

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! PROBLEMA C)ENERGIA

! Primer problema
! constants inicials del problema (PRIMER CAS) (phi(0)=1))
t0 = 0.d0
phi0 = 1.d0
dphi0 = 0.d0

! passos de temps
h = 2500
delta_t = (6.d0*TN-t0)/dble(h)

write(1,*)
write(1,*)

! Metode de Euler
do n=1,h
	w = dphi0 + delta_t*eq_w(phi0)
	phi = phi0 + delta_t*eq_phi(w)
	t = t0 + n*delta_t
	K = Ecine(t,w)
	V = Epoten(t,phi)
	E_tot = K + V
	write(1,*)t, phi, w, K, V, E_tot

	dphi0 = w
	phi0 = phi
end do

write(1,*)
write(1,*)

! Metode de Euler millorat
t0 = 0.d0
phi0 = 1.d0
dphi0 = 0.d0

do n=1,h
	if (n.eq.1) then
		! inicialitzem amb Euler normal
		w = dphi0 + delta_t*eq_w(phi0)
		phi = phi0 + delta_t*eq_phi(w)
		t = t0 + n*delta_t
		K = Ecine(t,w)
		V = Epoten(t,phi)
		E_tot = K + V
		write(1,*)t, phi, w, K, V, E_tot
	end if

	! Euler millorat
	if (n.GT.1) then
		w_n1 = dphi0 + 2.d0*delta_t*eq_w(phi)
		phi_n1 = phi0 + 2.d0*delta_t*eq_phi(w)
		t_n1 = t0 + n*delta_t
		K = Ecine(t,w_n1)
		V = Epoten(t,phi_n1)
		E_tot = K + V
		write(1,*)t_n1, phi_n1, w_n1, K, V, E_tot

		dphi0 = w
		phi0 = phi
		w = w_n1
		phi = phi_n1
	end if
end do


! Segon problema
! constants inicials del problema (SEGONS CAS) (phi(0)=1))
t0 = 0.d0
phi0 = 4.d0*atan(1.d0)-0.035d0
dphi0 = 0.d0

! passos de temps
h = 2500
delta_t = (6.d0*TN-t0)/dble(h)

write(1,*)
write(1,*)

! Metode de Euler
do n=1,h
	w = dphi0 + delta_t*eq_w(phi0)
	phi = phi0 + delta_t*eq_phi(w)
	t = t0 + n*delta_t
	K = Ecine(t,w)
	V = Epoten(t,phi)
	write(1,*)t, phi, w, K, V

	dphi0 = w
	phi0 = phi
end do

write(1,*)
write(1,*)

! Metode de Euler millorat
t0 = 0.d0
phi0 = 1.d0
dphi0 = 0.d0

do n=1,h
	if (n.eq.1) then
		! inicialitzem amb Euler normal
		w = dphi0 + delta_t*eq_w(phi0)
		phi = phi0 + delta_t*eq_phi(w)
		t = t0 + n*delta_t
		K = Ecine(t,w)
		V = Epoten(t,phi)
		write(1,*)t, phi, w, K, V
	end if

	! Euler millorat
	if (n.GT.1) then
		w_n1 = dphi0 + 2.d0*delta_t*eq_w(phi)
		phi_n1 = phi0 + 2.d0*delta_t*eq_phi(w)
		t_n1 = t0 + n*delta_t
		K = Ecine(t,w_n1)
		V = Epoten(t,phi_n1)
		write(1,*)t_n1, phi_n1, w_n1, K, V

		dphi0 = w
		phi0 = phi
		w = w_n1
		phi = phi_n1
	end if
end do

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! PROBLEMA D) TRANSICIO

! Branca el qual l'error es la part positiva (+0.05 rad)
! constants inicials del problema
t0 = 0.d0
phi0 = 0.d0
dphi0 = 2.d0*sqrt(g/l) + 0.05d0

! passos de temps
h = 2100
delta_t = (7.d0*TN-t0)/dble(h)

write(1,*)
write(1,*)

! Nomes ens demana que ho calculem amb Euler millorat
do n=1,h
	if (n.eq.1) then
		! inicialitzem amb Euler normal
		w = dphi0 + delta_t*eq_w(phi0)
		phi = phi0 + delta_t*eq_phi(w)
		t = t0 + n*delta_t
		write(1,*)t, phi, w
	end if

	! Euler millorat
	if (n.GT.1) then
		w_n1 = dphi0 + 2.d0*delta_t*eq_w(phi)
		phi_n1 = phi0 + 2.d0*delta_t*eq_phi(w)
		t_n1 = t0 + n*delta_t
		write(1,*)t_n1, phi_n1, w_n1

		dphi0 = w
		phi0 = phi
		w = w_n1
		phi = phi_n1
	end if
end do


! Branca el qual l'error es la part negativa (-0.05 rad)
! constants inicials del problema
t0 = 0.d0
phi0 = 0.d0
dphi0 = 2.d0*sqrt(g/l) - 0.05d0

! passos de temps
h = 2100
delta_t = (7.d0*TN-t0)/dble(h)

write(1,*)
write(1,*)

! Nomes ens demana que ho calculem amb Euler millorat
do n=1,h
	if (n.eq.1) then
		! inicialitzem amb Euler normal
		w = dphi0 + delta_t*eq_w(phi0)
		phi = phi0 + delta_t*eq_phi(w)
		t = t0 + n*delta_t
		write(1,*)t, phi, w
	end if

	! Euler millorat
	if (n.GT.1) then
		w_n1 = dphi0 + 2.d0*delta_t*eq_w(phi)
		phi_n1 = phi0 + 2.d0*delta_t*eq_phi(w)
		t_n1 = t0 + n*delta_t
		write(1,*)t_n1, phi_n1, w_n1

		dphi0 = w
		phi0 = phi
		w = w_n1
		phi = phi_n1
	end if
end do

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! PROBLEMA E) CONVERGENCIA DEL MEOTODE

! per passos = 400
! constants inicials del problema
t0 = 0.d0
phi0 = 2.1d0
dphi0 = 0.1d0
! passos de temps
h = 400
delta_t = (12.d0*TN-t0)/dble(h)

write(1,*)
write(1,*)


do n=1,h
	if (n.eq.1) then
		! inicialitzem amb Euler normal
		w = dphi0 + delta_t*eq_w(phi0)
		phi = phi0 + delta_t*eq_phi(w)
		t = t0 + n*delta_t
		K = Ecine(t,w_n1)
		V = Epoten(t,phi_n1)
		E_tot = K + V
		write(1,*)t, phi, w, E_tot
	end if

	! Euler millorat
	if (n.GT.1) then
		w_n1 = dphi0 + 2.d0*delta_t*eq_w(phi)
		phi_n1 = phi0 + 2.d0*delta_t*eq_phi(w)
		t_n1 = t0 + n*delta_t
		K = Ecine(t,w_n1)
		V = Epoten(t,phi_n1)
		E_tot = K + V
		write(1,*)t_n1, phi_n1, w_n1, E_tot

		dphi0 = w
		phi0 = phi
		w = w_n1
		phi = phi_n1
	end if
end do

! per passos = 1100
! constants inicials del problema
t0 = 0.d0
phi0 = 2.1d0
dphi0 = 0.1d0

! passos de temps
h = 1100
delta_t = (12.d0*TN-t0)/dble(h)

write(1,*)
write(1,*)


do n=1,h
	if (n.eq.1) then
		! inicialitzem amb Euler normal
		w = dphi0 + delta_t*eq_w(phi0)
		phi = phi0 + delta_t*eq_phi(w)
		t = t0 + n*delta_t
		K = Ecine(t,w_n1)
		V = Epoten(t,phi_n1)
		E_tot = K + V
		write(1,*)t, phi, w, E_tot
	end if

	! Euler millorat
	if (n.GT.1) then
		w_n1 = dphi0 + 2.d0*delta_t*eq_w(phi)
		phi_n1 = phi0 + 2.d0*delta_t*eq_phi(w)
		t_n1 = t0 + n*delta_t
		K = Ecine(t,w_n1)
		V = Epoten(t,phi_n1)
		E_tot = K + V
		write(1,*)t_n1, phi_n1, w_n1, E_tot

		dphi0 = w
		phi0 = phi
		w = w_n1
		phi = phi_n1
	end if
end do

! per passos = 2000
! constants inicials del problema
t0 = 0.d0
phi0 = 2.1d0
dphi0 = 0.1d0

! passos de temps
h = 2000
delta_t = (12.d0*TN-t0)/dble(h)

write(1,*)
write(1,*)


do n=1,h
	if (n.eq.1) then
		! inicialitzem amb Euler normal
		w = dphi0 + delta_t*eq_w(phi0)
		phi = phi0 + delta_t*eq_phi(w)
		t = t0 + n*delta_t
		K = Ecine(t,w_n1)
		V = Epoten(t,phi_n1)
		E_tot = K + V
		write(1,*)t, phi, w, E_tot
	end if

	! Euler millorat
	if (n.GT.1) then
		w_n1 = dphi0 + 2.d0*delta_t*eq_w(phi)
		phi_n1 = phi0 + 2.d0*delta_t*eq_phi(w)
		t_n1 = t0 + n*delta_t
		K = Ecine(t,w_n1)
		V = Epoten(t,phi_n1)
		E_tot = K + V
		write(1,*)t_n1, phi_n1, w_n1, E_tot

		dphi0 = w
		phi0 = phi
		w = w_n1
		phi = phi_n1
	end if
end do

! per passos = 16000
! constants inicials del problema
t0 = 0.d0
phi0 = 2.1d0
dphi0 = 0.1d0

! passos de temps
h = 16000
delta_t = (12.d0*TN-t0)/dble(h)

write(1,*)
write(1,*)


do n=1,h
	if (n.eq.1) then
		! inicialitzem amb Euler normal
		w = dphi0 + delta_t*eq_w(phi0)
		phi = phi0 + delta_t*eq_phi(w)
		t = t0 + n*delta_t
		K = Ecine(t,w_n1)
		V = Epoten(t,phi_n1)
		E_tot = K + V
		write(1,*)t, phi, w, E_tot
	end if

	! Euler millorat
	if (n.GT.1) then
		w_n1 = dphi0 + 2.d0*delta_t*eq_w(phi)
		phi_n1 = phi0 + 2.d0*delta_t*eq_phi(w)
		t_n1 = t0 + n*delta_t
		K = Ecine(t,w_n1)
		V = Epoten(t,phi_n1)
		E_tot = K + V
		write(1,*)t_n1, phi_n1, w_n1, E_tot

		dphi0 = w
		phi0 = phi
		w = w_n1
		phi = phi_n1
	end if
end do

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

close(1)
end program prepractica7


double precision function eq_w_aprox(phi)

	implicit none
	double precision WN, phi

	common/constants/wN

	eq_w_aprox = -wN**2 * phi
	return
end function eq_w_aprox

double precision function eq_w(phi)

	implicit none
	double precision WN, phi

	common/constants/wN

	eq_w = -wN**2 * sin(phi)
	return
end function eq_w

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
	m = 0.510d0
	l = 0.45d0

	Ecine = (1.d0/2.d0)*m*dphi**2*l**2
	return
end function Ecine

double precision function Epoten(t,phi)

	implicit none
	double precision t, phi
	double precision m,g,l

	! constants del problema
	m = 0.510d0
	g = 1.62d0
	l = 0.45d0

	Epoten = -m*g*l*cos(phi)
	return
end function Epoten

