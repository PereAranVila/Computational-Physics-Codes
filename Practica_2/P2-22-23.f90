
program practica_2

implicit none

real phi
real L, radi, w, t
integer i, counter
real x(6)
! variables apartat 6)
real TI(51), XI(51), aux(51)
! variables apartat 7)
real xlin, x0
real xinterpol, xinterpol0

! common blocks
common/posis/TI,XI

! constants del nostre problema
L = 22.5
w = 5.


! APARTAT 3)
open(1,file='P2-22-23-res1-b.dat')
do counter = 0,50
	t = real(counter)/10.
	call posiciones(L,w,t,x)
	write(1,*)t, x(1), x(2), x(3), x(4), x(5), x(6)
end do
close(1)


! APARTAT 6)
open(2,file='P2-22-23-res1-b.dat')
do counter=1,51
	read(2,*)TI(counter), aux(1), aux(1), XI(counter), aux(1), aux(1), aux(1)
end do
close(2)



! APARTAT 7)
open(3,file='P2-22-23-res2-b.dat')
do counter=0,1500
	t = real(counter)/500.
	xlin = xinterpol(t)
	x0 = xinterpol0(t)
	write(3,*)t, xlin, x0
end do
close(3)

end program practica_2


real function phi(i)

	! Calcula la fase del pisto
	! Args**: i: numero del pisto

	implicit none

	integer i

	phi = real(i)*4.*atan(1.)/6.

end function phi


subroutine radius(i,L,radi)
	
	! calcula el radi del pisto donat
	! Args**: i: numero del pisto, L: longitud del pisto, radi: es el valor resultant

	implicit none

	real radi, L
	integer i

	radi = L/real(i) -0.01
	return
end subroutine radius


subroutine posiciones(L,w,t,x)

	! calcula les posicions dels pistons

	implicit none

	real L, w, t
	real Ri, phi
	real x(6)
	integer i

	! calcul de la posicio del pisto per L,w i t concrets
	do i=1,6
		Ri = L/real(i) -0.01
		phi = i*4.*atan(1.)/6.

		x(i) = Ri*cos(w*t+phi) + (L**2-Ri**2*(sin(w*t+phi))**2)**0.5

	end do
	return
end subroutine posiciones


real function xinterpol(tin)
	
	! calcula el valor de la interpolacio lineal de les dades TI i XI
	! al punt tin. El metode seguit es: vaig començo des dels valors dels
	! extrems i miro si el "tin" esta mes aprop del maxim o del minim,
	! d'aquesta forma el interval se'm va dividint per 2 per cada iteracio,
	! posteriorment un cop tinc els 2 valors els quals al mig hi ha "tin",
	! faig la interpolacio lineal.

	! Args**: tin: temps el qual es vol calcular la interpolacio
	!			xout: valor de la interpolacio a tin


	! Els unics parametres que s'ha de modificar d'aquesta subrutina son
	! els marcats amb "! parametres A MODIFICAR".
	! params**: len_tmax: numero de valors de la variable independet +1
	!			len_tmin: es sempre 1 (no cal modificarse)
	!			tambe hauras de modificar les dimensions del "temps" i "posis" 
	!			per tal que s'ajusti al teu problema.




	implicit none
	integer len_tmax, len_tmin
	real TI(51), XI(51)
	real val_t1, val_t2, x_t1, x_t2, m, xout, tin

	common/posis/TI,XI

	! parametres a modificar (segons el problema)
	len_tmax = 51
	len_tmin = 1

	! busquem els 2 temps en que tin esta al seu interval
	do while (real(len_tmax-len_tmin).GT.(1.2))
		if (abs(TI(len_tmax) - tin).LT.abs(tin-TI(len_tmin))) then
			len_tmin = len_tmin + int((len_tmax-len_tmin)/2.)
		else
			len_tmax = len_tmax - int((len_tmax-len_tmin)/2.)
		end if
	end do

	! calculem el valor xout fent una recta de regressio
	val_t1 = TI(len_tmin)
	val_t2 = TI(len_tmax)

	x_t1 = XI(len_tmin)
	x_t2 = XI(len_tmax)

	! m es el pendent de l'ajust d'interpolacio lineal
	m = (x_t2 - x_t1)/(val_t2 - val_t1)

	xinterpol = x_t1 + m*(tin-val_t1)

	return
end function xinterpol



real function xinterpol0(tin)
	
	! calcula la interpolacio d'ordre 0. El valor que he escollit ha sigut la mitjana entre els
	! dos extrems.

	implicit none
	integer len_tmax, len_tmin
	real TI(51), XI(51)
	real x_t1, x_t2, tin

	common/posis/TI,XI

	! parametres a modificar (segons el problema)
	len_tmax = 51
	len_tmin = 1

	! busquem els 2 temps en que tin esta al seu interval
	do while (real(len_tmax-len_tmin).GT.(1.2))
		if (abs(TI(len_tmax) - tin).LT.abs(tin-TI(len_tmin))) then
			len_tmin = len_tmin + int((len_tmax-len_tmin)/2.)
		else
			len_tmax = len_tmax - int((len_tmax-len_tmin)/2.)
		end if
	end do

	! calculem el valor xout agafant el valor mig de xk i xk+1
	x_t1 = XI(len_tmin)
	x_t2 = XI(len_tmax)

	xinterpol0 = (x_t1+x_t2)/2.

	return
end function xinterpol0


