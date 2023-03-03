
program pre_practica_2

implicit none

double precision w0, L, val, radiT1, wk, t
double precision x(4)
integer k, ndat

double precision rk2, wk2
! variables apartat 3)
integer counter
! variables apartat 6)
double precision temps(400), posis(400), basura(400)
! variables apartat 7)
double precision tin, xout



external radiT1


! constants del problema
w0 = 4.3d0
L = 25.d0



! proves de la funcio radiT1(L,k) i wk(k,w0)
k = 2
rk2 = radiT1(L,k)
write(*,*)"El valor del radi per k=2 es: ", rk2



! EXERCICI 2)
x = [0.d0,0.d0,0.d0,0.d0]
t = 0.d0

call posiT1(w0,L,t,x)
write(*,*)"Les posicions dels 4 pistons a t=0 son: ", x


! EXERCICI 3)
open(1,file='P2-18P-res1.dat')
do counter=0,400
	t = real(counter)/100.d0
	call posiT1(w0,L,t,x)
	write(1,*) t,x(1),x(2),x(3),x(4)
end do
close(1)


! EXERCICI 4)
! important, quan hagis de llegir un fitxer amb columnes ho has de fer
! d'aquesta manera
open(1,file='P2-18P-res1.dat')
do counter=1,400
	read(1,*) temps(counter),basura(1),basura(1),posis(counter),basura(1)
end do
close(1)




! EXERCICI 7)
ndat = 2000
open(2,file='P2-18P-res2.dat')
do counter=1,2000
	tin = (3.d0*real(counter))/2000.d0
	call interpol(tin, xout, temps, posis, ndat)
	write(2,*) tin, xout
end do
close(2)



end program pre_practica_2


double precision function radiT1(L,k)
	
	! calcula el radi de la manovella R_k de cada pisto
	! Args**: L: longitud de les bieles, k: numero del pisto

	implicit none
	integer k
	double precision L

	radiT1 = L -0.1d0 -0.3d0*(real(k) -1.d0)
	return
end function radiT1


subroutine posiT1(w0,L,t,x)

	! calcula les posicions dels 4 pistons en funcio del temps
	! Args**: w0: frequencia incial, L: longitud de les bieles, t: temps,
	!			x: les posicions de cada un dels pistons

	implicit none
	double precision w0, t, L
	double precision x(4)
	double precision radiT1, wk
	integer k


	do k=1,4
		wk = w0*(dble(k)/4.d0 +1.d0)
		x(k) = radiT1(L,k)*cos(wk*t) &
		+ sqrt(L**2 -(radiT1(L,k)**2 *(sin(wk*t))**2))
	end do
	return
end subroutine posiT1


subroutine interpol(xin,yout,x_data,y_data,ndat)
	
	! calcula el valor de la interpolacio lineal de les dades X i Y
	! al punt tin. 
	! El metode seguit es: vaig començo des dels valors dels
	! extrems i miro si el "tin" esta mes aprop del maxim o del minim,
	! d'aquesta forma el interval se'm va dividint per 2 per cada iteracio,
	! posteriorment un cop tinc els 2 valors els quals al mig hi ha "tin",
	! faig la interpolacio lineal.

	! Args**: 	xin: variable independent la qual es vol torbar el valor y
	!					mitjançant interpolacio.
	!
	!			xout: valor calculat de la intepolacio per a un valor de la
	!					variable independent xin.
	!
	!			x_data: vector de valors de la variable independent, a partir
	!						del qual podrem calcular la interpolacio.
	!
	!			y_data: vector de valors de la variable dependent, a partir
	!						del qual ens servira per calcular la interpolacio.
	!
	!			ndat: nombre total de valors que tenim als vectors x_data i
	!					n_data.


	implicit none
	integer ndat
	integer len_tmax, len_tmin
	double precision x_data(ndat), y_data(ndat)
	double precision val_x1, val_x2, y_x1, y_x2, m, yout, xin


	! parametres a modificar (segons el problema)
	len_tmax = ndat
	len_tmin = 1

	! busquem els 2 temps en que tin esta al seu interval
	do while (dble(len_tmax-len_tmin).GT.(1.2d0))
		if (abs(x_data(len_tmax) - xin).LT.abs(xin-x_data(len_tmin))) then
			len_tmin = len_tmin + int((len_tmax-len_tmin)/2.d0)
		else
			len_tmax = len_tmax - int((len_tmax-len_tmin)/2.d0)
		end if
	end do

	! calculem el valor xout fent una recta de regressio
	val_x1 = x_data(len_tmin)
	val_x2 = x_data(len_tmax)

	y_x1 = y_data(len_tmin)
	y_x2 = y_data(len_tmax)

	! m es el pendent de l'ajust d'interpolacio lineal
	m = (y_x2 - y_x1)/(val_x2 - val_x1)

	yout = y_x1 + m*(xin-val_x1)

	return
end subroutine interpol

