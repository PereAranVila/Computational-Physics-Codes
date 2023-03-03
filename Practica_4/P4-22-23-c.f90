
program practica4

implicit none

double precision YKohoutek
double precision h_interval, At, As, x1,x2
integer m
double precision a,b

double precision Am, Tm1, Tm

external YKohoutek

common/const/a,b

! EXERCICI 1)

! constant exercici 1
a = 508.633*10**6
b = 429.074*10**6
x2 = (7.d0/2.d0)*b
x1 = 3.d0*b


! apartat a)
open(10,file='P4-22-23-res.dat')
do m = 2,20
	h_interval = abs(x1-x2)/2.d0**m
	call trapezoids(x1,x2,m,YKohoutek,At)
	call simpson(x1,x2,m,YKohoutek,As)
	write(10,'(3e21.14)') h_interval, At, As
end do

	

! apartat c)
write(10,*)
write(10,*)

do m=2,19
	h_interval = abs(x1-x2)/2.d0**m
	call trapezoids(x1,x2,m+1,YKohoutek,Tm1)
	call simpson(x1,x2,m,YKohoutek,Tm)

	Am = (4.d0*Tm1 - Tm)/3.d0
	write(10,'(2e21.14)')h_interval, Am
end do
close(10)

! Observacions dels resultats:
! veiem com per el mètode de Simpson convergeix mes rapidament que el
! metode dels Trapezis i el mètode d'aproximacions superiors dels Trapezis.

end program practica4


double precision function YKohoutek(x)

	implicit none
	double precision x
	double precision a,b

	common/const/a,b

	YKohoutek = a*sqrt(1.d0-(((x-3.d0*b)**2)/(b**2)))
	return
end function YKohoutek


subroutine trapezoids(x1,x2,k,funci,integral)

	! Calcula la integral trapezoidal entre 2 valors x1 i x2 de una funcio
	! Args**:	x1: valor inferior de la integral (type: dble)
	!			x2: valor superior de la integral (type: dble)
	!			k: valor que indica el nombre de intervals, el nombre de
	!				intervals es doncs de 2**k intervals  (type: int)
	!			funci: funcio a integrar (passada com a external)
	!					type(function)
	!			integral:	valor de la integral resultant (type: dble)


	implicit none

	double precision x1,x2,integral, funci
	integer k

	double precision h,f0,f1,x0_1,x1_1
	integer count

	h = abs(x2-x1)/2.d0**k 		! h es l'espeiat entre dos valors consecutius
	integral = 0.d0


	do count=0,(2**k)-1		! el -1 es degut al valor extrem del interval
		! punt x0 i x1 el qual evaluarem la funcio
		x0_1 = x1 + (abs(x1-x2)*count)/2.d0**k
		x1_1 = x1 + (abs(x1-x2)*(count+1)/2.d0**k)

		! valors de les funcions
		f0 = funci(x0_1)
		f1 = funci(x1_1)

		integral = integral + (h/2.d0)*(f0+f1)
	end do
	return
end subroutine trapezoids

subroutine simpson(x1,x2,k,funci,integral)

	! calcula la integral entre x1 i x2 de una funcio generica utilitzant el
	! metode de Simpson
	! Args**:	x1: valor inferior de la integral (type: dble)
	!			x2: valor superior de la integral (type: dble)
	!			k: valor que indica el nombre de intervals, el nombre de
	!				intervals es doncs de 2**k intervals  (type: int)
	!			funci: funcio a intergrar (passada com a external)
	!					type(function)
	!			integral:	valor de la integral resultant (type: dble)

	implicit none

	double precision x1,x2,funci,integral
	integer k

	double precision h,f0,f1,f2,x0_1,x1_1,x2_1
	integer count

	h = abs(x2-x1)/2.d0**k 		! h es l'espeiat entre dos valors consecutius
	integral = 0.d0


	do count=0,(2**k)-1		! el -1 es degut al valor extrem del interval
		! punt x0, x1 i x2 el qual evaluarem la funcio
		x0_1 = x1 + (abs(x1-x2)*count)/2.d0**k
		x1_1 = x1 + (abs(x1-x2)*(count+0.5)/2.d0**k)
		x2_1 = x1 + (abs(x1-x2)*(count+1)/2.d0**k)

		! valors de les funcions
		f0 = funci(x0_1)
		f1 = funci(x1_1)
		f2 = funci(x2_1)

		integral = integral + (h/6.d0)*(f0+4.d0*f1+f2)
	end do
	return	
end subroutine simpson




