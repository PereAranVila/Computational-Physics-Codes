
program prepractica4

implicit none

! variables exercici 0)
double precision xk1, xk
integer k, h

! variables exercici 1)
double precision x1,x2,integral,funci,longitud,f2,f3

! variables exercici 3)
double precision results_trap, results_simps,h_interval

external funci
external longitud
external f2
external f3





! EXERCICI 0)
! Com Fortran no em deixar calcular unes dades, guardar-les i al cap de un bloc
! de codi escriure una altre columna, la unica soluccio es fer-ho amb vectors
! on s'emmagatzem les dades.

! a) per "taula" entenc que s'ha d'escriure en un fitxer
open(1,file='preprac4-taula-apartat0-a.dat')
xk = 0.d0

do k=0,200000000
	if (k.eq.0) then	! cas k=0
		xk1 = 0.d0
		write(1,'(e14.8)') xk1
		!xka(k+1) = xk1

	else	! cas k != 0
		xk1 = xk + 0.02
		if (mod(k,100000).eq.0) then
			write(1,'(e14.8)')xk1
			!xka(k+1) = xk1
		end if
		xk = xk1
	end if
end do
close(1)


! b) taula amb xk=2*k*h i k =[0,2000] int
open(2,file='preprac4-taula-apartat0-b.dat')
h = 1000
do k=0,2000
	xk1 = 2.d0*k*h
	write(2,'(e14.8)')xk1
	!xkb(k+1) = xk1
end do
close(2)


! guardo les dades del apartat a) i b)
! NO HO HE POGUT FER JA QUE DONAVA ERROS EN EL ESPAI DE LA MEMORIA
!open(125,file='P4-22-23-res1.dat')
!do k=1,2001
!	write(125,'(e14.8)')xka(k), xkb(k)
!end do
!close(125)


! s'observa com els valors son els mateixos l'unic que varia es l'ultima xifra
! el motiu exacte per el qual ocorre aixo no ho sabria dir pero crec ques es
! perque en el primer anem fent sumes de coses que vallen en la ultima i en
! canvi en el cas b) es una unica expressio i no recorrem de les altres.
! Realment, el perque no el sabria dir (0 no el veig ara mateix)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!





! EXERCICI 1) OK FUNCIONA
! provem la subroutina trapezoids amb una funcio polinomial que m'he inventat
! jo per testejar-la
x1 = 0.d0
x2 = 2.d0
k = 12

call trapezoids(x1,x2,k,funci,integral)
write(*,*)"Trapezoids (polinomi meu): ", integral

! provem la subroutina simpson amb una funcio polinomial
x1 = 0.d0
x2 = 2.d0
k = 12

call simpson(x1,x2,k,funci,integral)
write(*,*)"Simpson (polinomi meu): ", integral

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!





! EXERCICI 2)
open(33,file='P4-22-23-res1.dat')
! funcio a) (longitud)
x1 = -4.d0*atan(1.d0)
x2 = 4.d0*atan(1.d0)
k = 18

call trapezoids(x1,x2,k,longitud,integral)
write(*,*)"Trapezoids (longitud) : ", integral
write(33,'(e14.8)')integral
call simpson(x1,x2,k,longitud,integral)
write(*,*)"Simpson (longitud) : ", integral
write(33,'(e14.8)')integral


! funcio b) (f2(x))
x1 = -43.52/(2.d0*1000.d0)
x2 = 43.52/(2.d0*1000.d0)
k = 18

call trapezoids(x1,x2,k,f2,integral)
write(*,*)"Trapezoids (f2) : ", integral
write(33,'(e14.8)')integral
call simpson(x1,x2,k,f2,integral)
write(*,*)"Simpson (f2) : ", integral
write(33,'(e14.8)')integral
close(33)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




! EXERCICI 3)
! NOTA IMPORTANT**: Per aquest apartat, al haber de calcular per valors de k
! que arriben fins a k=22 (és a dir, 2**22) intervals, el meu HP Notebook
! triga aproximadament 7 min, el PC de torre ho fa en 5s.

! convergencia apartat a)
x1 = -4.d0*atan(1.d0)	
x2 = 4.d0*atan(1.d0)

open(3,file='P4-22-23-res2.dat')
do k=4,22
	h_interval = abs(x1-x2)/2.d0**k
	call trapezoids(x1,x2,k,longitud,results_trap)
	call simpson(x1,x2,k,longitud,results_simps)
	write(3,*)h_interval, results_trap, results_simps
end do
close(3)


! convergencia apartat b)
x1 = -43.52/(2.d0*1000.d0)
x2 = 43.52/(2.d0*1000.d0)

open(4,file='P4-22-23-res3.dat')
do k=4,22
	h_interval = abs(x1-x2)/2.d0**k
	call trapezoids(x1,x2,k,f2,results_trap)
	call simpson(x1,x2,k,f2,results_simps)
	write(4,*)h_interval, results_trap, results_simps
end do
close(4)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!





! EXERCICI 4)
x1 = (3.d0/2.d0)*4.d0*atan(1.d0)
x2 = 4.d0*atan(1.d0)/2.d0

open(5,file='P4-22-23-res4.dat')
do k=6,20,2
	h_interval = abs(x1-x2)/2.d0**k
	call trapezoids(x1,x2,k,f3,results_trap)
	call simpson(x1,x2,k,f3,results_simps)
	write(5,*)h_interval, results_trap, results_simps
end do
close(5)



end program prepractica4


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

double precision function funci(x)
	
	implicit none

	double precision x

	funci = x**2 +2.d0
	return
end function funci

double precision function longitud(x)

	implicit none

	double precision x,A0
	double precision e,pi

	! constants del problema
	A0 = 0.35

	! valor comuns
	e = exp(1.d0)
	pi = 4.d0*atan(1.d0)


	longitud = A0*((cos(x-2)*e**(-x**2-sin(x)))**2 * (pi-x)**0.5)
	return
end function longitud

double precision function f2(x)

	implicit none

	double precision x,rho0,L

	! constants del problema
	rho0 = 0.72
	L = 43.52/(2.d0*1000.d0)

	f2 = rho0*(1-(x/L)**2)**0.5 * (1-(x/L))*((x/L)**2 + (x/L) +1)
	return
end function f2

double precision function f3(t)

	implicit none

	double precision t,L,rho0
	double precision x

	! constants del problema
	L = 43.52/(2.d0*1000.d0)
	rho0 = 0.72

	! canvi de variable i recordem que dx=Lcos(t)dt
	x = L*sin(t)

	f3 = rho0*(1-(x/L)**2)**0.5 * (1-(x/L)) * ((x/L)**2 +(x/L)+1) * L*cos(t)
	return
end function f3







