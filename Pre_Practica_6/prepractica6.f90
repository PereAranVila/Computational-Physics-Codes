
program prepractica6

implicit none

double precision I1, I2, f, g, I1_imp, g_inv, f1
integer N
double precision a,b,I,desvest
logical seed


! algunes variables extra pel Montecarlo d'Importancia
double precision xnums(40000)


external I1, I2, f, g, I1_imp, g_inv, f1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! INTEGRALS DE MONTECARLRO CRU 1D

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Integral 1 ("I1" del guio per comprobar que funciona montecarlo cru)
a = -4.d0*atan(1.d0)
b = 4.d0*atan(1.d0)
N = 20000
seed = .true.

call montecarlo_cru(N,a,b,I1,I,desvest,seed)

write(*,*)"I1 = ", I, 'error = ', desvest
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Integral 2 ("I2" del guio per comprobar que funciona montecarlo cru)
a = -4.d0*atan(1.d0)
b = 2.d0*4.d0*atan(1.d0)
N = 200000
seed = .true.

call montecarlo_cru(N,a,b,I2,I,desvest,seed)

write(*,*)"I2 = ", I, 'error = ', desvest
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Integral senzilla que sabem el seu valor. f = x**2. TOT OK!
a = 0.d0
b = 1.d0
N = 20000
seed = .false.

call montecarlo_cru(N,a,b,f,I,desvest,seed)

write(*,*)"Integral f= ", I, 'error = ', desvest

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! MONTECARLO D'IMPORTANCIA

write(*,*)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

a = 0.d0
b = 1.d0
N = 400000
call montecarlo_importancia(f,g,g_inv,a,b,N,I,desvest)
write(*,*)"I (montecarlo importancia, f=x**2)  = ", I, "error = ", desvest


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
a = 0.d0
b = 1.d0
N = 400000
call montecarlo_importancia(f1,g,g_inv,a,b,N,I,desvest)
write(*,*)"I (montecarlo importancia, f=x**2)  = ", I, "error = ", desvest




end program prepractica6

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! SUBRUTINES

subroutine montecarlo_cru(N,a,b,funci,I,desvest,seed)

	! Calcula la integral d'una funcio definida a un interval [a,b] utilitzant
	! Montecarlo Cru.
	!
	! INPUTS:	N:	type integer, es el numero de valors que utilitzarem per
	!				el calcul de la integral
	!
	!			a:	type dble, limit inferior de la integral
	!
	!			b: 	type dble, limit superior de la integral
	!
	!			funci:	type dble, funcio la qual integrem
	!
	!			seed: 	type logical, si es True inicialitzem amb una llavor
	!					concreta els valors aleatoris uniformes [0,1]
	!					si es false cada cop que s'executi cambiaran
	!
	! OUTPUTS:	I:	type dble, resultat de la integral
	!
	!			desvest:	type dble, desviaccio estandard de la integral


	implicit none

	integer N
	double precision a,b,funci,I,desvest
	logical seed
	integer ISEED

	double precision H, H2, IH2, x
	integer count

	! inicialitzem amb una llavor o no
	if (seed.eqv..true.) then
		ISEED = 17682733
		call srand(ISEED)
	end if

	! calcul de la integral fent Montecarlo cru
	I = 0.d0
	IH2 = 0.d0
	do count=1,N
		! x es un valor aleatori uniforme [0,1]
		call random_number(x)
		H =(b-a)*funci((b-a)*x +a)
		I = I + H
		H2 = H**2
		IH2 = IH2 + H2
	end do

	I = I/dble(N)
	IH2 = IH2/dble(N)

	desvest = (1.d0/sqrt(dble(N)))*sqrt(IH2-I**2)
	return
end subroutine montecarlo_cru

subroutine montecarlo_imp_no_funciona(ndat,xnums,funci,I,desvest)

	! DESUS** NO FUNCIONA !!!!!!!!!!!!

	! INPUTS:	ndat:	type:int . Nombre de valors amb que es calcula
	!					la integral.
	!
	!			xnums:	type:dble(ndat)	. Vector de valors aleatoris 
	!					distribuits segons una densitat de probabilitat.
	!
	!			funci: 	type: dble. Funcio f(x) on f(x) és el integrand.
	!
	! OUTPUTS:	I:	type:dble . Resultat de la integral.
	!
	!			Desvest:	type:dble . Desviacció estandard (error) de la integral

	implicit none

	integer ndat
	double precision xnums(ndat),funci,I,desvest

	integer count
	double precision I2


	I = 0.d0
	I2 = 0.d0

	do count=1,ndat
		I = I + funci(xnums(count))
		I2 = I2 + (funci(xnums(count)))**2
	end do

	I = I/dble(ndat)
	I2 = I2/dble(ndat)
	desvest = sqrt((I2-I**2)/dble(ndat))
	return
end subroutine montecarlo_imp_no_funciona


subroutine montecarlo_importancia(f, g, g_inv, a, b, n, result, error)


	! Args**: 	f: type:function (dble). Funcio la qual volem integrar.
	!
	!				g:	type:function (dble). Funcio amb la qual volem que tingui
	!					distribuccio. RECORDA** que ha d'estar normalitzada.
	!
	!				g_inv: type: function (dble). Funcio inversa de la funcio g
	!
	!				a: type (dble). Limit inferior de la integral
	!
	!				b: type (dble). Limit superior de la integral
	!
	!				result:	Valor de la integral calculada
	!
	!				error:	error del valor de la integral calculada


    double precision f, g, g_inv
    double precision a, b, result, error
    integer n
    double precision x, y, w, sum, sum2, val_integral
    integer i

    sum = 0.d0
    sum2 = 0.d0

    do i = 1, n
        x = a + (b-a)*rand()
        y = g_inv(x)
        w = f(y) / g(y)
        sum = sum + w
        sum2 = sum2 + w**2
    end do

    val_integral = (b-a)*sum/n
    error = (b-a)*sqrt((sum2/n - val_integral**2)/n)
    result = val_integral

    return
end subroutine montecarlo_importancia



! FUNCIONS

double precision function I1(x)

	implicit none
	double precision x

	double precision pi

	pi = 4.d0*atan(1.d0)

	I1 = sqrt(pi**2 +x**2)

	return
end function I1

double precision function I2(x)

	implicit none
	double precision x

	I2 = (x+3.d0*x**2*sin(x)-x**3)*(cos(x))**2*sin(x)

	return
end function I2

double precision function f(x)

	implicit none
	double precision x

	f = x

	return
end function f

double precision function g(x)

	implicit none

	double precision x

	g = 2.d0*x

	return
end function g

double precision function g_inv(x)

	implicit none
	double precision x

	g_inv = 1.d0/(2.d0*x)

	return
end function g_inv

double precision function f1(x)

	implicit none
	double precision x

	f1 = x**2

	return
end function f1
























