
program prepractica_3

implicit none

double precision E, fu, dfu
double precision xarrel, A, B, eps, x0
integer niter
integer counter

! apartat 4)
double precision E_34(34), f_34(34), E_420(420), f_420(420) 
double precision df_34(34), df_420(420), df_34comp(34), df_420comp(420)

external fun


! 2) TESTEJAR SUBROUTINES Bisection i NewtonRap

! a) representar gràficament la funcio F(E) amb E=[0,2*pi]
open(97,file='P3-valors-E-F-dF.dat')
do counter = 1,1000
	E = (dble(counter)/1000.d0)*2.d0*4.d0*atan(1.d0)
	call fun(E,fu,dfu)
	write(97,*) E, fu, dfu
end do
close(97)


! b) us subrutins bisseció trobar arrels de F(E)

! COSES A "SOLUCIONAR", sembla que les arrels les troba be pero
! no amb la precisio que demana de e-12, sembla que per alla al mig
! es com si es perdi amb alguns valors.


! donant aquest interval troba una arrel a 0
A = 0.d0
B = 1.d0
eps = 1.d-12
call Bisection(A,B,eps,fun,niter,xarrel)


! donant aquest altre interval troba l'arrel a 1.41....
A = 1.d0
B = 2.d0
eps = 1e-12
call Bisection(A,B,eps,fun,niter,xarrel)


! donant aquesta altre interval troba l'arrel a 2*pi
A = 2.d0
B = 3.d0
eps = 1.e-12
call Bisection(A,B,eps,fun,niter,xarrel)


! c) fem Newton-Raphson per trobar arrels comencat des de 9 punts dif
eps = 1d-12

open(1,file='P3-22-23-res.dat')

x0 = dble(0.1)
call NewtonRap(x0,eps,fun,niter,xarrel)
write(1,*) 0.1, niter
x0 = dble(0.2)
call NewtonRap(x0,eps,fun,niter,xarrel)
write(1,*)0.2, niter
x0 = dble(0.65)
call NewtonRap(x0,eps,fun,niter,xarrel)
write(1,*)0.65, niter
x0 = dble(0.7)
call NewtonRap(x0,eps,fun,niter,xarrel)
write(1,*)0.7, niter
x0 = dble(1.3)
call NewtonRap(x0,eps,fun,niter,xarrel)
write(1,*)1.3, niter
x0 = dble(2.4)
call NewtonRap(x0,eps,fun,niter,xarrel)
write(1,*)2.4, niter
x0 = dble(2.6)
call NewtonRap(x0,eps,fun,niter,xarrel)
write(1,*)2.6, niter
x0 = dble(3.9)
call NewtonRap(x0,eps,fun,niter,xarrel)
write(1,*)3.9, niter
x0 = dble(5.3)
call NewtonRap(x0,eps,fun,niter,xarrel)
write(1,*)5.3, niter

close(1)



! 4) Test de la subrutina "derfun"


! vectors de 34 punts entre [0,2*pi], valors analitics
do counter=1,34
	E_34(counter) = (dble(counter)/34.d0)*2.d0*4.d0*atan(1.d0)
	call fun(E_34(counter), f_34(counter), df_34(counter))
end do

! valor computacional de f'(E) de 34 valors
call derfun(34,E_34,f_34,df_34comp)

! vectors de 420 punts entre [0,2*pi], valors analitics
do counter=1,420
	E_420(counter) = (dble(counter)/420.d0)*2.d0*4.d0*atan(1.d0)
	call fun(E_420(counter), f_420(counter), df_420(counter))
end do

! valor computacional de f'(E) de 420 valors
call derfun(420,E_420,f_420,df_420comp)


! escrivim en 2 fitxers els resultats obtinguts per 34 i 420 valors
! fitxer de 34 valors
open(2,file='P3-22-23-res3-n34.dat')
do counter=1,34
	write(2,*) E_34(counter), f_34(counter), df_34comp(counter), df_34(counter)
end do
close(2)

! fitxer de 420 valors
open(3,file="P3-22-23-res3-n420.dat")
do counter=1,420
	write(3,*)E_420(counter), f_420(counter), df_420comp(counter), df_420(counter)
end do
close(3)


end program prepractica_3


subroutine NewtonRap(x0,eps,fun,niter,xarrel)
	
	! troba 1 arrel d'una funcio f(x)
	! Args**:   x0: double precision, punt inicial de Newton Raphson
	!			eps: double precision, precisió desitjada
	! 			fun(x,fu,dfu): subroutina que retorna el valor de f(x) i f'(x),
	!							respectivament, definida com a external
	!			niter: és un output, nombre d'iteracions per aconseguir
	!					la precisió
	!			xarrel: és un output, valor de l'arrel


	implicit none

	double precision x0, eps, xarrel
	integer niter

	external fun

	double precision x1,f_x0,df_x0
	logical control

	! variables a inicialitzar
	control = .true.
	niter = 0

	do while (control.EQV..true.)

		niter = niter +1

		! calcul de x1 a partir de x0
		call fun(x0,f_x0,df_x0)
		x1 = x0 - (f_x0/df_x0)

		! tenim el valor amb la precisio desitjada
		if (abs(x1-x0).LT.eps) then
			xarrel = x1
			control =.false.
		end if

		! no tenim el valor amb la precisio desitjada
		if (abs(x1-x0).GE.eps) then
			x0 = x1
		end if
	end do

	! imprimim per pantalla els valors trobats
	write(*,*)"Newton-Raphson: Valor de l'arrel es a x= ", xarrel
	write(*,*)"Newton-Raphson: Nombre d'iteracions = ", niter

	return
end subroutine NewtonRap


subroutine Bisection(A,B,eps,fun,niter,xarrel)

	! troba 1 arrel d'una funcio f(x) (retornara la primera que trobi)
	! Args**:	A,B: double precision, punts incials de bisecció (A<B)
	!			eps: double precision, precisió desitjada
	!			fun(x,fu): subroutina que retorna el valor de f(x),
	!						 definida com a external
	!			niter: output integer, nombre d'iteracions per aconseguir
	!					la precisió
	!			xarrel: output double precision, valor de l'arrel
	

	implicit none

	double precision A, B, eps, xarrel
	integer	niter
	double precision C,fa,fb,fc

	external fun

	! variable auxiliar del valor de la derivada (no utilitzare)
	double precision aux


	niter=0
	xarrel = 0.d0

	do while ((B-A).GT.eps)
		C = (A+B)/2.d0

		call fun(A,fa,aux)
		call fun(B,fb,aux)
		call fun(C,fc,aux)

		niter = niter +1

		! primer mirem si canvia de signe la funcio en A i B
		if (fa*fb.GT.0.d0) then
			write(*,*)"La funcio no canvia de signe en A,B"
			! tenco el bucle fent A=B
			A=B
		end if

		! [A,C] hi ha canvi de signe
		if ((fc*fa).LT.0.d0) then
			xarrel = C
			B = C
		end if

		! [C,B] hi ha canvi de signe
		if ((fc*fb).LT.0.d0) then
			xarrel = C
			A = C
		end if

		! C es exactament l'arrel
		if (fc.EQ.0.d0) then
			xarrel = C
			! tenco el bucle fent A=C i B=C
			A = C
			B = C
		end if

		! A es exactament l'arrel
		if (fa.EQ.0.d0) then
			xarrel = A
			! tenco el bucle fent B=A
			B = A
		end if

		! B es exactament l'arrel
		if (fb.EQ.0.d0) then
			xarrel = B
			! tenco el bucle fent A=B
			A = B
		end if
	end do

	! escriu els resultats per pantalla
	write(*,*)"Biseccio: Valor de l'arrel es a x= ", xarrel
	write(*,*)"Biseccio: Nombre d'iteracions = ", niter

	return
end subroutine Bisection


subroutine derfun(ndat,x,fu,dfu)

	! Calcula la derivada numèrica f'(x_k) de vectors de dades
	! Args**: 	ndat:	numero de valors que tenen els vectors x i fu
	!			x:		vector de valors de x, equiespaiats amb longitud
	!					igual a ndat
	!			fu:		vector de valors de la funcio en els punts x, amb
	!					longitud igual a ndat
	!			dfu:	output, que és un vector amb la derivada calculada
	!					numericament, amb dimensio ndat


	implicit none

	integer ndat
	double precision x(ndat), fu(ndat), dfu(ndat)
	double precision h

	! "i" es un contador
	integer i

	do i=1,ndat
		! valors de la variable x EQUIESPAIATS
		h = x(2) - x(1)
		
		! cas singular pel primer valor
		if (x(i).eq.x(1)) then
			dfu(1) = (fu(1+1)-fu(1))/h
		end if

		! cas singular per l'ultim valor
		if (x(i).eq.x(ndat)) then
			dfu(ndat) = (fu(ndat)-fu(ndat-1))/h
		end if

		! cas de valors que estan entre el primer i l'ultim valor
		if ((x(i).GT.x(1)).AND.(x(i).LT.x(ndat))) then
			dfu(i) = (fu(i+1)-fu(i-1))/(2.d0*h)
		end if
	end do
	return
end subroutine derfun


subroutine fun(E,fu,dfu)
	
	! funcio particular F(E) que es un polinomi per un sinh() el qual donat un
	! E retorna el valor de la funció i la seva derivada analítica en E

	implicit none

	double precision E, fu, dfu

	fu = ((177.d0/66.d0) + (50.d0/33.d0)*E -(233.d0/66.d0)*E**2 + E**3)*sinh(E)

	dfu = (3.d0*E**2 -(233.d0/33.d0)*E +(50.d0/33.d0))*sinh(E) &
			+ (E**3 -(233.d0/66.d0)*E**2 + (50.d0/33.d0)*E + (175.d0/66.d0)) &
			* cosh(E)

	return
end subroutine fun

