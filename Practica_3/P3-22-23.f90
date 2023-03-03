
program practica3

implicit none

double precision E_hal(100), D_hal(100), dD_hal(100)
double precision D

! variables EXERCICI 2)
double precision A, B, eps, xarrel,D_Emax
integer niter

! variables exercici 3)
double precision t
double precision TH, E0, a1, error, y, x

! comptador
integer count

external fun
external fun1


! EXERCICI 1)
! genero 100 E's entre[0,2*pi] i obtinc les D's
do count=1,100
	E_hal(count) = (dble(count)/100.d0)*2.d0*4.d0*atan(1.d0)
	D_hal(count) = D(E_hal(count))
end do

! calcul de la derivada numerica
call derfun(100,E_hal,D_hal,dD_hal)


open(1,file='P3-22-23-res.dat')
do count=1,100
	write(1,*)E_hal(count), D_hal(count), dD_hal(count)
end do



! EXERCICI 2
! busco les arrels de la funcio
! l'he representat i ser que hi han 3 en el interval com la meva subrutina de 
! Biseccio nomes busca 1 i agafa la primera que troba, he escollit intervals
! adients per trobar les 3.

eps = 1.d-10

! per trobar la primera arrel
A = 2.d0
B = 4.d0
eps = 1.d-10
call Bisection(A,B,eps,fun,niter,xarrel)
D_Emax = D(xarrel)
write(1,*)
write(1,*)
write(1,*) xarrel, D_Emax

! per trobar la segona arrel
A = 1.d0
B = 2.d0
call Bisection(A,B,eps,fun,niter,xarrel)
D_Emax = D(xarrel)
write(1,*) xarrel, D_Emax

! per trobar la segona arrel
A = 4.d0
B = 7.d0
call Bisection(A,B,eps,fun,niter,xarrel)
D_Emax = D(xarrel)
write(1,*) xarrel, D_Emax



! EXERCICI 3
! 80 valors equiespaiats t [0,TH]
TH = dble(75.3)
E0 = 4.d0*atan(1.d0)
error = 1d-12
a1 = dble(17.857619)
eps = dble(0.967990)

write(1,*)
write(1,*)

do count=1,80
	t = (dble(count)/80.d0)*TH
	call NewtonRap(t,E0,eps,fun1,niter,xarrel)
	x = a1*(cos(xarrel)-eps)
	y = a*1((1.d0-eps**2)*sin(xarrel))

	! escrivim els valors calculats
	write(1,*) t, xarrel, x, y

end do

	


close(1)
end program practica3




subroutine NewtonRap(t,x0,eps,fun1,niter,xarrel)
	
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

	external fun1

	double precision x1,f_x0,df_x0
	logical control

	double precision t

	! variables a inicialitzar
	control = .true.
	niter = 0

	do while (control.EQV..true.)

		niter = niter +1

		! calcul de x1 a partir de x0
		call fun1(t,x0,f_x0,df_x0)
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

double precision function D(E)

	! calcula la distancia al origen de coordenades 
	! Args**: E: valor entre [0,2pi]

	implicit none
	double precision E
	double precision x, y

	! contants del problema del cometa Halley
	double precision a, eps

	! "a" esta en unitats U.A.
	a = 17.857619
	eps = 0.967990

	x = a*(cos(E)-eps)
	y = a*(1-eps**2)**0.5 * sin(E)

	D = (x**2 + y**2)**0.5
	return
end function D

subroutine fun(E,fu,dfu)

	implicit none

	double precision E, fu, dfu
	double precision eps

	! constants del problema
	eps = 0.967990

	! dfu es una variable auxialar que no utilitzare
	dfu = 0.d0

	fu = sin(2*E)*(1-eps**2) - (cos(E)*(2-eps**2) -eps)*sin(E)
	
	return
end subroutine fun

subroutine fun1(t,E,fe,dfe)

	implicit none
	double precision t, fe, dfe, E
	double precision eps, th

	! constants del problema
	eps = dble(0.967990)
	TH = dble(75.3)

	dfe = 0.d0

	fe = -E + (2.d0*4.d0*atan(1.d0)/TH)*t -eps*sin(E)
	return
end subroutine fun1







