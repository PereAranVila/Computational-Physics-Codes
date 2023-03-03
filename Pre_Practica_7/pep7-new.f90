
program pre7

implicit none

double precision f2, fos1, fos2, fos3, fos4

double precision h,x0,xf, y01(1),y04(4)
integer num_file
character(100) name_file
logical is_file, is_close

external f2
external fos1, fos2, fos3, fos4

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! TESTEJEM LA SUBROUTINA EULER, caiguda lliure i amb friccio

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! TEST DE LA SUBROUTINA RK4 CAIGUDA LLIURE AMB FREGAMENT	!OK FUNCIONA

h = 0.01d0
x0 = 0.d0
xf = 50.d0
y01(1) = 0.d0
num_file = 1
name_file = "valors-euler.dat"
is_file = .false.
is_close = .false.

call euler(h,x0,xf,y01,1,f2,f2,f2,f2,f2,name_file,num_file,is_file,is_close)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! TEST DE LA SUBROUTINA RK4 SISTEMA D'OSCILADORS ACBOLATS (PSIM)	! OK FUNCIONA

h = 0.05d0
x0 = 0.d0
xf = 40.d0
y04(1) = 0.d0
y04(2) = 0.d0
y04(3) = 1.d0
y04(4) = 0.d0
num_file = 1
name_file = "valors-euler.dat"
is_file = .true.
is_close = .true.

call euler(h,x0,xf,y04,4,fos1,fos2,fos3,fos4,fos4,name_file,num_file,is_file, &
			is_close)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! TEST DE LA SUBRUTINA EULER MILLORAT

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! TEST DE LA SUBROUTINA RK4 CAIGUDA LLIURE AMB FREGAMENT	!OK FUNCIONA

h = 0.01d0
x0 = 0.d0
xf = 50.d0
y01(1) = 0.d0
num_file = 2
name_file = "valors-euler-millorat.dat"
is_file = .false.
is_close = .false.

call euler_millorat(h,x0,xf,y01,1,f2,f2,f2,f2,f2,name_file,num_file,is_file,is_close)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! TEST DE LA SUBROUTINA RK4 SISTEMA D'OSCILADORS ACBOLATS (PSIM)	! OK FUNCIONA

h = 0.05d0
x0 = 0.d0
xf = 40.d0
y04(1) = 0.d0
y04(2) = 0.d0
y04(3) = 1.d0
y04(4) = 0.d0
num_file = 2
name_file = "valors-euler-millorat.dat"
is_file = .true.
is_close = .true.

call euler_millorat(h,x0,xf,y04,4,fos1,fos2,fos3,fos4,fos4,name_file,num_file,is_file, &
			is_close)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


end program pre7

subroutine euler(h,x0,xf,y0,num_eq,f1,f2,f3,f4,f5,name_file,num_file, &
					is_file,is_close)

	! Resol el sistema d'equacions diferencials proposat utilitzant el metode
	! de Euler. 
	!
	! Aquesta subroutina pot arribar a calcular fins a 5 equaccions
	! diferencials de primer ordre. Si es volguessin fer més d'aquestes 5
	! equaccions s'hauria de modificar lleugerament.
	!
	! Args*:	h:	distància entre el següent pas
	!
	!			x0:	type:dble. Valor inicial de la variable dependent "x" el
	!				qual volem començar a resoldre la nostre eq.diferencial.
	!				A més, és el valor de la variable dependent el qual
	!				l'hi apliquem les condicions incials. y0 = y(x=x0)
	!
	!			xf: type:dble. Valor final de la variable independent "x" el
	!				qual volem resoldre la nostre eq.diferencial.
	!
	!			y0:	vector(num_eq). Conté les condicions incials de cada 
	!				equccio diferencial (variable dependent "y") (i ordenats
	!				per ordre). Exemple: y0(1) és la variable dependent de la
	!				condicció inicial de la primera equacció diferencial.
	!
	!			num_eq:		type:int.	Nombre d'equacions diferencials de
	!						primer ordre a resoldre en total. (Com a màxim
	!						num_eq = 5. Per mes la subroutina no funciona.)
	!
	!			f1,f2,f3,f4,f5:	funcions (type:dble). Corresponen a la
	!								terme igualat del dy/dx. El numero de cada
	!								funcio esta associat a l'eq.diferencial que
	!								l'hi correspon. Exemple: f1 pues es el terme:
	!								dy1/dx = f1(x,y1,y2,y3,y4,y5,y6).
	!
	!							Recorda que les funcions que no utilitzis a
	!							nivell practica també l'hi has de passar una
	!							funcio tot i que aquestes no facin res.
	!							Recomano que li donis una que ja has creat aixi
	!							no n'has de crear alguna d'auxiliar.
	!
	!
	!			name_file:	type:character .Nom del arxiu on hi ha d'incloure l'extensió .dat
	!						ja que els valors calculats de les eq. diferencials
	!						es guardaran en aquest arxiu.
	!
	!			num_file:	type:int .Valor el qual se l'hi assigna el arxiu al obrir-lo
	!						i en el que ens referenciem. ex : num_file = 2,
	!						open(2,file=name_file)
	!
	!			is_file:	type:logical .Indica si el arxiu s'ha creat i està obert.
	!						"true" vol dir que el arxiu esta creat i obert.
	!						"false" indica que no s'ha creat cap arxiu.
	!
	!			is_close:	type:logical .Indica si vols que un cop escrit
	!						es tenqui el fitxer.
	!						"true" indica que el fitxer es tancara
	!						"false" indica que el fitxer es deixa obert (per
	!						si volem escriure uns altres results mes endevant)

	implicit none

	integer num_eq, num_file
	double precision f1,f2,f3,f4,f5
	double precision h,x0,xf,y0(num_eq)
	character(100) name_file
	logical is_file, is_close

	integer i, count

	double precision xn
	double precision yn1,yn2,yn3,yn4,yn5



	! GESTIO DE FITXERS
	! decidim a on volem guardar els valors que anem obtenint
	if (is_file.eqv..false.) then
		open(num_file,file=name_file)
	end if

	! en el cas que ja hi hagi un fitxer obert escrivim 2 linies en blanc
	if (is_file.eqv..true.) then
		write(num_file,*)
		write(num_file,*)
	end if


	! nombre d'intervals (pasos a fer) "i"
	i = int(abs(xf-x0)/h)


	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	! ALGORITME EULER

	! CAS 1 EQ.DIF. DE PRIMER ORDRE
	if (num_eq.eq.1) then
		! algoritme Euler
		yn1 = y0(1)
		xn = x0

		! guardem les condicions incials al fitxer
		write(num_file,*)xn,yn1

		do count = 1,i
		
			xn = x0 + count*h
			yn1 = yn1 + h*f1(xn,yn1)

			! guardem els valors en el fitxer
			write(num_file,*) xn, yn1
		end do
	end if


	! CAS 2 EQ.DIF. DE PRIMER ORDRE
	if (num_eq.eq.2) then
		! algoritme Euler
		yn1 = y0(1)
		yn2 = y0(2)
		xn = x0

		! guardem les condicions incials al fitxer
		write(num_file,*)xn,yn1,yn2

		do count = 1,i
		
			xn = x0 + count*h
			yn1 = yn1 + h*f1(xn,yn1,yn2)
			yn2 = yn2 + h*f2(xn,yn1,yn2)

			! guardem els valors en el fitxer
			write(num_file,*) xn, yn1, yn2
		end do
	end if


	! CAS 3 EQ.DIF. DE PRIMER ORDRE
	if (num_eq.eq.3) then
		! algoritme Euler
		yn1 = y0(1)
		yn2 = y0(2)
		yn3 = y0(3)
		xn = x0

		! guardem les condicions incials al fitxer
		write(num_file,*)xn, yn1, yn2, yn3

		do count = 1,i
		
			xn = x0 + count*h
			yn1 = yn1 + h*f1(xn,yn1,yn2,yn3)
			yn2 = yn2 + h*f2(xn,yn1,yn2,yn3)
			yn3 = yn3 + h*f3(xn,yn1,yn2,yn3)

			! guardem els valors en el fitxer
			write(num_file,*) xn, yn1, yn2, yn3
		end do
	end if


	! CAS 4 EQ.DIF. DE PRIMER ORDRE
	if (num_eq.eq.4) then
		! algoritme Euler
		yn1 = y0(1)
		yn2 = y0(2)
		yn3 = y0(3)
		yn4 = y0(4)
		xn = x0

		! guardem les condicions incials al fitxer
		write(num_file,*)xn, yn1, yn2, yn3, yn4

		do count = 1,i
		
			xn = x0 + count*h
			yn1 = yn1 + h*f1(xn,yn1,yn2,yn3,yn4,yn5)
			yn2 = yn2 + h*f2(xn,yn1,yn2,yn3,yn4,yn5)
			yn3 = yn3 + h*f3(xn,yn1,yn2,yn3,yn4,yn5)
			yn4 = yn4 + h*f4(xn,yn1,yn2,yn3,yn4,yn5) 

			! guardem els valors en el fitxer
			write(num_file,*) xn, yn1, yn2, yn3, yn4, yn5

		end do
	end if


	! CAS 5 EQ.DIF. DE PRIMER ORDRE
	if (num_eq.eq.5) then
		! algoritme Euler
		yn1 = y0(1)
		yn2 = y0(2)
		yn3 = y0(3)
		yn4 = y0(4)
		yn5 = y0(5)
		xn = x0

		! guardem les condicions incials al fitxer
		write(num_file,*)xn, yn1, yn2, yn3, yn4, yn5

		do count = 1,i
		
			xn = x0 + count*h
			yn1 = yn1 + h*f1(xn,yn1,yn2,yn3,yn4,yn5)
			yn2 = yn2 + h*f2(xn,yn1,yn2,yn3,yn4,yn5)
			yn3 = yn3 + h*f3(xn,yn1,yn2,yn3,yn4,yn5)
			yn4 = yn4 + h*f4(xn,yn1,yn2,yn3,yn4,yn5)
			yn5 = yn5 + h*f5(xn,yn1,yn2,yn3,yn4,yn5)

			! guardem els valors en el fitxer
			write(num_file,*) xn, yn1, yn2, yn3, yn4, yn5
		end do
	end if

	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	! GESTIO TANCAMENT DEL ARXIU GENERAT
	! si volem tanquem el arxiu
	if (is_close.eqv..true.) then
		close(num_file)
	end if
end subroutine euler

subroutine euler_millorat(h,x0,xf,y0,num_eq,f1,f2,f3,f4,f5,name_file,num_file, &
					is_file,is_close)

	! Resol el sistema d'equacions diferencials proposat utilitzant un euler
	! millorat.
	!
	! Aquesta subroutina pot arribar a calcular fins a 5 equaccions
	! diferencials de primer ordre. Si es volguessin fer més d'aquestes 5
	! equaccions s'hauria de modificar lleugerament.
	!
	! Args*:	h:	distància entre el següent pas
	!
	!			x0:	type:dble. Valor inicial de la variable dependent "x" el
	!				qual volem començar a resoldre la nostre eq.diferencial.
	!				A més, és el valor de la variable dependent el qual
	!				l'hi apliquem les condicions incials. y0 = y(x=x0)
	!
	!			xf: type:dble. Valor final de la variable independent "x" el
	!				qual volem resoldre la nostre eq.diferencial.
	!
	!			y0:	vector(num_eq). Conté les condicions incials de cada 
	!				equccio diferencial (variable dependent "y") (i ordenats
	!				per ordre). Exemple: y0(1) és la variable dependent de la
	!				condicció inicial de la primera equacció diferencial.
	!
	!			num_eq:		type:int.	Nombre d'equacions diferencials de
	!						primer ordre a resoldre en total. (Com a màxim
	!						num_eq = 5. Per mes la subroutina no funciona.)
	!
	!			f1,f2,f3,f4,f5:	funcions (type:dble). Corresponen a la
	!								terme igualat del dy/dx. El numero de cada
	!								funcio esta associat a l'eq.diferencial que
	!								l'hi correspon. Exemple: f1 pues es el terme:
	!								dy1/dx = f1(x,y1,y2,y3,y4,y5,y6).
	!
	!							Recorda que les funcions que no utilitzis a
	!							nivell practica també l'hi has de passar una
	!							funcio tot i que aquestes no facin res.
	!							Recomano que li donis una que ja has creat aixi
	!							no n'has de crear alguna d'auxiliar.
	!
	!
	!			name_file:	type:character .Nom del arxiu on hi ha d'incloure l'extensió .dat
	!						ja que els valors calculats de les eq. diferencials
	!						es guardaran en aquest arxiu.
	!
	!			num_file:	type:int .Valor el qual se l'hi assigna el arxiu al obrir-lo
	!						i en el que ens referenciem. ex : num_file = 2,
	!						open(2,file=name_file)
	!
	!			is_file:	type:logical .Indica si el arxiu s'ha creat i està obert.
	!						"true" vol dir que el arxiu esta creat i obert.
	!						"false" indica que no s'ha creat cap arxiu.
	!
	!			is_close:	type:logical .Indica si vols que un cop escrit
	!						es tenqui el fitxer.
	!						"true" indica que el fitxer es tancara
	!						"false" indica que el fitxer es deixa obert (per
	!						si volem escriure uns altres results mes endevant)

	implicit none

	integer num_eq, num_file
	double precision f1,f2,f3,f4,f5
	double precision h,x0,xf,y0(num_eq)
	character(100) name_file
	logical is_file, is_close

	integer i, count

	double precision xn
	double precision yn10,yn20,yn30,yn40,yn50
	double precision yn11, yn21, yn31, yn41, yn51
	double precision yn12, yn22, yn32, yn42, yn52



	! GESTIO DE FITXERS
	! decidim a on volem guardar els valors que anem obtenint
	if (is_file.eqv..false.) then
		open(num_file,file=name_file)
	end if

	! en el cas que ja hi hagi un fitxer obert escrivim 2 linies en blanc
	if (is_file.eqv..true.) then
		write(num_file,*)
		write(num_file,*)
	end if


	! nombre d'intervals (pasos a fer) "i"
	i = int(abs(xf-x0)/h)


	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	! ALGORITME EULER MILLORAT

	! CAS 1 EQ.DIF. DE PRIMER ORDRE
	if (num_eq.eq.1) then
		! algoritme Euler millorat
		yn10 = y0(1)
		xn = x0

		! guardem les condicions incials al fitxer
		write(num_file,*)xn,yn10

		do count = 1,i
		
			xn = x0 + count*h

			! inicialitzem amb un Euler normal
			if (count.eq.1) then
				yn11 = yn10 + h*f1(xn,yn10)
				write(num_file,*) xn,yn11	
			end if

			! continuem amb un Euler millorat
			if (count.gt.1) then
				yn12 = yn10 +2.d0*h*f1(xn,yn11)
				yn10 = yn11
				yn11 = yn12
				! guardem els valors en el fitxer
				write(num_file,*) xn, yn12
			end if
		end do
	end if


	! CAS 2 EQ.DIF. DE PRIMER ORDRE
	if (num_eq.eq.2) then
		! algoritme Euler millorat
		yn10 = y0(1)
		yn20 = y0(2)
		xn = x0

		! guardem les condicions incials al fitxer
		write(num_file,*)xn, yn10, yn20

		do count = 1,i
		
			xn = x0 + count*h

			! inicialitzem amb un Euler normal
			if (count.eq.1) then
				yn11 = yn10 + h*f1(xn,yn10,yn20)
				yn21 = yn20 + h*f2(xn,yn10,yn20)
				write(num_file,*) xn, yn11, yn21	
			end if

			! continuem amb un Euler millorat
			if (count.ne.1) then
				yn12 = yn10 +2.d0*h*f1(xn,yn11,yn21)
				yn22 = yn20 +2.d0*h*f2(xn,yn11,yn21)

				yn10 = yn11
				yn11 = yn12
				yn20 = yn21
				yn21 = yn22

				! guardem els valors en el fitxer
				write(num_file,*) xn, yn12, yn22
			end if

		end do
	end if

	! CAS 3 EQ.DIF. DE PRIMER ORDRE
	if (num_eq.eq.3) then
		! algoritme Euler millorat
		yn10 = y0(1)
		yn20 = y0(2)
		yn30 = y0(3)
		xn = x0

		! guardem les condicions incials al fitxer
		write(num_file,*)xn, yn10, yn20, yn30

		do count = 1,i
		
			xn = x0 + count*h

			! inicialitzem amb un Euler normal
			if (count.eq.1) then
				yn11 = yn10 + h*f1(xn,yn10,yn20,yn30)
				yn21 = yn20 + h*f2(xn,yn10,yn20,yn30)
				yn31 = yn30 + h*f3(xn,yn10,yn20,yn30)
				write(num_file,*) xn, yn11, yn21, yn31	
			end if

			! continuem amb un Euler millorat
			if (count.ne.1) then
				yn12 = yn10 +2.d0*h*f1(xn,yn11,yn21,yn31)
				yn22 = yn20 +2.d0*h*f2(xn,yn11,yn21,yn31)
				yn32 = yn30 +2.d0*h*f3(xn,yn11,yn21,yn31)

				yn10 = yn11
				yn11 = yn12
				yn20 = yn21
				yn21 = yn22
				yn30 = yn31
				yn31 = yn32

				! guardem els valors en el fitxer
				write(num_file,*) xn, yn12, yn22, yn32

			end if
		end do
	end if


	! CAS 4 EQ.DIF. DE PRIMER ORDRE
	if (num_eq.eq.4) then
		! algoritme Euler millorat
		yn10 = y0(1)
		yn20 = y0(2)
		yn30 = y0(3)
		yn40 = y0(4)
		xn = x0

		! guardem les condicions incials al fitxer
		write(num_file,*)xn, yn10, yn20, yn30, yn40

		do count = 1,i
		
			xn = x0 + count*h

			! inicialitzem amb un Euler normal
			if (count.eq.1) then
				yn11 = yn10 + h*f1(xn,yn10,yn20,yn30,yn40)
				yn21 = yn20 + h*f2(xn,yn10,yn20,yn30,yn40)
				yn31 = yn30 + h*f3(xn,yn10,yn20,yn30,yn40)
				yn41 = yn40 + h*f4(xn,yn10,yn20,yn30,yn40)
				write(num_file,*) xn, yn11, yn21, yn31, yn41
			end if

			! continuem amb un Euler millorat
			if (count.ne.1) then
				yn12 = yn10 +2.d0*h*f1(xn,yn11,yn21,yn31,yn41)
				yn22 = yn20 +2.d0*h*f2(xn,yn11,yn21,yn31,yn41)
				yn32 = yn30 +2.d0*h*f3(xn,yn11,yn21,yn31,yn41)
				yn42 = yn40 +2.d0*h*f4(xn,yn11,yn21,yn31,yn41)


				yn10 = yn11
				yn11 = yn12
				yn20 = yn21
				yn21 = yn22
				yn30 = yn31
				yn31 = yn32
				yn40 = yn41
				yn41 = yn42

				! guardem els valors en el fitxer
				write(num_file,*) xn, yn12, yn22, yn32, yn42

			end if
		end do
	end if


	! CAS 5 EQ.DIF. DE PRIMER ORDRE
	if (num_eq.eq.5) then
		! algoritme Euler millorat
		yn10 = y0(1)
		yn20 = y0(2)
		yn30 = y0(3)
		yn40 = y0(4)
		yn50 = y0(5)
		xn = x0

		! guardem les condicions incials al fitxer
		write(num_file,*)xn, yn10, yn20, yn30, yn40, yn50

		do count = 1,i
		
			xn = x0 + count*h

			! inicialitzem amb un Euler normal
			if (count.eq.1) then
				yn11 = yn10 + h*f1(xn,yn10,yn20,yn30,yn40,yn50)
				yn21 = yn20 + h*f2(xn,yn10,yn20,yn30,yn40,yn50)
				yn31 = yn30 + h*f3(xn,yn10,yn20,yn30,yn40,yn50)
				yn41 = yn40 + h*f4(xn,yn10,yn20,yn30,yn40,yn50)
				yn51 = yn50 + h*f5(xn,yn10,yn20,yn30,yn40,yn50)
				write(num_file,*) xn, yn11, yn21, yn31, yn41, yn51
			end if

			! continuem amb un Euler millorat
			if (count.ne.1) then
				yn12 = yn10 +2.d0*h*f1(xn,yn11,yn21,yn31,yn41,yn51)
				yn22 = yn20 +2.d0*h*f2(xn,yn11,yn21,yn31,yn41,yn51)
				yn32 = yn30 +2.d0*h*f3(xn,yn11,yn21,yn31,yn41,yn51)
				yn42 = yn40 +2.d0*h*f4(xn,yn11,yn21,yn31,yn41,yn51)
				yn52 = yn50 +2.d0*h*f5(xn,yn11,yn21,yn31,yn41,yn51)


				yn10 = yn11
				yn11 = yn12
				yn20 = yn21
				yn21 = yn22
				yn30 = yn31
				yn31 = yn32
				yn40 = yn41
				yn41 = yn42
				yn50 = yn51
				yn51 = yn52


				! guardem els valors en el fitxer
				write(num_file,*) xn, yn12, yn22, yn32, yn42, yn52
			end if
		end do
	end if

	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	! GESTIO TANCAMENT DEL ARXIU GENERAT
	! si volem tanquem el arxiu
	if (is_close.eqv..true.) then
		close(num_file)
	end if
end subroutine euler_millorat



! FUNCIONS PER TESTEJAR LES SUBRUTINES

double precision function f2(t,v)

	implicit none
	double precision t,v
	double precision m, g, kappa

	! valors de les constants
	g = 9.807d0
	m = 60.d0
	kappa = 0.25d0

	f2 = g -kappa*(v**2)/m
	return
end function f2 

double precision function fos1(x,y1,y2,y3,y4)

	implicit none
	double precision x,y1,y2,y3,y4
	double precision k1,k2,m

	! constants del problema
	m = 1.d0
	k1 = 10.d0
	k2 = 0.5d0

	fos1 = (k2*y4 -(k1+k2)*y3)/m
	return
end function fos1

double precision function fos2(x,y1,y2,y3,y4)

	implicit none
	double precision x,y1,y2,y3,y4
	double precision k1,k2,m

	! constants del problema
	m = 1.d0
	k1 = 10.d0
	k2 = 0.5d0

	fos2 = (k2*y3 -(k1+k2)*y4)/m
	return
end function fos2

double precision function fos3(x,y1,y2,y3,y4)

	implicit none
	double precision x,y1,y2,y3,y4

	fos3 = y1
	return
end function fos3

double precision function fos4(x,y1,y2,y3,y4)

	implicit none
	double precision x,y1,y2,y3,y4

	fos4 = y2
	return
end function fos4