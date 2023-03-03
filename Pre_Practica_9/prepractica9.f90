
program prepractica9

implicit none

double precision rho, zero
double precision Lx,Ly,x0,y0,phi0(66,34),th
integer nx,ny
integer icontrol, num_file
logical is_file, is_close, ipunt
character(100) name_file
character(100) name_file_aux
double precision w
integer pos(2)

integer i,j

external rho

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! PROBLEMA 1

! PRIMERA FONT DE CALOR, RHO1, metode GAUSS-SEIDEL

! RECORDA**: Que quan dissenyis la primera matriu inicial corresponent a la
! malla, si et donen h que es la distancia necessitaras L/h +1 punts.
! Es molt important aixo, sino els extrems no coincidiran. 


Lx = 32.5d0
Ly = 16.5d0
x0 = 0.d0
y0 = 0.d0
nx = 66
ny = 34
th = 0.01d0
num_file = 110
is_file = .false.
is_close = .false.
name_file = 'P9-22-23-res.dat'
w = 0.d0
ipunt = .true.
pos(1) = 14
pos(2) = 14

! resolem el metode mitjançant el metode de Gauss-Seidel
icontrol = 1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! MALLA INICIAL

! generem la primera malla inicial
do i = 1,nx
	do j = 1,ny

		! condicions incials columna esquerra
		if (i.eq.1) then
			phi0(i,j) = 4.d0
		end if

		! condicions incials columna dreta
		if (i.eq.nx) then
			phi0(i,j) = 25.d0
		end if

		! condicions inicials fila inferior
		if (j.eq.1) then
			phi0(i,j) = 3.36d0
		end if

		! condicions incials fila superior
		if (j.eq.ny) then
			phi0(i,j) = 23.1d0
		end if

		! incialitzo els punts interiors amb una T = 12 graus. 
		if ((i.ne.1).and.(i.ne.nx).and.(j.ne.1).and.(j.ne.ny)) then
			phi0(i,j) = 12.d0
		end if
	end do
end do


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! resolem l'edp amb el metode de Gauss-Seidel
call edp_eliptica(rho,x0,y0,Lx,Ly,nx,ny,phi0,icontrol,th,w,pos,num_file, &
					name_file,is_file,is_close,ipunt)



icontrol = 0
is_file = .true.
is_close = .false.

! resolem l'edp amb el metode de Jacobi
call edp_eliptica(rho,x0,y0,Lx,Ly,nx,ny,phi0,icontrol,th,w,pos,num_file, &
	name_file,is_file,is_close,ipunt)



! resolem l'edp amb el metode de sobre-relaxcio
w = 1.73d0
icontrol = 2
is_file = .true.
is_close = .true.

call edp_eliptica(rho,x0,y0,Lx,Ly,nx,ny,phi0,icontrol,th,w,pos,num_file, &
	name_file_aux,is_file,is_close,ipunt)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!





end program prepractica9

subroutine edp_eliptica(f,x0,y0,Lx,Ly,nx,ny,phi0,icontrol,th,w,pos,num_file, &
						name_file,is_file, is_close,ipunt)

	!	Resol les equacions diferencials en derivades parcials del tipus
	!	eliptiques.
	!
	!	Els resultats es guarden de la seguent forma:
	!		- Si es vol es pot guardar com evoluciona el valor de un punt en
	!			cada iteracio.
	!		- Seguidament es guarden els valors final de la ultima malla.
	!
	!
	!
	! Args*:	f:	funcio definida com a external que equival al
	!				laplacia(phi) = f. En el cas que aquesta funcio
	!				sigui 0 sempre, s'ha de crear-ne un que sigui 0.
	!
	!			x0:	valor inicial del interval de les x ([x0,Lx])
	!
	!			y0: valor inicial del interval de les y ([y0,Ly])
	!
	!			Lx:	type(dble). Longitud x del rectangle de la malla
	!
	!			Ly:	type(dble). Longitud y del rectangle de la malla
	!
	!			nx:	type(int). Numero de punts en un fila de la malla
	!				(contorns inclosos)
	!
	!			ny: type(int). Numero de punts en una columna de la malla
	!				(contorns inclosos)
	!
	!			phi0: type(dble(array(nx,ny)). Matriu que correspon al grid
	!					de valors de sortida en que s'inicia els algoritmes
	!					de resolucio. (inclou les condicions de contorn i
	!					a dintre s'inicia amb valors randoms que creem
	!					fisicament possibles)
	!
	!			icontrol:	type(int). Indica quin dels metodes utilitzarem
	!						0 == Jacobi
	!						1 == Gauss-Seidel
	!						2 == sobre-relaxacio 
	!
	!			th:			type(dble). Es tracta de una threshold. Valor 
	!						absolut de la diferencia entre el valor de la 
	!						funcio phi en la iteracio k i la iteracio k+1.
	!
	!			w:			factor del metode de sobrerelaxacio
	!
	!			pos:		type:dble. Pos(1) component "x". Pos(2) component
	!						"y". Son les posicions en que volem veure com
	!						varia el seu valor conforme obtinm noves malles.
	!						Aquest es guarda al mateix fitxer.
	!						Recorda que son les posicions del grid, no pas
	!						les posicions reals.
	!
	!			num_file:	type(int). Es el numero que se l'hi assigna al
	!						fitxer quan l'obrim.
	!
	!			name_file:	type(character(100)). Nom del arxiu a on guardarem
	!						els valors de la solucio del problema.
	!
	!			is_file:	type(logical). "true" indica que el fitxer on
	!						volem esciure les dades ja esta obert.
	!
	!			is_close:	type(logical). "false" indica si volem tancar el
	!						fitxer un cop escrits els valors calculats.
	!
	!			ipunt:		type(logical). "true" indica que volem guardar
	!						com varia un punt. "false" el contrari

	implicit none

	double precision f
	integer nx,ny
	double precision Lx,Ly,phi0(nx,ny), th, x0,y0
	integer icontrol, num_file
	double precision w
	character(100) name_file
	logical is_file, is_close, ipunt
	integer pos(2)

	integer i,j
	double precision phik(nx,ny), phik1(nx,ny)
	double precision max_dif, h
	logical inicial
	integer counter

	! guardo les condicions incials aixi si les necessito en un altre problema
	! no es veuran afectades
	phik = phi0
	phik1 = phi0
	max_dif = 0.d0
	h = abs(Lx-x0)/(nx-1)
	inicial = .true.
	counter = 0



	! guardem les dades en un fitxer per despres fer un gnuplot
	if (is_file.eqv..false.) then
		open(num_file,file=name_file,status='unknown')
	end if

	if (is_file.eqv..true.) then
		write(num_file,*)
		write(num_file,*)
	end if


	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	! METODES


	! METODE DE JACOBI
	if (icontrol.eq.0) then

		do while ((max_dif.gt.th).or.(inicial.eqv..true.))
			inicial = .false.

			max_dif = 0.d0
			
			! calculem la k+1-esssima malla 
			do j = 2,ny-1
				do i = 2,nx-1
					phik1(i,j) = (phik(i+1,j)+phik(i-1,j)+phik(i,j+1) &
						+phik(i,j-1)+h**2*f(x0+h*(i-1),y0+h*(j-1)))/4.d0

					! calcul de la maxima diferencia en la malla
					if (abs(phik1(i,j)-phik(i,j)).gt.max_dif) then
						max_dif = abs(phik1(i,j)-phik(i,j))
					end if
				end do
			end do

			phik = phik1
			counter = counter+1 

			! control del numero de interacions
			if (mod(counter,100).eq.0) then
				write(*,*)"Jacobi: iteracio: ",counter
			end if

			! si volem podem guardar com evoluciona un punt amb el temps
			if ((ipunt.eqv..true.)) then
			! guardem un valor en cada iteracio
			write(num_file,*)counter,phik1(pos(1),pos(2))
			end if
		end do


		! 2 espais en blanc per gnuplot per seperar l'evolucio del punt
		! i els valors de la malla
		if (ipunt.eqv..true.) then
			write(num_file,*)
			write(num_file,*)
		end if
	end if

	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	! METODE DE GAUSS-SEIDEL
	if (icontrol.eq.1) then

		do while ((max_dif.gt.th).or.(inicial.eqv..true.))
			inicial = .false.

			! calculem la k+1-esssima malla
			max_dif = 0.d0
			do j = 2,ny-1
				do i = 2,nx-1
					phik1(i,j) = (phik(i+1,j)+phik1(i-1,j)+phik(i,j+1) &
							+phik1(i,j-1)+h**2*f(x0+h*(i-1),y0+h*(j-1)))/4.d0

					! calcul de la maxima diferencia en la malla
					if (abs(phik1(i,j)-phik(i,j)).gt.max_dif) then
						max_dif = abs(phik1(i,j)-phik(i,j))
					end if

				end do
			end do

			! actualitzo la malla
			phik = phik1

			! control del numero de interacions
			if (mod(counter,100).eq.0) then
				write(*,*)"Gauss-Seidel: iteracio: ",counter
			end if

			if ((ipunt.eqv..true.)) then
			! guardem un valor en cada iteracio
			write(num_file,*)counter,phik1(pos(1),pos(2))
			end if


			counter = counter+1 
		end do

		write(num_file,*)
		write(num_file,*)


	end if

	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	! METODE SOBRE-RELAXACIO
	if (icontrol.eq.2) then

		do while ((max_dif.gt.th).or.(inicial.eqv..true.))
			inicial = .false.
			max_dif = 0.d0
			
			! calculem la k+1-esssima malla 
			do j = 2,ny-1
				do i = 2,nx-1
					phik1(i,j) = phik(i,j) + (w*(phik(i+1,j)+phik1(i-1,j) &
								+phik(i,j+1)+phik1(i,j-1) -4.d0*phik(i,j) &
								+h**2*f(x0+h*(i-1),y0+h*(j-1))))/4.d0

					! calcul de la maxima diferencia en la malla
					if (abs(phik1(i,j)-phik(i,j)).gt.max_dif) then
						max_dif = abs(phik1(i,j)-phik(i,j))
					end if

				end do
			end do

			phik = phik1
			counter = counter+1 

			! control del numero de interacions
			if (mod(counter,100).eq.0) then
				write(*,*)"Sobrerelaxacio: iteracio: ",counter
			end if				

			if ((ipunt.eqv..true.)) then
			! guardem un valor en cada iteracio
			write(num_file,*)counter,phik1(pos(1),pos(2))
			end if

		end do

		write(num_file,*)
		write(num_file,*)

	end if


	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	! ESCRIVIM ELS VALORS FINALS DE LA MALLA EN EL FITXER
	do i=1,nx

		! un cop varia la x, escrivim una linea en blanc perque gnuplot ho
		! llegeixi correctament
		if (i.ne.1) then
			write(num_file,*)
		end if

		do j=1,ny
			
			! escrivim en el fitxer x,y,z
			write(num_file,*) x0+h*(i-1), y0+h*(j-1), phik1(i,j)

		end do
	end do


	! Si volem, tanquem el fitxer o el deixem obert per escriure altres calculs
	if (is_close.eqv..true.) then
		close(num_file)
	end if

	return
end subroutine edp_eliptica

double precision function rho(x,y)

	implicit none
	double precision x,y
	double precision r
	double precision rho10
	double precision rho1, rho2

	! Primer fogo
	r = sqrt((x-7.d0)**2+(y-8.d0)**2)
	rho10 = 1.33d0
	rho1 = rho10*exp(-(r-4.d0)**2/(0.4d0)**2)

	! Segon fogo
	if ((x.ge.20.d0).and.(x.le.24.d0).and.(y.gt.13.d0).and.(y.gt.15.d0)) then
		rho2 = 1.3d0
	else
		rho2 = 0.d0
	end if

	! suma dels 2 fogons
	rho = rho1 + rho2

	return
end function rho

double precision function zero(x,y)

	implicit none
	double precision x,y

	zero = 0.d0
	return
end function zero






