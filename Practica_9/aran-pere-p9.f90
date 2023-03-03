
program practica9

implicit none

double precision rho, no_fonts
double precision Lx,Ly,x0,y0,phi10(135,183),phi120(135,183),phi1040(135,183),th
integer nx,ny
integer icontrol, num_file
logical is_file, is_close, ipunt
character(100) name_file
double precision w
integer pos(2)

integer i,j

external rho, no_fonts


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! PROBLEMA 1

! PRIMERA FONT DE CALOR, RHO1, metode GAUSS-SEIDEL

! RECORDA**: Que quan dissenyis la primera matriu inicial corresponent a la
! malla, si et donen h que es la distancia necessitaras L/h +1 punts.
! Es molt important aixo, sino els extrems no coincidiran. 


Lx = 33.5d0
Ly = 45.5d0
x0 = 0.d0
y0 = 0.d0
nx = 135
ny = 183
th = 0.05d0
num_file = 110
is_file = .false.
is_close = .false.
name_file = 'P9-22-23-res.dat'
w = 0.d0
ipunt = .true.
pos(1) = 31
pos(2) = 94

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! MALLA INICIAL (temperatura inicial central = 10 graus)

! generem la primera malla inicial
do i = 1,nx
	do j = 1,ny

		! condicions incials columna esquerra
		if (i.eq.1) then
			phi10(i,j) = 17.d0
		end if

		! condicions incials columna dreta
		if (i.eq.nx) then
			phi10(i,j) = 25.3d0
		end if

		! condicions inicials fila inferior
		if (j.eq.1) then
			phi10(i,j) = 0.5d0
		end if

		! condicions incials fila superior
		if (j.eq.ny) then
			phi10(i,j) = 11.2d0
		end if

		! incialitzo els punts interiors amb una T = 12 graus. 
		if ((i.ne.1).and.(i.ne.nx).and.(j.ne.1).and.(j.ne.ny)) then
			phi10(i,j) = 10.d0
		end if
	end do
end do


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! generem la primera malla inicial (T = 120 graus al interior)
do i = 1,nx
	do j = 1,ny

		! condicions incials columna esquerra
		if (i.eq.1) then
			phi120(i,j) = 17.d0
		end if

		! condicions incials columna dreta
		if (i.eq.nx) then
			phi120(i,j) = 25.3d0
		end if

		! condicions inicials fila inferior
		if (j.eq.1) then
			phi120(i,j) = 0.5d0
		end if

		! condicions incials fila superior
		if (j.eq.ny) then
			phi120(i,j) = 11.2d0
		end if

		! incialitzo els punts interiors amb una T = 12 graus. 
		if ((i.ne.1).and.(i.ne.nx).and.(j.ne.1).and.(j.ne.ny)) then
			phi120(i,j) = 120.d0
		end if
	end do
end do

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! generem la primera malla inicial (T = 1040 graus al interior)
do i = 1,nx
	do j = 1,ny

		! condicions incials columna esquerra
		if (i.eq.1) then
			phi1040(i,j) = 17.d0
		end if

		! condicions incials columna dreta
		if (i.eq.nx) then
			phi1040(i,j) = 25.3d0
		end if

		! condicions inicials fila inferior
		if (j.eq.1) then
			phi1040(i,j) = 0.5d0
		end if

		! condicions incials fila superior
		if (j.eq.ny) then
			phi1040(i,j) = 11.2d0
		end if

		! incialitzo els punts interiors amb una T = 12 graus. 
		if ((i.ne.1).and.(i.ne.nx).and.(j.ne.1).and.(j.ne.ny)) then
			phi1040(i,j) = 1040.d0
		end if
	end do
end do

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! resolem l'edp amb el metode de Gauss-Seidel (T = 10 graus)
icontrol = 1
th = 0.0001d0
call edp_eliptica(rho,x0,y0,Lx,Ly,nx,ny,phi10,icontrol,th,w,pos,num_file, &
					name_file,is_file,is_close,ipunt)

! sobre-relaxacio (T = 10)
is_file = .true.
w = 1.35d0
icontrol = 2
call edp_eliptica(rho,x0,y0,Lx,Ly,nx,ny,phi10,icontrol,th,w,pos,num_file, &
					name_file,is_file,is_close,ipunt)


th = 0.001d0
! Gauss-Seidel (T = 120 graus)
icontrol = 1
call edp_eliptica(rho,x0,y0,Lx,Ly,nx,ny,phi120,icontrol,th,w,pos,num_file, &
					name_file,is_file,is_close,ipunt)

! Sobre-relaxacio (T=120 graus)
icontrol = 2
call edp_eliptica(rho,x0,y0,Lx,Ly,nx,ny,phi120,icontrol,th,w,pos,num_file, &
					name_file,is_file,is_close,ipunt)


! Gauss-Seidel (T = 1040 graus)
icontrol = 1
call edp_eliptica(rho,x0,y0,Lx,Ly,nx,ny,phi1040,icontrol,th,w,pos,num_file, &
					name_file,is_file,is_close,ipunt)

! Sobre-relaxacio (T = 1040 graus)
icontrol = 2
call edp_eliptica(rho,x0,y0,Lx,Ly,nx,ny,phi1040,icontrol,th,w,pos,num_file, &
					name_file,is_file,is_close,ipunt)


! Cas sense fonts amb T = 10 graus i amb Gauss-Seidel
icontrol = 1
th = 0.0001d0
call edp_eliptica(no_fonts,x0,y0,Lx,Ly,nx,ny,phi10,icontrol,th,w,pos,num_file, &
					name_file,is_file,is_close,ipunt)




end program practica9

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! SUBRUTINES i FUNCIONS

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
	double precision rho1, rho2, rho3
	double precision r2,rho20
	double precision r3, rho30


	! primer fogo
	if ((x.ge.18).and.(x.le.22).and.(y.gt.29).and.(y.le.35)) then
		rho1 = 3.d0
	else
		rho1 = 0.d0
	end if


	! segon fogo
	r2 = sqrt((x-8.d0)**2 +(y-22.5d0)**2)
	rho20 = 10.d0

	rho2 = rho20*exp(-(r2-5.d0)**2 /(0.3d0**2))


	! tercer fogo
	r3 = sqrt((x-22.d0)**2 +(y-10.5d0)**2)
	rho30 = 6.d0

	rho3 = rho30*exp(-(r3-4.d0)**2 /(0.8d0**2))

	! suma dels fugons
	rho = rho1+rho2+rho3

	return
end function rho


double precision function no_fonts(x,y)

	implicit none
	double precision x,y

	no_fonts = 0.d0
	return
end function no_fonts


