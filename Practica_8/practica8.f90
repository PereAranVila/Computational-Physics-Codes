
program practica8

implicit none

! funcions
double precision dphi, phi

! RK4
double precision x0,xf,y0(2),h
integer num_eq, num_file, n
character(100) name_file1
logical is_file, is_close

! constant E (common)
double precision E

! altres constants del problema
double precision hw, L

! variables extra metode de tir
double precision E1, E2
character(100) name_file2

! comptador
integer count

! variables per fer la suma
double precision x,nul,val_phi

common/constants/E

external dphi, phi

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! EXERCICI 1)

hw = 2.76041d0
L = 16.d0
x0 = -L/2.d0
xf = L/2.d0
num_eq = 2
num_file = 12
n = 500
h = abs(xf-x0)/dble(n)
name_file1 = 'P8-22-23-res-1.dat'
is_file = .false.
is_close = .false.

! valor E1
E = 0.43d0*hw
y0(1) = 10.d0**(-5)
y0(2) = 0.d0

call RK4(h,x0,xf,y0,num_eq,dphi,phi,phi,phi,phi,name_file1,num_file,is_file, &
			is_close)

! valor E2
is_file = .true.
E = 0.48d0*hw
y0(1) = 10.d0**(-5)
y0(2) =  0.d0

call RK4(h,x0,xf,y0,num_eq,dphi,phi,phi,phi,phi,name_file1,num_file,is_file, &
			is_close)

! valor E3
E = 1.4d0*hw
y0(1) = 10.d0**(-5)
y0(2) =  0.d0

call RK4(h,x0,xf,y0,num_eq,dphi,phi,phi,phi,phi,name_file1,num_file,is_file, &
			is_close)

! valor E4
is_close = .true.
E = 1.45d0*hw
y0(1) = 10.d0**(-5)
y0(2) =  0.d0

call RK4(h,x0,xf,y0,num_eq,dphi,phi,phi,phi,phi,name_file1,num_file,is_file, &
			is_close)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! EXERCICI 2)

! 1r autovalor
is_file = .false.
is_close = .false.
E1 = 0.43d0*hw
E2 = 0.48d0*hw
name_file2 = 'P8-22-23-res-2.dat'
num_file = 15
n = 500

call metode_tir(E1,E2,dphi,phi,n,num_file,name_file2,is_file,is_close)

! 2n autovalor
is_file = .true.
is_close = .false.
E1 = 1.4d0*hw
E2 = 1.45d0*hw

call metode_tir(E1,E2,dphi,phi,n,num_file,name_file2,is_file,is_close)

! 3r autovalor
is_file = .true.
is_close = .true.
E1 = 2.d0*hw
E2 = 2.1d0*hw

call metode_tir(E1,E2,dphi,phi,n,num_file,name_file2,is_file,is_close)

! no em dona temps a normalitzar les funcions propies phi(x), represento
! les funcions phi sense normalitzar


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! EXERCICI 3)


end program practica8


! funcions 
double precision function dphi(x,y1,y2)

	! y2 es phi
	! y1 es dphi/dx

	implicit none
	double precision x,y1,y2
	double precision E, V
	common/constants/E

	V = (1.d0/2.d0)*1.d0*(x**2)

	dphi = (1.d0/3.80995d0)*(-E+V)*y2
	return
end function dphi

double precision function phi(x,y1,y2)

	! y2 es phi
	! y1 es dphi/dx

	implicit none
	double precision x,y1,y2
	
	phi = y1
	return
end function phi

! subrutines
subroutine RK4(h,x0,xf,y0,num_eq,f1,f2,f3,f4,f5,name_file,num_file, &
					is_file,is_close)

	! Resol el sistema d'equacions diferencials proposat utilitzant un RK4.
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

	double precision k1,k2,k3,k4 
	double precision l1,l2,l3,l4
	double precision m1,m2,m3,m4
	double precision n1,n2,n3,n4
	double precision o1,o2,o3,o4

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
	! ALGORITME RK4

	! CAS 1 EQ.DIF. DE PRIMER ORDRE
	if (num_eq.eq.1) then
		! ALGORITME RK4
		yn1 = y0(1)
		xn = x0

		! guardem les condicions incials al fitxer
		write(num_file,*)xn,yn1

		do count = 1,i
			k1 = h*f1(xn,yn1)
			k2 = h*f1(xn+h/2.d0,yn1+(k1/2.d0))
			k3 = h*f1(xn+h/2.d0,yn1+(k2/2.d0))
			k4 = h*f1(xn+h,yn1+k3)

			yn1 = y0(1) + (1.d0/6.d0)*(k1+2.d0*k2+2.d0*k3+k4)

			xn = x0 + count*h

			! guardem els valors en el fitxer
			write(num_file,*) xn, yn1

			y0(1) = yn1
		end do
	end if


	! CAS 2 EQ.DIF. DE PRIMER ORDRE
	if (num_eq.eq.2) then
		! ALGORITME RK4
		yn1 = y0(1)
		yn2 = y0(2)
		xn = x0

		! guardem les condicions incials al fitxer
		write(num_file,*)xn,yn1,yn2

		do count = 1,i
			k1 = h*f1(xn,yn1,yn2)
			l1 = h*f2(xn,yn1,yn2)

			k2 = h*f1(xn+h/2.d0,yn1+(k1/2.d0),yn2+(l1/2.d0))
			l2 = h*f2(xn+h/2.d0,yn1+(k1/2.d0),yn2+(l1/2.d0))

			k3 = h*f1(xn+h/2.d0,yn1+(k2/2.d0),yn2+(l2/2.d0))
			l3 = h*f2(xn+h/2.d0,yn1+(k2/2.d0),yn2+(l2/2.d0))

			k4 = h*f1(xn+h,yn1+k3,yn2+l3)
			l4 = h*f2(xn+h,yn1+k3,yn2+l3)

			yn1 = y0(1) + (1.d0/6.d0)*(k1+2.d0*k2+2.d0*k3+k4)
			yn2 = y0(2) + (1.d0/6.d0)*(l1+2.d0*l2+2.d0*l3+l4)
			
			xn = x0 + count*h

			! guardem els valors en el fitxer
			write(num_file,*) xn, yn1, yn2

			y0(1) = yn1
			y0(2) = yn2
		end do
	end if


	! CAS 3 EQ.DIF. DE PRIMER ORDRE
	if (num_eq.eq.3) then
		! ALGORITME RK4
		yn1 = y0(1)
		yn2 = y0(2)
		yn3 = y0(3)
		xn = x0

		! guardem les condicions incials al fitxer
		write(num_file,*)xn,yn1,yn2,yn3

		do count = 1,i
			k1 = h*f1(xn,yn1,yn2,yn3)
			l1 = h*f2(xn,yn1,yn2,yn3)
			m1 = h*f3(xn,yn1,yn2,yn3)

			k2 = h*f1(xn+h/2.d0,yn1+(k1/2.d0),yn2+(l1/2.d0),yn3+(m1/2.d0))
			l2 = h*f2(xn+h/2.d0,yn1+(k1/2.d0),yn2+(l1/2.d0),yn3+(m1/2.d0))
			m2 = h*f3(xn+h/2.d0,yn1+(k1/2.d0),yn2+(l1/2.d0),yn3+(m1/2.d0))

			k3 = h*f1(xn+h/2.d0,yn1+(k2/2.d0),yn2+(l2/2.d0),yn3+(m2/2.d0))
			l3 = h*f2(xn+h/2.d0,yn1+(k2/2.d0),yn2+(l2/2.d0),yn3+(m2/2.d0))
			m3 = h*f3(xn+h/2.d0,yn1+(k2/2.d0),yn2+(l2/2.d0),yn3+(m2/2.d0))

			k4 = h*f1(xn+h,yn1+k3,yn2+l3,yn3+m3)
			l4 = h*f2(xn+h,yn1+k3,yn2+l3,yn3+m3)
			m4 = h*f3(xn+h,yn1+k3,yn2+l3,yn3+m3)

			yn1 = y0(1) + (1.d0/6.d0)*(k1+2.d0*k2+2.d0*k3+k4)
			yn2 = y0(2) + (1.d0/6.d0)*(l1+2.d0*l2+2.d0*l3+l4)
			yn3 = y0(3) + (1.d0/6.d0)*(m1+2.d0*m2+2.d0*m3+m4)


			xn = x0 + count*h

			! guardem els valors en el fitxer
			write(num_file,*) xn, yn1, yn2, yn3

			y0(1) = yn1
			y0(2) = yn2
			y0(3) = yn3
		end do
	end if


	! CAS 4.EQ.DIF DE PRIMER ORDRE
	if (num_eq.eq.4) then
		! ALGORITME RK4
		yn1 = y0(1)
		yn2 = y0(2)
		yn3 = y0(3)
		yn4 = y0(4)
		xn = x0

		! guardem les condicions incials al fitxer
		write(num_file,*)xn,yn1,yn2,yn3,yn4

		do count = 1,i
			k1 = h*f1(xn,yn1,yn2,yn3,yn4)
			l1 = h*f2(xn,yn1,yn2,yn3,yn4)
			m1 = h*f3(xn,yn1,yn2,yn3,yn4)
			n1 = h*f4(xn,yn1,yn2,yn3,yn4)

			k2 = h*f1(xn+h/2.d0,yn1+(k1/2.d0),yn2+(l1/2.d0),yn3+(m1/2.d0), &
						yn4+(n1/2.d0))
			l2 = h*f2(xn+h/2.d0,yn1+(k1/2.d0),yn2+(l1/2.d0),yn3+(m1/2.d0), &
						yn4+(n1/2.d0))
			m2 = h*f3(xn+h/2.d0,yn1+(k1/2.d0),yn2+(l1/2.d0),yn3+(m1/2.d0), &
						yn4+(n1/2.d0))
			n2 = h*f4(xn+h/2.d0,yn1+(k1/2.d0),yn2+(l1/2.d0),yn3+(m1/2.d0), &
						yn4+(n1/2.d0))

			k3 = h*f1(xn+h/2.d0,yn1+(k2/2.d0),yn2+(l2/2.d0),yn3+(m2/2.d0), &
						yn4+(n2/2.d0))
			l3 = h*f2(xn+h/2.d0,yn1+(k2/2.d0),yn2+(l2/2.d0),yn3+(m2/2.d0), &
						yn4+(n2/2.d0))
			m3 = h*f3(xn+h/2.d0,yn1+(k2/2.d0),yn2+(l2/2.d0),yn3+(m2/2.d0), &
						yn4+(n2/2.d0))
			n3 = h*f4(xn+h/2.d0,yn1+(k2/2.d0),yn2+(l2/2.d0),yn3+(m2/2.d0), &
						yn4+(n2/2.d0))

			k4 = h*f1(xn+h,yn1+k3,yn2+l3,yn3+m3,yn4+n3)
			l4 = h*f2(xn+h,yn1+k3,yn2+l3,yn3+m3,yn4+n3)
			m4 = h*f3(xn+h,yn1+k3,yn2+l3,yn3+m3,yn4+n3)
			n4 = h*f4(xn+h,yn1+k3,yn2+l3,yn3+m3,yn4+n3)


			yn1 = y0(1) + (1.d0/6.d0)*(k1+2.d0*k2+2.d0*k3+k4)
			yn2 = y0(2) + (1.d0/6.d0)*(l1+2.d0*l2+2.d0*l3+l4)
			yn3 = y0(3) + (1.d0/6.d0)*(m1+2.d0*m2+2.d0*m3+m4)
			yn4 = y0(4) + (1.d0/6.d0)*(n1+2.d0*n2+2.d0*n3+n4)
			

			xn = x0 + count*h

			! guardem els valors en el fitxer
			write(num_file,*) xn, yn1, yn2, yn3, yn4

			y0(1) = yn1
			y0(2) = yn2
			y0(3) = yn3
			y0(4) = yn4
		end do
	end if


	! CAS 5 EQ.DIF. DE PRIMER ORDRE
	if (num_eq.eq.5) then
		! ALGORITME RK4
		yn1 = y0(1)
		yn2 = y0(2)
		yn3 = y0(3)
		yn4 = y0(4)
		yn5 = y0(5)
		xn = x0

		! guardem les condicions incials al fitxer
		write(num_file,*)xn,yn1,yn2,yn3,yn4,yn5

		do count = 1,i
			k1 = h*f1(xn,yn1,yn2,yn3,yn4,yn5)
			l1 = h*f2(xn,yn1,yn2,yn3,yn4,yn5)
			m1 = h*f3(xn,yn1,yn2,yn3,yn4,yn5)
			n1 = h*f4(xn,yn1,yn2,yn3,yn4,yn5)
			o1 = h*f5(xn,yn1,yn2,yn3,yn4,yn5)


			k2 = h*f1(xn+h/2.d0,yn1+(k1/2.d0),yn2+(l1/2.d0),yn3+(m1/2.d0), &
						yn4+(n1/2.d0), yn5+(o1/2.d0))
			l2 = h*f2(xn+h/2.d0,yn1+(k1/2.d0),yn2+(l1/2.d0),yn3+(m1/2.d0), &
						yn4+(n1/2.d0), yn5+(o1/2.d0))
			m2 = h*f3(xn+h/2.d0,yn1+(k1/2.d0),yn2+(l1/2.d0),yn3+(m1/2.d0), &
						yn4+(n1/2.d0), yn5+(o1/2.d0))
			n2 = h*f4(xn+h/2.d0,yn1+(k1/2.d0),yn2+(l1/2.d0),yn3+(m1/2.d0), &
						yn4+(n1/2.d0), yn5+(o1/2.d0))
			o2 = h*f5(xn+h/2.d0,yn1+(k1/2.d0),yn2+(l1/2.d0),yn3+(m1/2.d0), &
						yn4+(n1/2.d0), yn5+(o1/2.d0))

			k3 = h*f1(xn+h/2.d0,yn1+(k2/2.d0),yn2+(l2/2.d0),yn3+(m2/2.d0), &
						yn4+(n2/2.d0), yn5+(o2/2.d0))
			l3 = h*f2(xn+h/2.d0,yn1+(k2/2.d0),yn2+(l2/2.d0),yn3+(m2/2.d0), &
						yn4+(n2/2.d0), yn5+(o2/2.d0))
			m3 = h*f3(xn+h/2.d0,yn1+(k2/2.d0),yn2+(l2/2.d0),yn3+(m2/2.d0), &
						yn4+(n2/2.d0), yn5+(o2/2.d0))
			n3 = h*f4(xn+h/2.d0,yn1+(k2/2.d0),yn2+(l2/2.d0),yn3+(m2/2.d0), &
						yn4+(n2/2.d0), yn5+(o2/2.d0))
			o3 = h*f5(xn+h/2.d0,yn1+(k2/2.d0),yn2+(l2/2.d0),yn3+(m2/2.d0), &
						yn4+(n2/2.d0), yn5+(o2/2.d0))

			k4 = h*f1(xn+h,yn1+k3,yn2+l3,yn3+m3,yn4+n3,yn5+o3)
			l4 = h*f2(xn+h,yn1+k3,yn2+l3,yn3+m3,yn4+n3,yn5+o3)
			m4 = h*f3(xn+h,yn1+k3,yn2+l3,yn3+m3,yn4+n3,yn5+o3)
			n4 = h*f4(xn+h,yn1+k3,yn2+l3,yn3+m3,yn4+n3,yn5+o3)
			o4 = h*f4(xn+h,yn1+k3,yn2+l3,yn3+m3,yn4+n3,yn5+o3)



			yn1 = y0(1) + (1.d0/6.d0)*(k1+2.d0*k2+2.d0*k3+k4)
			yn2 = y0(2) + (1.d0/6.d0)*(l1+2.d0*l2+2.d0*l3+l4)
			yn3 = y0(3) + (1.d0/6.d0)*(m1+2.d0*m2+2.d0*m3+m4)
			yn4 = y0(4) + (1.d0/6.d0)*(n1+2.d0*n2+2.d0*n3+n4)
			yn5 = y0(5) + (1.d0/6.d0)*(o1+2.d0*o2+2.d0*o3+o4)
			

			xn = x0 + count*h

			! guardem els valors en el fitxer
			write(num_file,*) xn, yn1, yn2, yn3, yn4, yn5

			y0(1) = yn1
			y0(2) = yn2
			y0(3) = yn3
			y0(4) = yn4
			y0(5) = yn5
		end do
	end if
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	! GESTIO TANCAMENT DEL ARXIU GENERAT
	! si volem tanquem el arxiu
	if (is_close.eqv..true.) then
		close(num_file)
	end if
end subroutine RK4

subroutine metode_tir(E1,E2,f1,f2,n,num_file,name_file,is_file,is_close)

	! Realitza el metode de tir. Nomes es valida per 2 equacions dierencials
	! de segon ordre. Si es voles per mes equacions s'hauria de modificar
	! lleugerament.
	!
	! Args*:	E1:		valor inicial de l'energia en que busquem el VAP
	!
	!			E2:		valor final de l'energia en que busquem el VAP
	!			
	!			f1:		primera equaccio diferencial de primer ordre
	!
	!			f2:		segona equacció diferencial de segon ordre
	!
	!			y0:		vector de 2 dimensions on hi han les condicions
	!					inicials per ordre.
	!
	!			n:		numero de passos a resoldre l'eq.dif. amb RK4.
	!
	!			num_file:	numero amb el qual s'assignara el arxiu on
	!						es guardara el resultat
	!			
	!			name_file:	nom del arxiu on es guardaran els resultats.
	!
	!			is_file:	"true" si ja existeix i volem guardar els resultats
	!						a continuacio. "false" si el arxiu encara no
	!						existeix
	!
	!			is_close:	"true" si volem tancar el arxiu. Del contrari,
	!						"false" el deixa obert			
	!
	!	NOTA*: quan s'utilitzi, s'ha de ficar les condicions inicials
	!			y0 corresponent al problema que resolem. Tambe, s'ha de
	!			modificiar x0 i xf.

	implicit none

	! funcions dy/dx de les eq.dif.
	double precision f1,f2

	double precision E1,E2
	double precision E3
	integer n

	! variables per el RK4 (s'han de modificar els valors segons el problema)
	double precision h,x0,xf,y0(2)
	integer num_file_E1, num_file_E2,num_file_E3,num_eq
	character(100) name_file_E1, name_file_E2, name_file_E3
	logical is_file_E, is_close_E

	! variables que corresponen al valor de l'eq.dif. en x=1
	double precision val_E1, val_E2, val_E3

	! variables que corresponen a on es voldra escriure el resultat final
	integer num_file
	character(100) name_file
	logical is_file, is_close

	! comptador per tal de saber les iteracions que porto
	integer count

	! common block de E
	double precision E
	common/constants/E

	external f1,f2

	! parametres dels RK4 que utilitzarem
	x0 = -8.d0
	xf = 8.d0
	h = abs(x0-xf)/dble(n)
	num_file_E1 = 114
	num_file_E2 = 115
	num_file_E3 = 76
	num_eq = 2
	name_file_E1 = 'E1.dat'
	name_file_E2 = 'E2.dat'
	name_file_E3 = 'E3.dat'
	is_file_E = .false.
	is_close_E = .true.


	! incialitzo el contador
	count = 0

	! donc un valor random mes gran que el criteri de convergencia per tal de
	! inicialitzar el metode
	val_E3 = 1000.d0

	! obrim un arxiu on guardare la convergencia del metode
	open(28,file='convergencia-ex-2.dat')
	write(28,*)
	write(28,*)

	! METODE DE TIR
	do while (abs(val_E3).gt.10.d0**(-8))

		! resolem les eq.dif. per RK4 per E1 i E2
		E = E1
		y0(1) = 10.d0**(-5)
		y0(2) =  0.d0
		call RK4(h,x0,xf,y0,num_eq,f1,f2,f2,f2,f2,name_file_E1,num_file_E1,is_file_E, &
				is_close_E)

		E = E2
		! haig de tornar a escriure les condicions inicials ja que el rk4 si
		! prove de un rk4 anterior, aquest modifica les condicions inicials.
		y0(1) = 10.d0**(-5)
		y0(2) =  0.d0
		call RK4(h,x0,xf,y0,num_eq,f1,f2,f2,f2,f2,name_file_E2,num_file_E2,is_file_E, &
				is_close_E)	


		! valors de la solucio de l'eq.dif. per E1 i E2 en x=1
		call read_line_value(name_file_E1,12,3,n+1,3,val_E1)
		call read_line_value(name_file_E2,12,3,n+1,3,val_E2)

		! calcul de E3
		E3 = (E1*val_E2 -E2*val_E1)/(val_E2-val_E1)

		! calculem l'eq.dif. per al valor de E3
		E = E3
		y0(1) = 10.d0**(-5)
		y0(2) =  0.d0
		call RK4(h,x0,xf,y0,num_eq,f1,f2,f2,f2,f2,name_file_E3,num_file_E3,is_file_E, &
				is_close_E)

		! valor de l'eq.dif per E3 en x=1
		call read_line_value(name_file_E3,12,3,n+1,3,val_E3)

		! si ha convergit escrivim els resultats en el fitxer que indiquem
		if (abs(val_E3).lt.10.d0**(-8)) then
			y0(1) = 10.d0**(-5)
			y0(2) =  0.d0
			call RK4(h,x0,xf,y0,num_eq,f1,f2,f2,f2,f2,name_file,num_file,is_file, &
					is_close)

			! imprimim els valor de E per pantalla
			write(*,*)"Valor de E que es solucio= ", E3
		end if

		! guardem els valors de l'energia per veure la convegencia del metode
		! aixo ho fem en un fitxer apart
		write(28,*)count+1,E3

		! si encara no ha convergit recalculem les energies E1 i E2
		if (abs(val_E3).gt.10.d0**(-8)) then
			E1 = E2
			E2 = E3
		end if 
	
		! controlo el bucle while true per saber la iteracio en que estem
		count = count +1
		if (mod(count,25).eq.0) then
			write(*,*)"Metode de tir, iteracio numero: ",count
		end if

	end do
end subroutine metode_tir

subroutine read_line_value(name_file, num_file, ncolum, nrow, pos_column, &
				 last_value)

	! Obte en memoria el valor de la fila i la columna que li diguem d'un
	! fitxer de dades. IMPORTANT: esta dissenyada per un fitxer on hi hagi un
	! nombre fix de columnes. 
	!
	! Args*: 	name_file:	type character. Nom del arxiu a obrir.
	!
	!			num_file: type integer. Valor amb el que s'assigna el fitxer.
	!						ex: open(num_file,file=name_file)
	!
	!			ncolum:	type integer. Nombre de columnes del fitxer.
	!
	!			nrow:	type integer. Fila en el qual esta el valor que volem
	!					guardar en memoria. (La primera fila=1)
	!
	!			pos_column:	type integer. Columna en el qual esta el valor que
	!						volem guardar en memoria. (La primera columna=1)
	!
	!			last_value: type dble. Valor el qual voliem llegir i guardar
	!						en memoria.
	

	implicit none

	integer num_file, ncolum, nrow, pos_column
	double precision last_row(ncolum)
	double precision last_value
	character(100) name_file

	integer i

	open(num_file,file=name_file,status='old')

	if (ncolum.eq.1) then
		do i = 1,nrow
			read(num_file,*)last_row(1)
		end do
	end if
		
	if (ncolum.eq.2) then
		do i = 1,nrow
			read(num_file,*)last_row(1),last_row(2)
		end do
	end if

	if (ncolum.eq.3) then
		do i = 1,nrow
			read(num_file,*)last_row(1),last_row(2), last_row(3)
		end do
	end if

	if (ncolum.eq.4) then
		do i = 1,nrow
			read(num_file,*)last_row(1),last_row(2), last_row(3), last_row(4)
		end do
	end if

	if (ncolum.eq.5) then
		do i = 1,nrow
			read(num_file,*)last_row(1),last_row(2), last_row(3), last_row(4), &
								last_row(5)
		end do
	end if	


	last_value = last_row(pos_column)

	close(num_file)

	return
end subroutine read_line_value



