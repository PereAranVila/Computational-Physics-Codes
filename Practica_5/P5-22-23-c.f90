 
program practica5

implicit none

double precision p,g

! variables 1) exercici apartat a)
integer ndat1
double precision a1,b1,M1,xnums1(50000)
double precision L, pi 

double precision xhis1(100), vhis1(100), errorhis1(100), boxsize1
integer ierr1, ncaixes1

! variables 1) c)
double precision integral, x1, x2
integer k


! variables EXERCICI 2) a)
integer ndat2
double precision a2,b2,xnums2(20000)
double precision sigma, mu
logical is_seed

double precision xhis2(100), vhis2(100), errorhis2(100), boxsize2
integer ierr2, ncaixes2

double precision sum_x, x_mitja, sum_var, var_x, desvest_x



integer count


external p
external g

! EXERCICI 1)
! a)
L = 4.d0
pi = 4.d0*atan(1.d0)
ndat1 = 50000
a1 = -L*pi
b1 = L*pi
M1 = 1.d0/(L*pi)
ncaixes1 = 100

! generem els valors aleatoris segons la distribucio p(x)
call acceptrebuig(ndat1,xnums1,a1,b1,M1,p)
! obtenim el histograma
call histograma(ndat1,xnums1,a1,b1,ncaixes1,xhis1,vhis1,errorhis1,boxsize1, &
					ierr1)


! escrivim els valors en un fitxer
open(1,file='P5-22-23-res.dat')
do count=1,ncaixes1
	write(1,'(3e21.14)') xhis1(count), vhis1(count), errorhis1(count)
end do


! b)
! calcul de la probabilitat P1: x=[-L*pi, L*pi/2]
x1 = -L*pi
x2 = (L*pi)/2.d0
k = 12

write(1,*)
write(1,*)
call simpson(x1,x2,k,p,integral)
write(1,'(e21.14)')integral

x2 = L*pi
call simpson(x1,x2,k,p,integral)
write(1,'(e21.14)')integral


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! EXERCICI 2) a) BOX-MULLER
sigma = 3.d0
mu = 0.d0
is_seed = .true.
ndat2 = 20000
a2 = -4.d0*sigma
b2 = 4.d0*sigma
ncaixes2 = 100

! generem els valors aleatoris segons la distribucio p(x)
call Box_Muller(ndat2,xnums2,mu,sigma,is_seed)
! obtenim el histograma
call histograma(ndat2,xnums2,a2,b2,ncaixes2,xhis2,vhis2,errorhis2,boxsize2, &
					ierr2)


! escrivim els valors en un fitxer
write(1,*)
write(1,*)
do count=1,ncaixes2
	write(1,'(3e21.14)') xhis2(count), vhis2(count), errorhis2(count)
end do


! apartat b)
write(1,*)
write(1,*)
! calculs valor mitja, variancia i desviaccio estandard
! valor mitja
sum_x = 0.d0
do count=1,ndat2
    sum_x = sum_x + xnums2(count)
end do
x_mitja = sum_x/dble(ndat2)

! varianza
sum_var = 0.d0
do count=1,ndat2
    sum_var = sum_var + (xnums2(count) - x_mitja)**2
end do
var_x = sum_var/dble(ndat2)

! desviacio estandard
desvest_x = sqrt(var_x)

write(1,'(3e21.14)')x_mitja, var_x, desvest_x
write(1,'(3e21.14)')mu, sigma**2, sigma 
close(1)


end program practica5


double precision function p(x)

	implicit none

	double precision x
	double precision L, pi

	L = 4.d0
	pi = 4.d0*atan(1.d0)

	p = ((sin(x/L))**2)/(L*pi)
	return
end function p

double precision function g(x)

	implicit none

	double precision x
	double precision sigma, pi

	sigma = 3.d0
	pi = 4.d0*atan(1.d0)

	g = exp((-x**2)/(2.d0*sigma**2)) / sqrt(2.d0*pi*sigma**2)
	return
end function g

subroutine acceptrebuig(ndat,xnums,a,b,M,fun)

   ! Genera valors segons la distribucio donada (per fun) en el inteval [a,b]
   ! i retorna els valors aleatoris en un vector (xnums). Com a extra aquesta
   ! subrutina escriu per pantalla:
   !
   !     1) La proporccio acceptaccio/totals
   !     2) El Valor mitja
   !     3) La Variancia
   !     4) Desviaccio Estandard
   !
   !
   !     INPUTS   ndat        type: integer. Nombre de numeros aleatoris a 
   !                                generar.
   !
   !              a           type: dble. Inici del interval dels valors 
   !                                aleatoris generats.
   !
   !              b           type: dble. Final del interval dels valors 
   !                                aleatoris generats.
   !
   !              fun         type: dble (function). Funcio la qual volem
   !                                que els valors aleatoris que treiem a
   !                                l'atzar s'aproximi a la forma de la
   !                                funcio en el cas de fer un Histograma
   !
   !              M           type: dble. Cota d'acceptacio/rebuig (tipicament
   !                                s'escull com a valor, el maxim de la funcio
   !                                fun al interval en questio)
   !
   !
   !     OUTPUTS  xnums       type: dble xnums(ndat). Vector de valors 
   !                                aleatoris distribuits segons la funcio fun.

	
	implicit none

   integer ndat
	double precision a,b,M,xnums(ndat), fun

	double precision x1,x2,x,p,val_aux
	integer count, count_total

   double precision sum_x, sum_var, x_mitja, var_x, desvest_x

   integer ISEED

   ISEED = 17682733
   call srand(ISEED)


   count = 1
   count_total = 1
   do while (count.LE.ndat)
      ! 2 nombres al atzar uniformement x en [a,b] i p en [0,M]
      call random_number(x1)
      call random_number(x2)
      x = (b-a)*x1 + a
      p = M*x2

      ! si fun(x) >= p acceptem x fins tenir ndat nombres acceptats
      val_aux = fun(x)
      if (val_aux.GE.p) then
         xnums(count) = x
         count = count +1
      end if

      ! comptador per saber la proporcio total d'acceptats vs rebutjats
      count_total = count_total +1
   end do

   ! Proporcio de acceptats/totals
   write(*,*)"La proporcio d'acceptacció/totals = ", count/dble(count_total)


   ! calculs valor mitja, variancia i desviaccio estandard
   ! valor mitja
   sum_x = 0.d0
   do count=1,ndat
      sum_x = sum_x + xnums(count)
   end do
   x_mitja = sum_x/dble(ndat)
   write(*,*)"Valor mitja de la distribucio: ", x_mitja

   ! varianza
   sum_var = 0.d0
   do count=1,ndat
      sum_var = sum_var + (xnums(count) - x_mitja)**2
   end do
   var_x = sum_var/dble(ndat)
   write(*,*)"Varianza de la distribucio: ", var_x 

   ! desviacio estandard
   desvest_x = sqrt(var_x)
   write(*,*)"Desviaccio estandard de la distribucio:", desvest_x
end subroutine acceptrebuig

subroutine histograma(NDAT,XDATA,XA,XB,NBOX,XHIS,VHIS,ERRHIS,BOXSIZE,IERR)

   ! AQUESTA SUBROUTINA ESTA FET PER ELS PROFESSORS PERO JO L'HE ADAPTADA
   ! GENERA UN HISTOGRAMA NORMALITZAT A PARTIR DELS VALORS QUE TINGUEM
   ! GUARDATS COM A VECTOR (XDATA). LA RESTA DE ARGUMENTS L'EXPLICACIO ES
   ! CLARA AMB LA DOCUMENTACIO QUE HI HA.

   ! 
   ! INPUTS:   XDATA(NDAT)         ORIGINAL DATA
   !           XA                  LOWER LIMIT
   !           XB                  UPPER LIMIT
   !           NBOX                NUMBER OF BOXES
   !
   ! OUTPUTS:  XHIS                CENTRAL VALUE OF THE COLUMN
   !           VHIS                COLUMN VALUE
   !           ERRHIS              BINOMIAL ESTIMATE OF THE ERROR
   !           BOXSIZE             SIZE OF EACH BOX
   !           
   !           IERR                =0, EVERYTHING ok
   !                               =1, XA>XB
   !                               =2, IF THERE ARE NO POINTS IN THE INTERVAL



   IMPLICIT NONE

   INTEGER NDAT,NBOX
   DOUBLE PRECISION XDATA(NDAT),XA,XB
   DOUBLE PRECISION XHIS(NBOX),VHIS(NBOX),ERRHIS(NBOX)
   INTEGER IERR

   INTEGER I,IBOX,ICOUNT
   DOUBLE PRECISION BOXSIZE

   IF (XA.GE.XB) THEN 
      IERR=1
      RETURN
   ENDIF

   BOXSIZE=(XB-XA)/NBOX

   ICOUNT=0

   DO I=1,NBOX
      VHIS(I)=0
      ERRHIS(I)=0
   ENDDO

   DO I=1,NDAT
      IF (XDATA(I).GE.XA.AND.XDATA(I).LE.XB) THEN 
      IBOX=INT((XDATA(I)-XA)/BOXSIZE)+1
      IF (IBOX.EQ.NBOX+1) IBOX=NBOX 

      VHIS(IBOX)=VHIS(IBOX)+1
      ICOUNT=ICOUNT+1
      ENDIF
   ENDDO

   IF (ICOUNT.EQ.0) THEN 
      IERR=2
      RETURN
   ENDIF

   IERR=0
   WRITE(*,*)"HISTOGRAMA"
   PRINT*,"ACCEPTED:",ICOUNT, " OUT OF:",NDAT

   DO I=1,NBOX
      XHIS(I)=XA+BOXSIZE/2.D0+(I-1)*BOXSIZE
      ERRHIS(I)=SQRT(VHIS(I)/ICOUNT*(1.D0-VHIS(I)/ICOUNT)) &
      /BOXSIZE / SQRT(DBLE(ICOUNT))
      VHIS(I)=VHIS(I)/ICOUNT/BOXSIZE
   ENDDO
end subroutine histograma

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

subroutine Box_Muller(ndat,xnums,mu,sigma,is_seed)

   ! Genera un vector de valors aleatoris segons una densitat de probabilitat
   ! gaussiana amb una \mu i una \sigma determinada (pasades com a arguments).
   !
   ! NOTA EXTRA**: en el mètode de Box-Muller es generen 2 distribucions
   ! gaussianes indepedents a partir d'una distribucio uniforme de x1 i x2.
   ! En el nostre cas només agafem 1 d'aquestes distribucions i l'altre
   ! no la calculem.
   !
   



   implicit none

   integer ndat
   double precision xnums(ndat),mu,sigma

   double precision x1,x2,z,y,pi
   integer count

   integer ISEED
   logical is_seed

   pi = 4.d0*atan(1.d0)

   ! Aquesta part es per dir si vull que els numeros pseudoaleatoris varin
   ! cada cop. O que sigui sempre els mateixos

   if (is_seed.eqv..true.) then
      ISEED = 17682733
      call srand(ISEED)
   end if

   ! generem valors aleatoris segons la distribuccio de probabilitat gaussiana
   ! N(z1,0,1)
   ! Nota**: nomes em quedare amb una de les dues distribucions gaussianes que
   ! es poden generar.

   do count=1,ndat
      call random_number(x1)
      call random_number(x2)

      z = sqrt(-2.d0*log(x1))*cos(2.d0*pi*x2)

      ! passem de la N(z1,0,1) a una N(y,mu,sigma)
      y = mu + z*sigma
      xnums(count) = y
   end do
end subroutine Box_Muller


