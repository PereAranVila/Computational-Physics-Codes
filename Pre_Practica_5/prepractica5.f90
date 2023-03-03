
program prepractica5

implicit none

double precision p

! variables apartat 1) c)
integer ndat1
double precision a1,b1,M1,xnums1(2000)
integer ncaixes1,iterr1
double precision x_his1(30),v_his1(30),errorhis1(30), boxsize1

integer count, count_total

! variables aparat 2)
integer ndat2
double precision xexpo(14000), xlambda

integer ncaixes2, iterr2
double precision x_his2(120),v_his2(120),errorhis2(120),boxsize2


external p

! APARTAT 1) c)
ndat1 = 2000
a1 = 0.d0
b1 = 4.d0*atan(1.d0)
M1 = 0.9d0
ncaixes1 = 30


! generem nombres aleatoris segons la distribucio de probabilitat de p(x)
call acceptrebuig(ndat1,xnums1,a1,b1,M1,p)   
call histograma(ndat1,xnums1,a1,b1,ncaixes1,x_his1,v_his1,errorhis1,boxsize1, &
                     iterr1)

! escric en un fitxer els valors de l'histograma resultant (normalitzats)
open(1,file='P5-22-23-res.dat')
do count=1,ncaixes1
   write(1,'(3e21.14)')x_his1(count), v_his1(count), errorhis1(count)
end do


! APARTAT 2
! b)
ndat2 = 14000
ncaixes2 = 120
xlambda = atan(1.d0)

! generem la distribucio exponencial demenada
call subexpo(ndat2,xlambda,xexpo)

! generem el histograma normalitzat de la distru exponencial
call histograma(ndat2,xexpo,0.d0,3.d0,ncaixes2,x_his2,v_his2,errorhis2, &
                  boxsize2, iterr2)

! escrivim els valors del histograma normalitzat en un fixer
write(1,*)
write(1,*)
do count=1,ncaixes2
   write(1,'(3e21.14)')x_his2(count), v_his2(count), errorhis2(count)
end do
close(1)

end program prepractica5



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


subroutine subexpo(ndat,xlambda,xexpo)

   ! Genera valors aleatoris segons una distribucio exponencial tal com
   ! p(x) = \lambda *e**(- \lambda *x)

   !     INPUTS:     ndat        type: integer. Nombre de valors aleatoris
   !                                   a generar.
   !
   !                 xlambda     type: dble. Valor de la constant \lambda
   !                                   de la distribucio exponencial
   !
   !     OUTPUTS:    xexpo       type: dble(xexpo(ndat)). Vector que conte
   !                                   els valors aleatoris generats segons
   !                                   la distribucio exponencial.



   implicit none

   integer ndat
   double precision xlambda, xexpo(ndat)

   integer count
   double precision x

   ! generem distribucio exponencial a partir del canvi de variable
   do count=1,ndat
      call random_number(x)
      xexpo(count) = (-1.d0/xlambda)*log(x)
   end do
end subroutine subexpo

double precision function p(x)
   
   ! OK, TESTEJADA

   implicit none

   double precision x
   double precision pi

   ! algunes constants per simplificar el problema
   pi = 4.d0*atan(1.d0)

   p = (9.d0/(2.d0*pi*(3.d0*pi**2-20.d0)))*x**3 * (sin(x))**3
   return
end function p


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

