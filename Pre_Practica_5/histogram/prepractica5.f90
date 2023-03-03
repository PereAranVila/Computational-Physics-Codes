
program prepractica5

implicit none



end program prepractica5


subroutine acceptrebuig(ndat,xnums,a,b,M,fun)
	
	implicit none

	double precision a,b,M
	integer ndat

	double precision x1,x2,x,p,val_aux
	integer count

	! 2 nombres al atzar uniformement x en [a,b] i p en [0,M]
	call random_number(x1)
	call random_number(x2)
	x = (b-a)*x1 + a
	p = M*x2

	! si fun(x) >= p acceptem x fins tenir ndat nombres acceptats
	do while (count.NE.ndat)
		val_aux = fun(x)
		if (val_aux.GE.p) then
		


SUBROUTINE HISTOGRAMA(NDAT,XDATA,XA,XB,NBOX,XHIS,VHIS,ERRHIS,
    BOXSIZE,IERR)
       
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
        PRINT*,"ACCEPTED:",ICOUNT,
    	" OUT OF:",NDAT

        DO I=1,NBOX

           XHIS(I)=XA+BOXSIZE/2.D0+(I-1)*BOXSIZE

           ERRHIS(I)=SQRT(VHIS(I)/ICOUNT*(1.D0-VHIS(I)/ICOUNT))
           /BOXSIZE / SQRT(DBLE(ICOUNT))

           VHIS(I)=VHIS(I)/ICOUNT/BOXSIZE
        ENDDO
END SUBROUTINE HISTOGRAMA