! Prepràctica 1 03/10/2022

program fortran_i_gnuplot

implicit none

integer k
real func_pk, pk
real suma
integer n1, n2


k = 0

! EXERCICI 1)
! demanem el valor per pantalla
do while ((k<4).or.(k>522))
	
	write(*,*)"Escrigui un enter entre 4 i 522: "
	read(*,*) k
enddo

! calculem el valor de pk amb la funcio func_pk
pk = func_pk(k)

! escric per pantalla el valor de pk
write(*,*)"El valor de P_k es: ", pk



! EXERCICI 2)
! fem la suma desde n1=10 fins a n2=62 dels P_k (utilitzant una subrutina) 
suma = 0.
n1 = 10
n2 = 62

call sumatori(suma,n1,n2)

write(*,*)"El valor de la suma del apartat 2) es: ", suma



! EXERCICI 3)
! calculem les sumes que ens demana l'anunciat i escrivim en un fitxer
! per cada N2 el valor de la suma S
open(1,file='P1-22-23-res1.dat')
n1 = 11

do n2=12,332,2
	call sumatori(suma,n1,n2)
	write(1,*) n2, suma
enddo
close(1)

end program fortran_i_gnuplot




real function func_pk(k)
	
	implicit none

	integer k
	! recorda que aqui, lo mes correcte seria posar k com real(k)
	func_pk = (k**2 + k + 2)/2

	return
end

subroutine sumatori(suma,n1,n2)

	implicit none

	integer k, n1, n2
	real suma

	suma = 0

	do k= n1,n2,1
		suma = suma + ((real(k)**2) + real(k) + 2)/2
	enddo

	return
end 

