! Practica 1. Pere Aran Vila 05/10/2022

program practica_1

implicit none

integer k
real pk, func_pk
integer m, n
real suma


! EXERCICI 1)
k = 0

! demano un valor de k tal que 4<k<301
do while ((k<5).or.(k>301))
	write(*,*)"Escriu un valor de k entre 5 i 301"
	read(*,*) k
enddo

pk = func_pk(k)
write(*,*) pk



! EXERCICI 2)
! fem la suma desde M=14 fins a N=70 dels P_k (utilitzant una subrutina) 
suma = 0.
m = 14
n = 70

call sumatori(suma,m,n)

write(*,*)"El valor de la suma del apartat 2) es: ", suma



! EXERCICI 3)
open(1,file='P1-22-23-res1.dat')
m = 3

do n=4,210,2
	call sumatori(suma,m,n)
	write(1,*) n, suma
enddo
close(1)

end program practica_1


real function func_pk(k)
	
	implicit none

	integer k
	real pi

	pi = 4*atan(1.0)

	func_pk = (5./3.)* (real(k)**2) + pi -2.*real(k)

	return
end

subroutine sumatori(suma,m,n)

	implicit none

	integer k, m, n
	real pi
	real suma

	pi = 4*atan(1.0)

	suma = 0.

	do k= m,n,1
		suma = suma + (5./3.)* (real(k)**2) + pi -2.*real(k)
	enddo

	return
end 