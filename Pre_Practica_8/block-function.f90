program prova_comon

implicit none

double precision f1,f2, f3
double precision E1,E2

common/constants/E1,E2

external f3

E1 = 12.d0
E2 = 16.d0



call calcul(f3)

end program prova_comon

subroutine calcul(f3)

	implicit none
	double precision f3
	double precision val,x
	double precision E1, E2
	
	double precision val_aux

	common/constants/E1,E2

	x = 0.15d0

	E1 = 0.4d0
	E2 = 0.1d0

	val = f3(x)
	write(*,*)val

	call inside(val_aux,f3)
	write(*,*)val_aux
	return
end subroutine calcul

subroutine inside(val_aux,f3)

	implicit none
	double precision f3
	double precision val_aux

	val_aux = f3(2.d0)
	return
end subroutine inside

double precision function f3(x)

	implicit none
	double precision x
	double precision E1, E2

	common/constants/E1, E2

	f3 = E1-E2 +x
	return
end function f3


