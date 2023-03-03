program prova_comon

implicit none

double precision f1,f2, f3
double precision E1, E2
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

	common/constants/E1, E2

	x = 0.15d0

	E1 = 0.4d0
	E2 = 0.1d0

	val = f3(x)
	write(*,*)val
	return
end subroutine calcul



double precision function f1(x,y1,y2)

	implicit none
	double precision x,y1,y2
	common/constants/E1,E2

	f1 = (-2*E1+1.d0)*y1
	return
end function f1

double precision function f2(x,y1,y2)

	implicit none
	double precision x,y1,y2
	common/constants/E1,E2

	f2 = y2
	return
end function f2

double precision function f3(x)

	implicit none
	double precision x
	common/constants/E1, E2

	f3 = E1-E2 +x
	return
end function f3