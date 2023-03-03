program prova_read_file

implicit none

integer num_file, ncolum, nrow, pos_colum
double precision last_value
character(100) name_file

name_file = 'valors-RK4.dat'
num_file = 10
ncolum = 2
nrow = 21
pos_colum = 2


call read_line_value(name_file, num_file, ncolum, nrow, pos_colum, &
						last_value)

write(*,*)last_value

! segona prova
name_file = 'P3-22-23-res.dat'
num_file = 12
ncolum = 3
nrow = 18
pos_colum = 3

call read_line_value(name_file, num_file, ncolum, nrow, pos_colum, &
						last_value)

write(*,*)last_value

end program prova_read_file


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