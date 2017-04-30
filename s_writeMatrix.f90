
!---------------------
!
! THE SUBROUTINE FOR WRITING OUT A MATRIX IN A NICE WAY
! version 0.9
!
! TODO
! 1. convert integer value of n to string n_str so that it can be used in format declaration
!
!---------------------
! This subroutine is used for writing out matrices in a nice way


subroutine writeMatrix(A,d1,d2)

implicit none

! Defining variables
integer 			d1,d2				!size of the matrix A
integer 			i,j  			!loop indexes
real 				A(d1,d2)			!matrix to be written out
character(len=3) 	cell_width_str 	!character of the lenght 3 defines that the weidth of the cell has maximaly 3 characters
character(len=3) 	dec_places_str	!amount of the decimal places in the created matrix
character(len=3) 	n_str
integer 			cell_width_int
character(len=40) 	fmt				! format of the rows
integer 			row_separator_lenght
character(len=10000) row_separator

!
!      DEFINING THE FORMATTING
!

!Defining the cell width
cell_width_str='8'      !must be in ''

!Defining decimal places
dec_places_str='3'      !must be in ''


!trimming the strings so that they are fit for the format string
cell_width_str=trim(cell_width_str)
dec_places_str=trim(dec_places_str)

!creating integer value of cell width
read(cell_width_str,*) cell_width_int

! Defining string needed for separation of rows
row_separator = '-'

!Defining needed row separator lenght
row_separator_lenght = ((cell_width_int+2)*d2)-2

!Making the row-separation string as long as the matrix width
do i=1,row_separator_lenght
	row_separator = trim(row_separator)//'-'         !trim() function needed for removing blank places
enddo

!Defining the format of writing out rows
fmt ='("|", 1000(F'//cell_width_str//'.'//dec_places_str//', " |"))'       !this should be fixed so that instead of 1000 there would be a number of columns - n_str

!WRITING OUT THE MATRIX

!writing out the top border
write(*,*) trim(row_separator)

!Writing out rows
do i=1,d1
	write(*,fmt) (A(i,j), j=1,d2)      !writing out cell calues in a row
	write(*,*) trim(row_separator)    !writing out row separator after every row
enddo

end subroutine

