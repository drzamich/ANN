subroutine differencesMatrix(matrixA,matrixB,matrixC,rows,columns)
    implicit none
    integer rows, columns
    integer i,j
    real matrixA(rows,columns)
    real matrixB(rows,columns)
    real matrixC(rows,columns)

    do i=1,rows
        do j=1,columns
            matrixC(i,j) = matrixA(i,j) - matrixB(i,j)
        end do
    end do
end subroutine
