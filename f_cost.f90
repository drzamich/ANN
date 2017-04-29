subroutine costFunction(matrixA,matrixB,rows,columns,cost)
    implicit none
    real cost
    integer rows, columns
    integer i,j
    real matrixA(rows,columns)
    real matrixB(rows,columns)

    cost=0.0

    do i=1,rows
        do j=1,columns
            cost=cost+0.5*((matrixA(i,j)-matrixB(i,j))**2)
        end do
    end do
end subroutine
