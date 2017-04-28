real function sigmoid(x)
    implicit none
    real x

    sigmoid = 1/(1+exp(-1*(x)))
end function


subroutine matrixSigmoid(matrix,rows,columns)
    implicit none
    integer rows, columns
    real matrix(rows,columns)
    integer i,j
    real sigmoid

    do i=1,rows
        do j=1,columns
            matrix(i,j) = sigmoid(matrix(i,j))
        end do
    end do
end subroutine
