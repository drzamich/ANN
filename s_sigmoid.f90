real function sigmoid(x)
    implicit none
    real x

    sigmoid = 1/(1+exp(-1*(x)))
end function

real function sigmoidDerivative(x)
    implicit none
    real x

    sigmoidDerivative=exp(x)/(exp(x)+1)**2
end function

subroutine matrixSigmoid(matrixA,matrixB,rows,columns)
    implicit none
    integer rows, columns
    real matrixA(rows,columns)
    real matrixB(rows,columns)
    integer i,j
    real sigmoid

    do i=1,rows
        do j=1,columns
            matrixB(i,j) = sigmoid(matrixA(i,j))
        end do
    end do
end subroutine


subroutine matrixSigmoidDerivative(matrixA,matrixB,rows,columns)
    implicit none
    integer rows, columns
    real matrixA(rows,columns)
    real matrixB(rows,columns)
    integer i,j
    real sigmoidDerivative

    do i=1,rows
        do j=1,columns
            matrixB(i,j) = sigmoidDerivative(matrixA(i,j))
        end do
    end do
end subroutine
