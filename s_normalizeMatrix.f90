subroutine normalizeMatirx(matrix,rows,columns)
    implicit none
    integer rows
    integer columns
    real suma, average, minValue, maxValue
    real matrix(rows,columns)
    real matrixArbitrary(rows,columns)
    integer i,j

    minValue = minval(matrix)
    maxValue = maxval(matrix)
    suma=sum(matrix)
    average=suma/(rows*columns)

    do i=1,rows
        do j=1,columns
            matrixArbitrary(i,j)=(matrix(i,j)-average)/(maxValue-minValue)
        end do
    end do

    matrix=matrixArbitrary
end subroutine

subroutine normalizeMatirxCheck
    implicit none
    real matrix(3,3)
    integer i,j,k

    k=1
    do i=1,3
        do j=1,3
            matrix(i,j) = k
            k=k+1
        end do
    end do

    call writeMatrix(matrix,3,3)
    call normalizeMatirx(matrix,3,3)
    call writeMatrix(matrix,3,3)
end subroutine
