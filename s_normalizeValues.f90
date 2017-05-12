subroutine normalizeValues(inputMatrix,outputMatrix,matrixSize,parameterMatrix)
    implicit none

    integer matrixSize
    real inputMatrix(matrixSize)
    real outputMatrix(matrixSize)
    real parameterMatrix(3)

    real suma
    real average
    real maxValue
    real minValue

    integer i
    suma=0

    do i=1,matrixSize
        suma=suma+inputMatrix(i)
    end do

    average=suma/matrixSize
    minValue = minval(inputMatrix)
    maxValue = maxval(inputMatrix)

    do i=1,matrixSize
        outputMatrix(i) = (inputMatrix(i)-average)/(maxValue-minValue)
    end do

    parameterMatrix(1) = average
    parameterMatrix(2) = minValue
    parameterMatrix(3) = maxValue

end subroutine


subroutine denormalizeValues(inputMatrix,outputMatrix,matrixSize,parameterMatrix)
    implicit none

    integer matrixSize
    real inputMatrix(matrixSize)
    real outputMatrix(matrixSize)
    real parameterMatrix(3)

    real suma
    real average
    real maxValue
    real minValue

    integer i

    average = parameterMatrix(1)
    minValue = parameterMatrix(2)
    maxValue = parameterMatrix(3)

    do i=1,matrixSize
        outputMatrix(i) = inputMatrix(i)*(maxValue-minValue)+average
    end do

end subroutine
