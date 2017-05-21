subroutine generateCheckingData
    use variables
    implicit none

    real top
    real bottom
    integer steps
    real x, x_norm
    real objective


    bottom=inputValues(1,1)
    top = inputValues(hiddenValuesRows,1)

    steps=int((top-bottom)/pointsChecking)+1


    open(10,file='comparison.txt')
    x=bottom
    do i=1,steps+2

        testValue(1,1) = (x-inputValuesParameters(1))/(inputValuesParameters(3)-inputValuesParameters(2))

        hiddenValuesChecking = matmul(testValue,inputToHiddenWeights)

        call matrixSigmoid(hiddenValuesChecking,hiddenValuesCheckingSigmoid,1,hiddenValuesColumns)

        additionalValuesChecking = matmul(hiddenValuesCheckingSigmoid,additionalLayerWeights)

        call matrixSigmoid(additionalValuesChecking,additionalValuesCheckingSigmoid,1,additionalLayerValuesColumns)

        outputValuesChecking = matmul(additionalValuesCheckingSigmoid,hiddenToOutputWeights)

        write(10,*) x, objective(x), outputValuesChecking(1,1)/factor

        x=x+checkingStep
    end do

    close(10)
end subroutine

