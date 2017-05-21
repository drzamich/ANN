subroutine trainingPreparation
    use variables
    implicit none

    !reading important parameters from file input.txt
    call readInputFile

    !allocating matrices needed in the algorithm
    call allocateMatrices

    !assigning random weights in the range given in the input file
    !to matrices with weights
    call assignRandomWeights(inputToHiddenWeights,&
                            inputToHiddenWeightsRows,inputToHiddenWeightsColumns,&
                            lowerRandomWeightValue,upperRandomWeightValue)

    call assignRandomWeights(hiddenToOutputWeights,&
                            hiddenToOutputWeightsRows,hiddenToOutputWeightsColumns,&
                            lowerRandomWeightValue,upperRandomWeightValue)

    call assignRandomWeights(additionalLayerWeights,&
                            additionalLayerWeightsRows,additionalLayerWeightsColumns,&
                            lowerRandomWeightValue,upperRandomWeightValue)
end subroutine

subroutine displayTrainingData
    use variables
    implicit none

    !normalizing training input data to the range -0.5 - 0.5; saving the parameters (min,max,average) to parameter array
    call normalizeValues(inputValues,inputValuesNormalized,inputDataRows,inputValuesParameters)


    !displaying training input data on the screen
    write(*,*) "Input values"
    call writeMatrix(inputValues,inputDataRows,inputDataColumns)

    !displaying training output data on the screen
    write(*,*)
    write(*,*) "(Expected) output values"
    call writeMatrix(outputValuesExpected,outputDataRows,outputDataColumns)


end subroutine

subroutine training
    use variables
    implicit none
    real arbitraryMatrix(3)

!
! COMPARING ACTUAL EXPECTED VALUES WITH THE NET VALUES OF OUTPUT LAYER
!

do i=1,iterationSteps
    !calculating net values of hidden layer
    hiddenValues = matmul(inputValuesNormalized,inputToHiddenWeights)

    !sigmoiding net values of hidden layer
    call matrixSigmoid(hiddenValues,hiddenValuesSigmoid,hiddenValuesRows,hiddenValuesColumns)

    additionalLayerValues=matmul(hiddenValuesSigmoid,additionalLayerWeights)

    call matrixSigmoid(additionalLayerValues,additionalLayerValuesSigmoid,additionalLayerValuesRows,additionalLayerValuesColumns)

    !calculating net values of net output layer
    outputValues = matmul(additionalLayerValuesSigmoid,hiddenToOutputWeights)

    call normalizeValues(outputValues,outputValuesNormalized,inputDataRows,arbitraryMatrix)

    !sigmoiding output values
    call matrixSigmoid(outputValuesNormalized,outputValuesSigmoid,outputDataRows,outputDataColumns)

    !calculating the derivatives hidden->output layer
    !calculating values of sigmoid derivative function of net output layer
    call matrixSigmoidDerivative(outputValuesNormalized,outputValuesSigmoidDerivative,outputDataRows,outputDataColumns)

    delta3 = (-1)*(outputValuesExpected-outputValues)*outputValuesSigmoidDerivative

    hiddenToOutputDerivative = matmul(transpose(additionalLayerValuesSigmoid),delta3)


    !calculating the derivatives hidden->additional layer
    !sigmoid derivative
    call matrixSigmoidDerivative(additionalLayerValues,additionalLayerValuesSigmoidDerivative,&
                                    additionalLayerValuesRows,additionalLayerValuesColumns)

    deltaAdditional = matmul(delta3,transpose(hiddenToOutputWeights))*additionalLayerValuesSigmoidDerivative

    additionalLayerWeightsDerivative = matmul(transpose(hiddenValuesSigmoid),deltaAdditional)

    !calculating the derivatives input->hidden layer
    !calculating values of sigmoid derivative function of net output layer
    call matrixSigmoidDerivative(hiddenValues,hiddenValuesSigmoidDerivatives,hiddenValuesRows,hiddenValuesColumns)

    delta2 = matmul(deltaAdditional,transpose(additionalLayerWeights))*hiddenValuesSigmoidDerivatives

    inputToHiddenDerivative = matmul(transpose(inputValuesNormalized),delta2)

    !learning rate
    step=0.3

    !correcting the weights' values
    inputToHiddenWeights = inputToHiddenWeights -step*inputToHiddenDerivative
    hiddenToOutputWeights = hiddenToOutputWeights -step*hiddenToOutputDerivative
    additionalLayerWeights = additionalLayerWeights -step*additionalLayerWeightsDerivative

    !calculating the cost of the function
    call costFunction(outputValues,outputValuesExpected,outputDataRows,outputDataColumns,cost)

    costValues(i) = cost

    !notyfying user in which iteration step the program currently is
    if(mod(i,stepIndicatorProgram)==0) then
        write(*,*) "Step", i
    end if

    !displaying the value of net output layer in the last iteration step
    if(i==iterationSteps) then

        write(*,*) "Input -> 1st hidden weights:"
        call writeMatrix(inputToHiddenWeights,inputToHiddenWeightsRows,inputToHiddenWeightsColumns)

        write(*,*) "Input -> 1st hidden weights derivative:"
        call writeMatrix(inputToHiddenDerivative,inputToHiddenWeightsRows,inputToHiddenWeightsColumns)

        write(*,*) "1st hidden layer values:"
        call writeMatrix(hiddenValues,hiddenValuesRows,hiddenValuesColumns)

        write(*,*) "1st -> 2nd hidden weights:"
        call writeMatrix(additionalLayerWeights,additionalLayerWeightsRows,additionalLayerWeightsColumns)

        write(*,*) "1st -> 2nd hidden weights derivative:"
        call writeMatrix(additionalLayerWeightsDerivative,additionalLayerWeightsRows,additionalLayerWeightsColumns)

        write(*,*) "2nd hidden values:"
        call writeMatrix(additionalLayerValues,additionalLayerValuesRows,additionalLayerValuesColumns)

        write(*,*) "2nd hidden -> output weights"
        call writeMatrix(hiddenToOutputWeights,hiddenToOutputWeightsRows,hiddenToOutputWeightsColumns)

        write(*,*) "2nd hidden -> output weights derivative"
        call writeMatrix(hiddenToOutputDerivative,hiddenToOutputWeightsRows,hiddenToOutputWeightsColumns)

        write(*,*) hiddenToOutputWeights

        write(*,*) "Net output values at the last step of iteration:"
        call writeMatrix(outputValues,outputDataRows,outputDataColumns)
    end if


end do

write(*,*) "Network trained."
!displaying values of weights
write(*,*) "Weights input->hidden"
call writeMatrix(inputToHiddenWeights,1,hiddenLayerCells)

write(*,*) "Weights hidden->output"
call writeMatrix(hiddenToOutputWeights,hiddenLayerCells,1)


call plotCost

end subroutine


subroutine netCheck
    use variables
    implicit none

!asking user for value with which the network will be checked
write(*,*) "Checking the net with value "
read(*,*) testValue(1,1)

!normalizing the input given by user with the parameters (min,max,average) of training data
testValue(1,1) = (testValue(1,1)-inputValuesParameters(1))/(inputValuesParameters(3)-inputValuesParameters(2))

write(*,*) "Normalized test value:", testValue(1,1)

!feeding the trained net with the checking input data
hiddenValuesChecking = matmul(testValue,inputToHiddenWeights)

write(*,*) "hidden values checking"
write(*,*) hiddenValuesChecking

call matrixSigmoid(hiddenValuesChecking,hiddenValuesCheckingSigmoid,1,hiddenValuesColumns)

write(*,*) "hidden values checking sigmoid"
write(*,*) hiddenValuesCheckingSigmoid

additionalValuesChecking = matmul(hiddenValuesCheckingSigmoid,additionalLayerWeights)

write(*,*) "additional values checking"
write(*,*) additionalValuesChecking

call matrixSigmoid(additionalValuesChecking,additionalValuesCheckingSigmoid,1,additionalLayerValuesColumns)

write(*,*) "additional values checking sigmoid"
write(*,*) additionalValuesCheckingSigmoid

outputValuesChecking = matmul(additionalValuesCheckingSigmoid,hiddenToOutputWeights)

write(*,*) "Output value: ", outputValuesChecking(1,1)

end subroutine


subroutine compareResults
    use variables
    implicit none

    real top
    real bottom
    integer steps
    real x, x_norm
    real objective


    bottom=inputValues(1,1)
    top = inputValues(hiddenValuesRows,1)

    steps=int((top-bottom)/checkingStep)


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

    call system('binary\gnuplot\wgnuplot plotComparison.plt')
end subroutine

real function objective(x)
    implicit none
    real x
    objective = sin(x)
end function
