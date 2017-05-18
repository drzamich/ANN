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

    !calculating net values of net output layer
    outputValues = matmul(hiddenValuesSigmoid,hiddenToOutputWeights)

    call normalizeValues(outputValues,outputValuesNormalized,inputDataRows,arbitraryMatrix)

    !sigmoiding output values
    call matrixSigmoid(outputValuesNormalized,outputValuesSigmoid,outputDataRows,outputDataColumns)

    !calculating the derivatives hidden->output layer
    !calculating values of sigmoid derivative function of net output layer
    call matrixSigmoidDerivative(outputValuesNormalized,outputValuesSigmoidDerivative,outputDataRows,outputDataColumns)


    delta3 = (-1)*(outputValuesExpected-outputValues)*outputValuesSigmoidDerivative

    hiddenToOutputDerivative = matmul(transpose(hiddenValuesSigmoid),delta3)

    !calculating the derivatives input->hidden layer
    !calculating values of sigmoid derivative function of net output layer
    call matrixSigmoidDerivative(hiddenValues,hiddenValuesSigmoidDerivatives,hiddenValuesRows,hiddenValuesColumns)

    delta2 = matmul(delta3,transpose(hiddenToOutputWeights))*hiddenValuesSigmoidDerivatives

    inputToHiddenDerivative = matmul(transpose(inputValuesNormalized),delta2)

    !learning rate
    step=0.3

    !correcting the weights' values
    inputToHiddenWeights = inputToHiddenWeights -step*inputToHiddenDerivative
    hiddenToOutputWeights = hiddenToOutputWeights -step*hiddenToOutputDerivative

    !calculating the cost of the function
    call costFunction(outputValues,outputValuesExpected,outputDataRows,outputDataColumns,cost)

    costValues(i) = cost

    !notyfying user in which iteration step the program currently is
    if(mod(i,stepIndicatorProgram)==0) then
        write(*,*) "Step", i
    end if

    !displaying the value of net output layer in the last iteration step
    if(i==iterationSteps) then
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

!feeding the trained net with the checking input data
hiddenValuesChecking = matmul(testValue,inputToHiddenWeights)

call matrixSigmoid(hiddenValuesChecking,hiddenValuesCheckingSigmoid,1,hiddenValuesColumns)

outputValuesChecking = matmul(hiddenValuesCheckingSigmoid,hiddenToOutputWeights)

write(*,*) "Output value: ", outputValuesChecking(1,1)

end subroutine
