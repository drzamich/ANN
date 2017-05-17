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

    !normalizing training output data to the range -0.5 - 0.5; saving the parameters (min,max,average) to parameter array
    call normalizeValues(outputValuesExpected,outputValuesExpectedNormalized,outputDataRows,outputValuesParameters)

    !displaying training input data on the screen
    write(*,*) "Input values"
    call writeMatrix(inputValues,inputDataRows,inputDataColumns)

    !displaying training output data on the screen
    write(*,*)
    write(*,*) "(Expected) output values"
    call writeMatrix(outputValuesExpected,outputDataRows,outputDataColumns)


    !sigmoiding matrix with normalized output data
    call matrixSigmoid(outputValuesExpectedNormalized,outputValuesExpectedNormalizedSigmoided,outputDataRows,outputDataColumns)

end subroutine

subroutine training
    use variables
    implicit none

do i=1,iterationSteps
    !calculating net values of hidden layer
    hiddenValues = matmul(inputValuesNormalized,inputToHiddenWeights)

    !sigmoiding net values of hidden layer
    call matrixSigmoid(hiddenValues,hiddenValuesSigmoid,hiddenValuesRows,hiddenValuesColumns)

    !calculating net values of net output layer
    outputValues = matmul(hiddenValuesSigmoid,hiddenToOutputWeights)

    !sigmoiding output values
    call matrixSigmoid(outputValues,outputValuesSigmoid,outputDataRows,outputDataColumns)

    !calculating the derivatives hidden->output layer
    !calculating values of sigmoid derivative function of net output layer
    call matrixSigmoidDerivative(outputValues,outputValuesSigmoidDerivative,outputDataRows,outputDataColumns)

    delta3 = (-1)*(outputValuesExpectedNormalized-outputValues)*outputValuesSigmoidDerivative

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
    call costFunction(outputValuesSigmoid,outputValuesExpectedNormalizedSigmoided,outputDataRows,outputDataColumns,cost)

    costValues(i) = cost

    !notyfying user in which iteration step the program currently is
    if(mod(i,stepIndicatorProgram)==0) then
        write(*,*) "Step", i
    end if

    !displaying the value of net output layer in the last iteration step
    if(i==iterationSteps) then
       call writeMatrix(outputValues,outputDataRows,outputDataColumns)
        exit
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

!denormalizing the output of the net with parameters of output training data
call denormalizeValues(outputValuesChecking,outputValuesCheckingDenormalized,1,outputValuesParameters)

write(*,*) outputValuesCheckingDenormalized(1,1)

end subroutine
