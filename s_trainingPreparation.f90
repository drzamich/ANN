subroutine trainingPreparation
    use variables
    implicit none

    call readInputFile

    call allocateMatrices


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

    call normalizeValues(inputValues,inputValuesNormalized,inputDataRows,inputValuesParameters)
    call normalizeValues(outputValuesExpected,outputValuesExpectedNormalized,outputDataRows,outputValuesParameters)

    write(*,*) "Input values"
    call writeMatrix(inputValues,inputDataRows,inputDataColumns)

    write(*,*)
    write(*,*) "(Expected) output values"
    call writeMatrix(outputValuesExpected,outputDataRows,outputDataColumns)

    call matrixSigmoid(outputValuesExpectedNormalized,outputValuesExpectedNormalizedSigmoided,outputDataRows,outputDataColumns)

end subroutine

subroutine training
    use variables
    implicit none

do i=1,iterationSteps
    hiddenValues = matmul(inputValuesNormalized,inputToHiddenWeights)

    !sigmoiding hidden values
    call matrixSigmoid(hiddenValues,hiddenValuesSigmoid,hiddenValuesRows,hiddenValuesColumns)

    !values of output layer = hidden values * weights
    outputValues = matmul(hiddenValuesSigmoid,hiddenToOutputWeights)

    !displaying the value of net output layer in the last iteration step
    if(i==iterationSteps) then
       call writeMatrix(outputValues,outputDataRows,outputDataColumns)
        exit
    end if


    !sigmoiding output values
    call matrixSigmoid(outputValues,outputValuesSigmoid,outputDataRows,outputDataColumns)

    call matrixSigmoidDerivative(outputValues,outputValuesSigmoidDerivative,outputDataRows,outputDataColumns)

    delta3 = (-1)*(outputValuesExpectedNormalized-outputValues)*outputValuesSigmoidDerivative

    hiddenToOutputDerivative = matmul(transpose(hiddenValuesSigmoid),delta3)

    call matrixSigmoidDerivative(hiddenValues,hiddenValuesSigmoidDerivatives,hiddenValuesRows,hiddenValuesColumns)

    delta2 = matmul(delta3,transpose(hiddenToOutputWeights))*hiddenValuesSigmoidDerivatives

    inputToHiddenDerivative = matmul(transpose(inputValuesNormalized),delta2)

    step=0.3

    inputToHiddenWeights = inputToHiddenWeights -step*inputToHiddenDerivative
    hiddenToOutputWeights = hiddenToOutputWeights -step*hiddenToOutputDerivative

    call costFunction(outputValuesSigmoid,outputValuesExpectedNormalizedSigmoided,outputDataRows,outputDataColumns,cost)

    costValues(i) = cost

    if(mod(i,stepIndicatorProgram)==0) then
        write(*,*) "Step", i
    end if


end do

write(*,*) "Network trained."
write(*,*) "Weights input->hidden"
call writeMatrix(inputToHiddenWeights,1,hiddenLayerCells)

write(*,*) "Weights hidden->output"
call writeMatrix(hiddenToOutputWeights,hiddenLayerCells,1)

end subroutine


subroutine netCheck
    use variables
    implicit none

write(*,*) "Checking the net with value "
read(*,*) testValue(1,1)

testValue(1,1) = (testValue(1,1)-inputValuesParameters(1))/(inputValuesParameters(3)-inputValuesParameters(2))
write(*,*) "Normalized testValue:", testValue(1,1)

hiddenValuesChecking = matmul(testValue,inputToHiddenWeights)

call matrixSigmoid(hiddenValuesChecking,hiddenValuesCheckingSigmoid,1,hiddenValuesColumns)

outputValuesChecking = matmul(hiddenValuesCheckingSigmoid,hiddenToOutputWeights)

call denormalizeValues(outputValuesChecking,outputValuesCheckingDenormalized,1,outputValuesParameters)

write(*,*) outputValuesCheckingDenormalized(1,1)


!call plotCost
end subroutine
