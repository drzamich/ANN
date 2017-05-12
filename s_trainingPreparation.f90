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

    write(*,*) "Input values normalized"
    call writeMatrix(inputValuesNormalized,inputDataRows,inputDataColumns)


    write(*,*)
    write(*,*) "(Expected) output values"
    call writeMatrix(outputValuesExpected,outputDataRows,outputDataColumns)


    write(*,*)
    write(*,*) "(Expected) output values normalized"
    call writeMatrix(outputValuesExpectedNormalized,outputDataRows,outputDataColumns)

end subroutine

subroutine trainingFirstPhase
    use variables
    implicit none

do m=1,iterationSteps

    write(*,*) "-----------------------------------------"
    write(*,*) "Step", m

    write(*,*)
    write(*,*) "Weights from input to hidden layer"
    call writeMatrix(inputToHiddenWeights,inputToHiddenWeightsRows,inputToHiddenWeightsColumns)



    ! values of hidden layer = input values * weights
    hiddenValues = matmul(inputValues,inputToHiddenWeights)

    write(*,*)
    write(*,*) "hidden values"
    call writeMatrix(hiddenValues,hiddenValuesRows,hiddenValuesColumns)

    !sigmoiding hidden values
    call matrixSigmoid(hiddenValues,hiddenValuesSigmoid,hiddenValuesRows,hiddenValuesColumns)

    write(*,*)
    write(*,*) "hidden values sigmoided"
    call writeMatrix(hiddenValuesSigmoid,hiddenValuesRows,hiddenValuesColumns)

    write(*,*)
    write(*,*) "Weights from hidden to output layer"
    call writeMatrix(hiddenToOutputWeights,hiddenToOutputWeightsRows,hiddenToOutputWeightsColumns)

    !values of output layer = hidden values * weights
    outputValues = matmul(hiddenValuesSigmoid,hiddenToOutputWeights)

    write(*,*) "output values"
    call writeMatrix(outputValues,outputDataRows,outputDataColumns)

    !sigmoiding output values
    call matrixSigmoid(outputValues,outputValuesSigmoid,outputDataRows,outputDataColumns)

    write(*,*) "output values (sigmoided)"
    call writeMatrix(outputValuesSigmoid,outputDataRows,outputDataColumns)


    call matrixSigmoidDerivative(outputValues,outputValuesSigmoidDerivative,outputDataRows,outputDataColumns)


    delta3 = (-1)*(outputValuesExpected-outputValuesSigmoid)*outputValuesSigmoidDerivative

    hiddenToOutputDerivative = matmul(transpose(hiddenValuesSigmoid),delta3)

    call matrixSigmoidDerivative(hiddenValues,hiddenValuesSigmoidDerivatives,hiddenValuesRows,hiddenValuesColumns)

    delta2 = matmul(delta3,transpose(hiddenToOutputWeights))*hiddenValuesSigmoidDerivatives

    inputToHiddenDerivative = matmul(transpose(inputValues),delta2)


    write(*,*) "derivative input->hidden"
    call writeMatrix(inputToHiddenDerivative,inputToHiddenWeightsRows,inputToHiddenWeightsColumns)

    write(*,*) "derivative hidden->output"
    call writeMatrix(hiddenToOutputDerivative,hiddenToOutputWeightsRows,hiddenToOutputWeightsColumns)

    call random_number(momentum)
    call random_number(step)

    write (*,*) "Learning rate:", step

    inputToHiddenWeights = inputToHiddenWeights -step*inputToHiddenDerivative
    hiddenToOutputWeights = hiddenToOutputWeights -step*hiddenToOutputDerivative


    write(*,*)
    write(*,*) "Cost equals"
    call costFunction(outputValuesSigmoid,outputValuesExpected,outputDataRows,outputDataColumns,cost)
    write(*,*) cost

end do
end subroutine


