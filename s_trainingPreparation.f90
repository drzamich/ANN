subroutine trainingPreparation
    use variables
    implicit none

    !Defining information about our training data
    inputDataRows = 5
    outputDataRows = inputDataRows

    inputDataColumns = 1       !number of cells in input layer
    outputDataColumns = 1      !number of cells in output layer

    !allocating sizes of input and output matrices
    allocate(inputValues(inputDataRows,inputDataColumns))
    allocate(outputValues(outputDataRows,outputDataColumns))
    allocate(outputValuesSigmoid(outputDataRows,outputDataColumns))
    allocate(outputValuesSigmoidDerivative(outputDataRows,outputDataColumns))
    allocate(outputValuesExpected(outputDataRows,outputDataColumns))
    allocate(outputValuesDifferences(outputDataRows,outputDataColumns))
    allocate(backPropagatingError(outputDataRows,outputDataColumns))

    !putting in training data
    inputValues = reshape((/-2.0,-0.2,0.5,1.0,3.0/),shape(inputValues))
    outputValuesExpected = reshape((/4.0,0.04,0.25,1.0,9.0/),shape(outputValuesExpected))

    !Defining information about weights matrices
    !INPUT -> HIDDEN
    inputToHiddenWeightsRows = inputDataColumns     !size of the matrix containing
    inputToHiddenWeightsColumns = inputDataRows

    allocate(inputToHiddenWeights(inputToHiddenWeightsRows,inputToHiddenWeightsColumns))
    allocate(inputToHiddenWeightsDerivatives(inputToHiddenWeightsRows,inputToHiddenWeightsColumns))


    !HIDDEN-> OUTPUT
    hiddenToOutputWeightsRows = outputDataRows
    hiddenToOutputWeightsColumns = outputDataColumns

    allocate(hiddenToOutputWeights(hiddenToOutputWeightsRows,hiddenToOutputWeightsColumns))
    allocate(hiddenToOutputWeightsDerivatives(hiddenToOutputWeightsRows,hiddenToOutputWeightsColumns))

    !Defining information about values matrices
    hiddenValuesSize = inputDataRows    !size of the matrix of hidden layer values

    allocate(hiddenValues(hiddenValuesSize,hiddenValuesSize))
    allocate(hiddenValuesSigmoid(hiddenValuesSize,hiddenValuesSize))
    allocate(hiddenValuesSigmoidDerivatives(hiddenValuesSize,hiddenValuesSize))

    !Setting boundaries for random weights
    lowerRandomWeightValue = -0.1
    upperRandomWeightValue = 0.1


    !Assigning random weights
    ! INPUT->HIDDEN
    call assignRandomWeights(inputToHiddenWeights,&
                            inputToHiddenWeightsRows,inputToHiddenWeightsColumns,&
                            lowerRandomWeightValue,upperRandomWeightValue)

    ! HIDDEN->OUTPUT
    call assignRandomWeights(hiddenToOutputWeights,&
                            hiddenToOutputWeightsRows,hiddenToOutputWeightsColumns,&
                            lowerRandomWeightValue,upperRandomWeightValue)

    allocate(hiddenToOutputDerivative(hiddenValuesSize,hiddenValuesSize))
    allocate(delta2(outputDataRows,outputDataRows))
    allocate(delta2_help(outputDataRows,outputDataRows))
    allocate(inputToHiddenDerivative(inputDataColumns,inputDataRows))
end subroutine

subroutine displayTrainingData
    use variables
    implicit none

    write(*,*) "Input values"
    call writeMatrix(inputValues,inputDataRows,inputDataColumns)

    write(*,*)
    write(*,*) "(Expected) output values"
    call writeMatrix(outputValuesExpected,outputDataRows,outputDataColumns)


    write(*,*)
    write(*,*) "Weights from input to hidden layer"
    call writeMatrix(inputToHiddenWeights,inputToHiddenWeightsRows,inputToHiddenWeightsColumns)

    write(*,*)
    write(*,*) "Weights from hidden to output layer"
    call writeMatrix(hiddenToOutputWeights,hiddenToOutputWeightsRows,hiddenToOutputWeightsColumns)
end subroutine

subroutine trainingFirstPhase
    use variables
    implicit none

do m=1,50
    ! values of hidden layer = input values * weights
    hiddenValues = matmul(inputValues,inputToHiddenWeights)

    !sigmoiding hidden values
    call matrixSigmoid(hiddenValues,hiddenValuesSigmoid,hiddenValuesSize,hiddenValuesSize)

    !values of output layer = hidden values * weights
    outputValues = matmul(hiddenValuesSigmoid,hiddenToOutputWeights)

    !sigmoiding output values
    call matrixSigmoid(outputValues,outputValuesSigmoid,outputDataRows,outputDataColumns)

    !now outputValuesSigmoid matrix is the matrix with our results

    write(*,*)
    write(*,*) "values of output layer"
    call writeMatrix(outputValuesSigmoid,outputDataRows,outputDataColumns)

    write(*,*)
    write(*,*) "Cost equals"
    call costFunction(outputValuesSigmoid,outputValuesExpected,outputDataRows,outputDataColumns,cost)
    write(*,*) cost

    call differencesMatrix(outputValuesExpected,outputValues,outputValuesDifferences,outputDataRows,outputDataColumns)
    outputValuesDifferences = outputValuesDifferences*(-1)
    call matrixSigmoidDerivative(outputValues,outputValuesSigmoidDerivative,outputDataRows,outputDataColumns)

    call matrixOfScalarMulti(outputValuesDifferences,outputValuesSigmoidDerivative,backPropagatingError)
    !outputValuesSigmoidDerivative matrix is <delta>3 from the video

    hiddenToOutputDerivative=matmul(transpose(hiddenValuesSigmoid),outputValuesSigmoidDerivative)

    call matrixSigmoidDerivative(hiddenValues,hiddenValuesSigmoidDerivatives,hiddenValuesSize,hiddenValuesSize)

    delta2_help=matmul(backPropagatingError,transpose(hiddenToOutputWeights))

    call matrixOfScalarMulti(delta2_help,hiddenValuesSigmoidDerivatives,delta2,hiddenValuesSize,hiddenValuesSize)
    inputToHiddenDerivative = matmul(transpose(inputValues),delta2)

    write(*,*) "Before"
    call writeMatrix(inputToHiddenWeights,inputToHiddenWeightsRows,inputToHiddenWeightsColumns)
    call writeMatrix(hiddenToOutputWeights,hiddenToOutputWeightsRows,hiddenToOutputWeightsColumns)
    write(*,*) "Derivatives"
    call writeMatrix(inputToHiddenDerivative,inputDataColumns,inputDataRows)
    call writeMatrix(hiddenToOutputDerivative,hiddenValuesSize,hiddenValuesSize)
    step = 1
    inputToHiddenWeights = inputToHiddenWeights - step*inputToHiddenDerivative
    hiddenToOutputWeights = hiddenToOutputWeights - step*hiddenToOutputDerivative
    write(*,*) "After"
    call writeMatrix(inputToHiddenWeights,inputToHiddenWeightsRows,inputToHiddenWeightsColumns)
    call writeMatrix(hiddenToOutputWeights,hiddenToOutputWeightsRows,hiddenToOutputWeightsColumns)

end do
end subroutine
