subroutine trainingPreparation
    use variables
    implicit none

    call readInputFile

    hiddenLayerCells = inputDataRows

    !allocating sizes of input and output matrices
    allocate(outputValues(outputDataRows,outputDataColumns))
    allocate(outputValuesSigmoid(outputDataRows,outputDataColumns))
    allocate(outputValuesSigmoidDerivative(outputDataRows,outputDataColumns))
    allocate(outputValuesDifferences(outputDataRows,outputDataColumns))
    allocate(delta3(outputDataRows,outputDataColumns))



    !Defining information about weights matrices
    !INPUT -> HIDDEN
    inputToHiddenWeightsRows = inputDataColumns     !size of the matrix containing
    inputToHiddenWeightsColumns = hiddenLayerCells   ! number of cells in hidden layer

    allocate(inputToHiddenWeights(inputToHiddenWeightsRows,inputToHiddenWeightsColumns))
    allocate(inputToHiddenWeightsDerivatives(inputToHiddenWeightsRows,inputToHiddenWeightsColumns))


    !HIDDEN-> OUTPUT
    hiddenToOutputWeightsRows = outputDataRows
    hiddenToOutputWeightsColumns = outputDataColumns

    allocate(hiddenToOutputWeights(hiddenToOutputWeightsRows,hiddenToOutputWeightsColumns))
    allocate(hiddenToOutputWeightsDerivatives(hiddenToOutputWeightsRows,hiddenToOutputWeightsColumns))

    !Defining information about values matrices
    hiddenValuesRows = inputDataRows    !size of the matrix of hidden layer values
    hiddenValuesColumns = hiddenLayerCells

    allocate(hiddenValues(hiddenValuesRows,hiddenValuesColumns))
    allocate(hiddenValuesSigmoid(hiddenValuesRows,hiddenValuesColumns))
    allocate(hiddenValuesSigmoidDerivatives(hiddenValuesRows,hiddenValuesColumns))


    !Assigning random weights
    ! INPUT->HIDDEN
    call assignRandomWeights(inputToHiddenWeights,&
                            inputToHiddenWeightsRows,inputToHiddenWeightsColumns,&
                            lowerRandomWeightValue,upperRandomWeightValue)

    ! HIDDEN->OUTPUT
    call assignRandomWeights(hiddenToOutputWeights,&
                            hiddenToOutputWeightsRows,hiddenToOutputWeightsColumns,&
                            lowerRandomWeightValue,upperRandomWeightValue)


    !allocate(hiddenToOutputDerivative(hiddenValuesRows,hiddenValuesColumns))

    allocate(hiddenToOutputDerivative(outputDataRows,outputDataColumns))
    allocate(delta2(outputDataRows,outputDataRows))
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


end subroutine

subroutine trainingFirstPhase
    use variables
    implicit none

do m=1,3

    write(*,*) "step", m

    write(*,*)
    write(*,*) "Weights from input to hidden layer"
    call writeMatrix(inputToHiddenWeights,inputToHiddenWeightsRows,inputToHiddenWeightsColumns)

    write(*,*)
    write(*,*) "Weights from hidden to output layer"
    call writeMatrix(hiddenToOutputWeights,hiddenToOutputWeightsRows,hiddenToOutputWeightsColumns)


    ! values of hidden layer = input values * weights
    hiddenValues = matmul(inputValues,inputToHiddenWeights)

    !write(*,*) "hidden values"
    !call writeMatrix(hiddenValues,hiddenValuesRows,hiddenValuesColumns)

    !sigmoiding hidden values
    call matrixSigmoid(hiddenValues,hiddenValuesSigmoid,hiddenValuesRows,hiddenValuesColumns)

    !write(*,*) "hidden values sigmoided"
    !call writeMatrix(hiddenValuesSigmoid,hiddenValuesRows,hiddenValuesColumns)

    !values of output layer = hidden values * weights
    outputValues = matmul(hiddenValuesSigmoid,hiddenToOutputWeights)

    !write(*,*) "output values"
    !call writeMatrix(outputValues,outputDataRows,outputDataColumns)



    !sigmoiding output values
    call matrixSigmoid(outputValues,outputValuesSigmoid,outputDataRows,outputDataColumns)

    write(*,*) "output values (sigmoided)"
    call writeMatrix(outputValuesSigmoid,outputDataRows,outputDataColumns)

    !write(*,*) "output values sigmoid"
    !call writeMatrix(outputValuesSigmoid,outputDataRows,outputDataColumns)

    call matrixSigmoidDerivative(outputValues,outputValuesSigmoidDerivative,outputDataRows,outputDataColumns)

    !write(*,*) "output sigmoids derivative"
    !call writeMatrix(outputValuesSigmoidDerivative,outputDataRows,outputDataColumns)

    delta3 = (-1)*(outputValuesExpected-outputValuesSigmoid)*outputValuesSigmoidDerivative
    !delta3 = (-1)*(outputValuesExpected-outputValues)*outputValuesSigmoidDerivative

    !write(*,*) "differences"
    !call writeMatrix(outputValuesExpected-outputValuesSigmoid,outputDataRows,outputDataColumns)


    !write(*,*) "delta3"
    !call writeMatrix(delta3,outputDataRows,outputDataColumns)

    hiddenToOutputDerivative = matmul(transpose(hiddenValuesSigmoid),delta3)

    call matrixSigmoidDerivative(hiddenValues,hiddenValuesSigmoidDerivatives,hiddenValuesRows,hiddenValuesColumns)

    delta2 = matmul(delta3,transpose(hiddenToOutputWeights))*hiddenValuesSigmoidDerivatives

    inputToHiddenDerivative = matmul(transpose(inputValues),delta2)

    step = 1 !learni
    inputToHiddenWeights = inputToHiddenWeights - step*inputToHiddenDerivative
    hiddenToOutputWeights = hiddenToOutputWeights - step*hiddenToOutputDerivative




    write(*,*) "derivative input->hidden"
    call writeMatrix(inputToHiddenDerivative,inputDataColumns,inputDataRows)

    write(*,*) "derivative hidden->output"
    !call writeMatrix(hiddenToOutputDerivative,hiddenValuesRows,hiddenValuesColumns)
    call writeMatrix(hiddenToOutputDerivative,outputDataRows,outputDataColumns)

    write(*,*)
    write(*,*) "Cost equals"
    call costFunction(outputValuesSigmoid,outputValuesExpected,outputDataRows,outputDataColumns,cost)
    write(*,*) cost






end do
end subroutine


    !now outputValuesSigmoid matrix is the matrix with our results

    !write(*,*)
    !write(*,*) "values of output layer"
    !call writeMatrix(outputValuesSigmoid,outputDataRows,outputDataColumns)

    !outputValuesSigmoidDerivative matrix is <delta>3 from the video


   ! write(*,*) "Before"
    !call writeMatrix(inputToHiddenWeights,inputToHiddenWeightsRows,inputToHiddenWeightsColumns)
    !call writeMatrix(hiddenToOutputWeights,hiddenToOutputWeightsRows,hiddenToOutputWeightsColumns)
    !write(*,*) "Derivatives"
    !call writeMatrix(inputToHiddenDerivative,inputDataColumns,inputDataRows)
    !call writeMatrix(hiddenToOutputDerivative,hiddenValuesRows,hiddenValuesColumns)

    !write(*,*) "After"
    !call writeMatrix(inputToHiddenWeights,inputToHiddenWeightsRows,inputToHiddenWeightsColumns)
    !call writeMatrix(hiddenToOutputWeights,hiddenToOutputWeightsRows,hiddenToOutputWeightsColumns)


