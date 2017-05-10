subroutine trainingPreparation
    use variables
    implicit none

    call readInputFile


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
    allocate(inputToHiddenDerivative(inputToHiddenWeightsRows,inputToHiddenWeightsColumns))
    allocate(inputToHiddenCorrections(inputToHiddenWeightsRows,inputToHiddenWeightsColumns))
    allocate(inputToHiddenCorrectionsOld(inputToHiddenWeightsRows,inputToHiddenWeightsColumns))


    !HIDDEN-> OUTPUT
    hiddenToOutputWeightsRows = hiddenLayerCells
    hiddenToOutputWeightsColumns = outputDataColumns

    allocate(hiddenToOutputWeights(hiddenToOutputWeightsRows,hiddenToOutputWeightsColumns))
    allocate(hiddenToOutputDerivative(hiddenToOutputWeightsRows,hiddenToOutputWeightsColumns))
    allocate(hiddenToOutputCorrections(hiddenToOutputWeightsRows,hiddenToOutputWeightsColumns))
    allocate(hiddenToOutputCorrectionsOld(hiddenToOutputWeightsRows,hiddenToOutputWeightsColumns))

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



    allocate(delta2(outputDataRows,outputDataRows))

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

    inputToHiddenCorrectionsOld = 0.0
    hiddenToOutputCorrectionsOld = 0.0

do m=1,iterationSteps

    write(*,*) "-----------------------------------------"
    write(*,*) "Step", m

    write(*,*)
    write(*,*) "Weights from input to hidden layer"
    do i=1,hiddenLayerCells
        inputToHiddenWeights(inputToHiddenWeightsRows,i) = tresholdValues(i)
    end do

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




    write(*,*) "derivative input->hidden"
    call writeMatrix(inputToHiddenDerivative,inputToHiddenWeightsRows,inputToHiddenWeightsColumns)

    write(*,*) "derivative hidden->output"
    !call writeMatrix(hiddenToOutputDerivative,hiddenValuesRows,hiddenValuesColumns)
    call writeMatrix(hiddenToOutputDerivative,hiddenToOutputWeightsRows,hiddenToOutputWeightsColumns)

    call random_number(momentum)
    call random_number(step)

    write (*,*) "Learning rate:", step

    if(m.gt.1) then
    write(*,*) "Momentum:", momentum

        write(*,*)
        write(*,*) "Corrections from previous step"
        write(*,*) "Input -> hidden"
        call writeMatrix(inputToHiddenCorrectionsOld,inputToHiddenWeightsRows,inputToHiddenWeightsColumns)
        write(*,*) "Hidden -> output"
        call writeMatrix(hiddenToOutputCorrectionsOld,hiddenToOutputWeightsRows,hiddenToOutputWeightsColumns)

    end if

    inputToHiddenCorrections = -step*inputToHiddenDerivative + momentum*inputToHiddenCorrectionsOld
    hiddenToOutputCorrections = -step*hiddenToOutputDerivative + momentum*hiddenToOutputCorrectionsOld

    write(*,*)
    write(*,*) "NEW Input->Hidden corrections"
    call writeMatrix(inputToHiddenCorrections,inputToHiddenWeightsRows,inputToHiddenWeightsColumns)

    write(*,*)
    write(*,*) "NEW Hidden->Output corrections"
    call writeMatrix(hiddenToOutputCorrections,hiddenToOutputWeightsRows,hiddenToOutputWeightsColumns)

    inputToHiddenWeights = inputToHiddenWeights + inputToHiddenCorrections
    hiddenToOutputWeights = hiddenToOutputWeights + hiddenToOutputCorrections


    write(*,*)
    write(*,*) "Cost equals"
    call costFunction(outputValuesSigmoid,outputValuesExpected,outputDataRows,outputDataColumns,cost)
    write(*,*) cost

    inputToHiddenCorrectionsOld = inputToHiddenCorrections
    hiddenToOutputCorrectionsOld = hiddenToOutputCorrections
end do
end subroutine


