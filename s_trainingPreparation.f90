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

subroutine trainingFirstPhase
    use variables
    implicit none

do i=1,iterationSteps


    ! values of hidden layer = input values * weights
    hiddenValues = matmul(inputValuesNormalized,inputToHiddenWeights)

    ! new ---------------
    call normalizeMatirx(hiddenValues,hiddenValuesRows,hiddenValuesColumns)
    ! end new ----------------------

    !sigmoiding hidden values
    call matrixSigmoid(hiddenValues,hiddenValuesSigmoid,hiddenValuesRows,hiddenValuesColumns)

    !values of output layer = hidden values * weights
    outputValues = matmul(hiddenValuesSigmoid,hiddenToOutputWeights)

    if(i==iterationSteps) then
        write(*,*) "ostatni przed normalizacja"
        call writeMatrix(outputValues,outputDataRows,outputDataColumns)
    end if

    outputValuesNormalized = outputValues
    !new -------------
    call normalizeMatirx(outputValuesNormalized,outputDataRows,outputDataColumns)
    !end new -------------


    !sigmoiding output values
    call matrixSigmoid(outputValuesNormalized,outputValuesSigmoid,outputDataRows,outputDataColumns)

    call matrixSigmoidDerivative(outputValuesNormalized,outputValuesSigmoidDerivative,outputDataRows,outputDataColumns)

    ! OLD
    !delta3 = (-1)*(outputValuesExpectedNormalizedSigmoided-outputValuesSigmoid)*outputValuesSigmoidDerivative

    !NEW
    delta3 = (-1)*(outputValuesExpected-outputValues)*outputValuesSigmoidDerivative
    !end new

    hiddenToOutputDerivative = matmul(transpose(hiddenValuesSigmoid),delta3)

    call matrixSigmoidDerivative(hiddenValues,hiddenValuesSigmoidDerivatives,hiddenValuesRows,hiddenValuesColumns)

    delta2 = matmul(delta3,transpose(hiddenToOutputWeights))*hiddenValuesSigmoidDerivatives

    inputToHiddenDerivative = matmul(transpose(inputValues),delta2)

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
write(*,*)

write(*,*) "Checking the net with value "
read(*,*) testValue(1,1)

!normalizing the test value
testValue(1,1) = (testValue(1,1)-inputValuesParameters(1))/(inputValuesParameters(3)-inputValuesParameters(2))

write(*,*) "Normalized testValue:", testValue(1,1)

hiddenValuesChecking = matmul(testValue,inputToHiddenWeights)

! new ---------------
call normalizeMatirx(hiddenValuesChecking,1,hiddenValuesColumns)

call matrixSigmoid(hiddenValuesChecking,hiddenValuesCheckingSigmoid,1,hiddenValuesColumns)

outputValuesChecking = matmul(hiddenValuesCheckingSigmoid,hiddenToOutputWeights)

write(*,*) outputValuesChecking



!write(*,*) "Weights input->hidden"
!write(*,*) inputToHiddenWeights

!write(*,*) "Weights hidden->output"
!write(*,*) hiddenToOutputWeights

!OLD
!call denormalizeValues(outputValues,outputValuesDenormalized,outputDataRows,outputValuesParameters)

!new
!call denormalizeValues(outputValues,outputValuesDenormalized,outputDataRows,inputValuesParameters)

!write(*,*) "Values after training the net"
!call writeMatrix(outputValuesDenormalized,outputDataRows,outputDataColumns)

!call plotCost
end subroutine
