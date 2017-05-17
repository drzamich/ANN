subroutine netCheck_old
    use variables
    implicit none
    real valuee
    real output(1,1)
    real hidden123(inputDataRows+1,hiddenLayerCells)

    write(*,*) "Give the value you want to check the net with"
    read(*,*) valuee

    do i=1,inputDataRows
        inputTraining(i,1) = inputValues(i,1)
        outputTraining(i,1) = outputValues(i,1)
    end do

    inputTraining(inputDataRows+1,1) = valuee


    call normalizeValues(inputTraining,inputTrainingNormalized,inputDataRows+1,trainingParameters)

    hidden123 = matmul(inputTrainingNormalized,inputToHiddenWeights)

    call matrixSigmoid(hidden123,trainingSigmoid,inputDataRows+1,hiddenLayerCells)
    outputTraining=matmul(trainingSigmoid,hiddenToOutputWeights)

    call denormalizeValues(outputTraining,outputTrainingNormalized,inputDataRows+1,trainingParameters)

    write(*,*) outputTrainingNormalized(inputDataRows+1,1)

end subroutine
