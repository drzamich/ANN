subroutine allocateMatrices
    use variables
    implicit none

    inputToHiddenWeightsRows = inputDataColumns
    inputToHiddenWeightsColumns = hiddenLayerCells

    hiddenToOutputWeightsRows = hiddenLayerCells
    hiddenToOutputWeightsColumns = outputDataColumns

    hiddenValuesRows = inputDataRows
    hiddenValuesColumns = hiddenLayerCells

    allocate(outputValues(outputDataRows,outputDataColumns))
    allocate(outputValuesSigmoid(outputDataRows,outputDataColumns))
    allocate(outputValuesSigmoidDerivative(outputDataRows,outputDataColumns))

    allocate(outputValuesNormalized(outputDataRows,outputDataColumns))
    allocate(outputValuesExpectedNormalized(outputDataRows,outputDataColumns))
    allocate(outputValuesDenormalized(outputDataRows,outputDataColumns))


    allocate(delta3(outputDataRows,outputDataColumns))

    allocate(inputToHiddenWeights(inputToHiddenWeightsRows,inputToHiddenWeightsColumns))
    allocate(inputToHiddenDerivative(inputToHiddenWeightsRows,inputToHiddenWeightsColumns))

    allocate(hiddenToOutputWeights(hiddenToOutputWeightsRows,hiddenToOutputWeightsColumns))
    allocate(hiddenToOutputDerivative(hiddenToOutputWeightsRows,hiddenToOutputWeightsColumns))

    allocate(hiddenValues(hiddenValuesRows,hiddenValuesColumns))
    allocate(hiddenValuesSigmoid(hiddenValuesRows,hiddenValuesColumns))
    allocate(hiddenValuesSigmoidDerivatives(hiddenValuesRows,hiddenValuesColumns))

    allocate(delta2(outputDataRows,outputDataRows))

    allocate(inputValuesNormalized(inputDataRows,inputDataColumns))
    allocate(costValues(iterationSteps))
    allocate(outputValuesExpectedNormalizedSigmoided(outputDataRows,outputDataColumns))

    allocate(inputTraining(inputDataRows+1,inputDataColumns))
    allocate(outputTraining(outputDataRows+1,outputDataColumns))

    allocate(inputTrainingNormalized(inputDataRows+1,inputDataColumns))
    allocate(outputTrainingNormalized(outputDataRows+1,outputDataColumns))

    allocate(trainingSigmoid(inputDataRows+1,hiddenLayerCells))

    allocate(hiddenValuesChecking(1,hiddenValuesColumns))
    allocate(hiddenValuesCheckingSigmoid(1,hiddenValuesColumns))
end subroutine
