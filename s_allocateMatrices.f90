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

end subroutine
