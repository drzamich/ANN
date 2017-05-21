subroutine allocateMatrices
    use variables
    implicit none

    inputToHiddenWeightsRows = inputDataColumns
    inputToHiddenWeightsColumns = hiddenLayerCells

    hiddenToOutputWeightsRows = additionalLayerCells
    hiddenToOutputWeightsColumns = outputDataColumns

    hiddenValuesRows = inputDataRows
    hiddenValuesColumns = hiddenLayerCells

    additionalLayerWeightsRows = hiddenLayerCells
    additionalLayerWeightsColumns = additionalLayerCells

    additionalLayerValuesRows = inputDataRows
    additionalLayerValuesColumns = additionalLayerCells

    allocate(outputValues(outputDataRows,outputDataColumns))
    allocate(outputValuesSigmoid(outputDataRows,outputDataColumns))
    allocate(outputValuesSigmoidDerivative(outputDataRows,outputDataColumns))
    allocate(outputValuesNormalized(outputDataRows,outputDataColumns))


    allocate(delta3(outputDataRows,outputDataColumns))

    allocate(inputToHiddenWeights(inputToHiddenWeightsRows,inputToHiddenWeightsColumns))
    allocate(inputToHiddenDerivative(inputToHiddenWeightsRows,inputToHiddenWeightsColumns))

    allocate(hiddenToOutputWeights(hiddenToOutputWeightsRows,hiddenToOutputWeightsColumns))
    allocate(hiddenToOutputDerivative(hiddenToOutputWeightsRows,hiddenToOutputWeightsColumns))

    allocate(hiddenValues(hiddenValuesRows,hiddenValuesColumns))
    allocate(hiddenValuesSigmoid(hiddenValuesRows,hiddenValuesColumns))
    allocate(hiddenValuesSigmoidDerivatives(hiddenValuesRows,hiddenValuesColumns))

    allocate(delta2(hiddenValuesRows,hiddenValuesColumns))

    allocate(inputValuesNormalized(inputDataRows,inputDataColumns))
    allocate(costValues(iterationSteps))


    allocate(hiddenValuesChecking(1,hiddenValuesColumns))
    allocate(hiddenValuesCheckingSigmoid(1,hiddenValuesColumns))

    allocate(additionalLayerWeights(additionalLayerWeightsRows,additionalLayerWeightsColumns))
    allocate(additionalLayerWeightsDerivative(additionalLayerWeightsRows,additionalLayerWeightsColumns))

    allocate(additionalLayerValues(additionalLayerValuesRows,additionalLayerValuesColumns))
    allocate(additionalLayerValuesSigmoid(additionalLayerValuesRows,additionalLayerValuesColumns))
    allocate(additionalLayerValuesSigmoidDerivative(additionalLayerValuesRows,additionalLayerValuesColumns))

    allocate(additionalValuesChecking(1,additionalLayerValuesColumns))
    allocate(additionalValuesCheckingSigmoid(1,additionalLayerValuesColumns))

    allocate(deltaAdditional(additionalLayerValuesRows,additionalLayerValuesColumns))
end subroutine
