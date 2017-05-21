module variables
    integer hiddenLayerCells
    integer inputDataRows, inputDataColumns
    integer outputDataRows, outputDataColumns

    integer hiddenValuesRows, hiddenValuesColumns
    integer inputToHiddenWeightsRows, inputToHiddenWeightsColumns
    integer hiddenToOutputWeightsRows, hiddenToOutputWeightsColumns


    real lowerRandomWeightValue,upperRandomWeightValue

    real, allocatable:: inputValuesNormalized(:,:), outputValuesNormalized(:,:)

    real,allocatable:: inputToHiddenWeights(:,:), inputToHiddenWeightsDerivatives(:,:)
    real,allocatable:: hiddenToOutputWeights(:,:), hiddenToOutputWeightsDerivatives(:,:)

    real,allocatable:: inputValues(:,:)
    real,allocatable:: hiddenValues(:,:), hiddenValuesSigmoid(:,:), hiddenValuesSigmoidDerivatives(:,:)

    real, allocatable:: outputValues(:,:), outputValuesExpected(:,:)
    real, allocatable:: outputValuesSigmoid(:,:), outputValuesSigmoidDerivative(:,:)

    real, allocatable:: delta3(:,:)
    real, allocatable:: hiddenToOutputDerivative(:,:)
    real, allocatable:: inputToHiddenDerivative(:,:)
    real, allocatable:: delta2(:,:)

    real inputValuesParameters(3)
    real,allocatable:: costValues(:)

    real randomFromRange
    real reallyRandom
    real sigmoid, sigmoidDerivative
    integer i,j,k,l
    real cost
    real step
    integer iterationSteps
    integer stepIndicatorProgram, stepIndicatorChart


    real testValue(1,1)
    real, allocatable :: hiddenValuesChecking(:,:), hiddenValuesCheckingSigmoid(:,:)
    real outputValuesChecking(1,1)



    integer additionalLayerCells
    integer additionalLayerWeightsRows
    integer additionalLayerWeightsColumns

    integer additionalLayerValuesRows
    integer additionalLayerValuesColumns

    real,allocatable:: additionalLayerWeights(:,:)
    real,allocatable:: additionalLayerWeightsDerivative(:,:)

    real,allocatable:: additionalLayerValues(:,:)
    real,allocatable:: additionalLayerValuesSigmoid(:,:)
    real,allocatable:: additionalLayerValuesSigmoidDerivative(:,:)

    real,allocatable:: deltaAdditional(:,:)

    real, allocatable:: additionalValuesChecking(:,:), additionalValuesCheckingSigmoid(:,:)

    real checkingStep
    real factor, factorInv
end module
