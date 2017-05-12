module variables
    integer hiddenLayerCells
    integer inputDataRows, inputDataColumns
    integer outputDataRows, outputDataColumns

    integer hiddenValuesRows, hiddenValuesColumns
    integer inputToHiddenWeightsRows, inputToHiddenWeightsColumns
    integer hiddenToOutputWeightsRows, hiddenToOutputWeightsColumns


    real lowerRandomWeightValue,upperRandomWeightValue

    real, allocatable:: inputValuesNormalized(:,:), outputValuesNormalized(:,:)
    real, allocatable:: outputValuesExpectedNormalized(:,:), outputValuesDenormalized(:,:)
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

    real outputValuesParameters(3), inputValuesParameters(3)
    real costValues(:)

    real randomFromRange
    real reallyRandom
    real sigmoid, sigmoidDerivative
    integer i,j,k,l,m
    real cost
    real step, momentum
    integer iterationSteps

end module
