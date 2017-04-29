module variables
    integer inputDataColumns, inputLayerCells
    integer inputDataRows, outputDataRows
    integer outputDataColumns, outputLayerCells
    integer hiddenValuesSize
    integer inputToHiddenWeightsRows, inputToHiddenWeightsColumns
    integer hiddenToOutputWeightsRows, hiddenToOutputWeightsColumns

    integer hiddenLayerCells

    real lowerRandomWeightValue,upperRandomWeightValue
    real,allocatable:: inputToHiddenWeights(:,:), inputToHiddenWeightsDerivatives(:,:)
    real,allocatable:: hiddenToOutputWeights(:,:), hiddenToOutputWeightsDerivatives(:,:)
    real,allocatable:: inputValues(:,:)
    real,allocatable:: hiddenValues(:,:), hiddenValuesSigmoid(:,:), hiddenValuesSigmoidDerivatives(:,:)
    real, allocatable:: outputValues(:,:), outputValuesExpected(:,:), outputValuesDifferences(:,:)
    real, allocatable:: outputValuesSigmoid(:,:), outputValuesSigmoidDerivative(:,:)
    real, allocatable:: backPropagatingError(:,:)
    real, allocatable:: hiddenToOutputDerivative(:,:)
    real, allocatable:: inputToHiddenDerivative(:,:)
    real, allocatable:: delta2(:,:), delta2_help(:,:)


    real randomFromRange
    real reallyRandom
    real sigmoid, sigmoidDerivative
    integer i,j,k,l,m
    real cost
    real step

end module
