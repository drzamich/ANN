module variables
    integer hiddenLayerCells
    integer inputDataColumns, inputLayerCells
    integer inputDataRows, outputDataRows
    integer outputDataColumns, outputLayerCells
    integer hiddenValuesRows, hiddenValuesColumns
    integer inputToHiddenWeightsRows, inputToHiddenWeightsColumns
    integer hiddenToOutputWeightsRows, hiddenToOutputWeightsColumns


    real lowerRandomWeightValue,upperRandomWeightValue
    real,allocatable:: inputToHiddenWeights(:,:), inputToHiddenWeightsDerivatives(:,:)
    real,allocatable:: hiddenToOutputWeights(:,:), hiddenToOutputWeightsDerivatives(:,:)
    real,allocatable:: inputValues(:,:)
    real,allocatable:: hiddenValues(:,:), hiddenValuesSigmoid(:,:), hiddenValuesSigmoidDerivatives(:,:)
    real, allocatable:: outputValues(:,:), outputValuesExpected(:,:), outputValuesDifferences(:,:)
    real, allocatable:: outputValuesSigmoid(:,:), outputValuesSigmoidDerivative(:,:)
    real, allocatable:: delta3(:,:)
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
