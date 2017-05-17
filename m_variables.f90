module variables
    integer hiddenLayerCells
    integer inputDataRows, inputDataColumns
    integer outputDataRows, outputDataColumns

    integer hiddenValuesRows, hiddenValuesColumns
    integer inputToHiddenWeightsRows, inputToHiddenWeightsColumns
    integer hiddenToOutputWeightsRows, hiddenToOutputWeightsColumns


    real lowerRandomWeightValue,upperRandomWeightValue

    real, allocatable:: inputValuesNormalized(:,:), outputValuesNormalized(:,:), outputValuesNormalizedForced(:,:)
    real, allocatable:: outputValuesExpectedNormalized(:,:), outputValuesDenormalized(:,:)
    real, allocatable:: outputValuesExpectedNormalizedSigmoided(:,:)



    real,allocatable:: inputToHiddenWeights(:,:), inputToHiddenWeightsDerivatives(:,:)
    real,allocatable:: hiddenToOutputWeights(:,:), hiddenToOutputWeightsDerivatives(:,:)
    real,allocatable:: inputValues(:,:)
    real,allocatable:: hiddenValues(:,:), hiddenValuesSigmoid(:,:), hiddenValuesSigmoidDerivatives(:,:)
    real, allocatable:: outputValues(:,:), outputValuesExpected(:,:), outputValuesSaved(:,:)
    real, allocatable:: outputValuesSigmoid(:,:), outputValuesSigmoidDerivative(:,:)
    real, allocatable:: delta3(:,:)
    real, allocatable:: hiddenToOutputDerivative(:,:)
    real, allocatable:: inputToHiddenDerivative(:,:)
    real, allocatable:: delta2(:,:)

    real outputValuesParameters(3), inputValuesParameters(3)
    real,allocatable:: costValues(:)

    real randomFromRange
    real reallyRandom
    real sigmoid, sigmoidDerivative
    integer i,j,k,l,m
    real cost
    real step, momentum
    integer iterationSteps
    integer stepIndicatorProgram, stepIndicatorChart

    real, allocatable:: inputTraining(:,:), outputTraining(:,:), inputTrainingNormalized(:,:), outputTrainingNormalized(:,:)
    real, allocatable:: trainingSigmoid(:,:)
    real trainingParameters(3)

    real testValue(1,1)
    real, allocatable :: hiddenValuesChecking(:,:), hiddenValuesCheckingSigmoid(:,:)
    real outputValuesChecking(1,1), outputValuesCheckingDenormalized(1,1)
end module
