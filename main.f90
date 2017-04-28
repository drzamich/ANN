program main
    implicit none

    integer inputDataColumns, inputLayerCells
    integer inputDataRows, outputDataRows
    integer outputDataColumns, outputLayerCells
    integer hiddenValuesSize
    integer inputToHiddenWeightsRows, inputToHiddenWeightsColumns

    integer hiddenLayerCells

    real lowerRandomWeightValue,upperRandomWeightValue
    real,allocatable:: inputToHiddenWeights(:,:)
    real,allocatable:: hiddenToOutputWeights(:,:)
    real,allocatable:: inputValues(:,:)
    real,allocatable:: hiddenValues(:,:)

    real randomFromRange
    real reallyRandom
    real sigmoid
    integer i

    inputDataRows = 3
    inputDataColumns = 2
    lowerRandomWeightValue = -0.1
    upperRandomWeightValue = 0.1
    hiddenValuesSize = inputDataRows
    inputToHiddenWeightsRows = inputDataColumns
    inputToHiddenWeightsColumns = inputDataRows

    allocate(inputValues(inputDataRows,inputDataColumns))

    ! TEST VALUES OF INPUT DATA
    inputValues = reshape((/1.0,2.0,3.0,6.0,7.0,8.0/),shape(inputValues))
    write(*,*) "Input values"
    call writeMatrix(inputValues,inputDataRows,inputDataColumns)

    allocate(inputToHiddenWeights(inputToHiddenWeightsRows,inputToHiddenWeightsColumns))
    allocate(hiddenValues(hiddenValuesSize,hiddenValuesSize))

    call assignRandomWeights(inputToHiddenWeights,&
                            inputToHiddenWeightsRows,inputToHiddenWeightsColumns,&
                            lowerRandomWeightValue,upperRandomWeightValue)

    write(*,*)
    write(*,*) "Weights from input to hidden layer"
    call writeMatrix(inputToHiddenWeights,inputToHiddenWeightsRows,inputToHiddenWeightsColumns)

    write(*,*)
    write(*,*) "Input values multiplied by weights"
    hiddenValues = matmul(inputValues,inputToHiddenWeights)
    call writeMatrix(hiddenValues,hiddenValuesSize,hiddenValuesSize)

    write(*,*)
    write(*,*) "Sigmoided values of hidden layer"
    call matrixSigmoid(hiddenValues,hiddenValuesSize,hiddenValuesSize)
    call writeMatrix(hiddenValues,hiddenValuesSize,hiddenValuesSize)
end
