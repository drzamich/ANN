program main
    implicit none

    integer inputDataColumns, inputLayerCells
    integer inputDataRows, outputDataRows
    integer outputDataColumns, outputLayerCells

    integer hiddenLayerCells

    real,allocatable:: inputToHiddenWeights(:,:)
    real,allocatable:: hiddenToOutputWeights(:,:)

    real randomFromRange
    real reallyRandom
    integer i
    real test(10,2)

    call assignRandomWeights(test,10,2,-0.1,0.1)

    call writeMatrix(test,10,2)

end
