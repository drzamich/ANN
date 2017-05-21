program main
    use variables
    implicit none

    call readInputFile

    call generateTrainingData

    call readTrainingData

    call allocateMatrices

    call assignWeights

    call displayTrainingData

    call training

    call generateCheckingData

    !call plot

    !call netCheck

    !call compareResults

end
