program main
    use variables
    implicit none

    call trainingPreparation

    call displayTrainingData

    call training

    !call netCheck

    call compareResults

    write(*,*) sin(1.0)
end
