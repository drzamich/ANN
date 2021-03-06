subroutine readInputFile
    use variables
    implicit none

    open(10,file='input.txt')
    read(10,*)
    read(10,*) hiddenLayerCells
    read(10,*) additionalLayerCells
    read(10,*)
    read(10,*)
    read(10,*) inputDataColumns
    read(10,*) outputDataColumns
    read(10,*) lowerInterpBoundary
    read(10,*) upperInterpBoundary
    read(10,*) pointsTraining
    read(10,*) pointsChecking
    read(10,*) lowerRandomWeightValue
    read(10,*) upperRandomWeightValue
    read(10,*) factor
    read(10,*)
    read(10,*)
    read(10,*) iterationSteps
    read(10,*) stepIndicatorProgram
    read(10,*) stepIndicatorChart
    close(10)

end subroutine
