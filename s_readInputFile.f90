subroutine readInputFile
    use variables
    implicit none

    open(10,file='input.txt')
    read(10,*) inputDataRows
    read(10,*) inputDataColumns
    read(10,*) outputDataColumns

    read(10,*)
    read(10,*)

    allocate(inputValues(inputDataRows,inputDataColumns))

    do i=1,inputDataRows
        read(10,*) (inputValues(i,j), j=1,inputDataColumns) !short form of the loop
    enddo

    read(10,*)
    read(10,*)

    outputDataRows=inputDataRows
    allocate(outputValuesExpected(outputDataRows,outputDataColumns))

    do i=1,outputDataRows
        read(10,*) (outputValuesExpected(i,j), j=1,outputDataColumns) !short form of the loop
    enddo

    read(10,*)
    read(10,*) lowerRandomWeightValue
    read(10,*) upperRandomWeightValue

    close(10)
end subroutine
