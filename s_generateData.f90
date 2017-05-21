!
! DEFINING THE OBJECTIVE FUNCTION
!
real function objective(x)
    implicit none
    real x
    objective =2*x
end function

subroutine generateTrainingData
    use variables
    real x, step1
    real objective

    x = lowerInterpBoundary

    step1=(upperInterpBoundary-lowerInterpBoundary)/pointsTraining
    open(10,file='dataInput.txt')
    open(11,file='dataOutput.txt')
    write(10,*) pointsTraining+1, "Number of points"
    do i=1,pointsTraining+1
        write(10,*) x
        write(11,*) objective(x)
        x=x+step1
    end do
    close(10)
    close(11)
end subroutine

subroutine readTrainingData
    use variables

    open(10,file='dataInput.txt')
    open(11,file='dataOutput.txt')

    read(10,*) inputDataRowsR
    inputDataRows=int(inputDataRowsR)


    allocate(inputValues(inputDataRows,inputDataColumns))

    outputDataRows=inputDataRows
    allocate(outputValuesExpected(outputDataRows,outputDataColumns))

    do i=1,inputDataRows
        read(10,*) (inputValues(i,j), j=1,inputDataColumns) !short form of the loop
        read(11,*) (outputValuesExpected(i,j), j=1,outputDataColumns) !short form of the loop
   enddo

    write(*,*) inputValues
    write(*,*) outputValuesExpected
   outputValuesExpected=outputValuesExpected*factor
end subroutine



subroutine generateCheckingData
    use variables

end subroutine
