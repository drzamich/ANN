subroutine plot
    use variables
    implicit none

    !Saving the values of cost function to the file
    open(10,file='cost.txt')
            write(10,*) 1, costValues(1)
    do i=1,iterationSteps
        if(mod(i,stepIndicatorChart)==0) then
            write(10,*) i, costValues(i)
        endif
    end do
    close(10)

    !plotting the graph with cost function
    call system('gnuplot plotCost.plt')

    !plotting the graph with comparison of functions
    call system('gnuplot plotComparison.plt')
end subroutine

subroutine costFunction(matrixA,matrixB,rows,columns,cost)
    implicit none
    real cost
    integer rows, columns
    integer i,j
    real matrixA(rows,columns)
    real matrixB(rows,columns)

    cost=0.0

    do i=1,rows
        do j=1,columns
            cost=cost+0.5*((matrixA(i,j)-matrixB(i,j))**2)
        end do
    end do
end subroutine
