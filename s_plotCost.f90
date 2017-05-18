subroutine plotCost
    use variables
    implicit none

    open(10,file='cost.txt')
            write(10,*) 1, costValues(1)
    do i=1,iterationSteps
        if(mod(i,stepIndicatorChart)==0) then
            write(10,*) i, costValues(i)
        endif
    end do
    close(10)

    open(10,file='plot.plt')
    write(10,*) 'set title "Cost function"'
    write(10,*) 'set xrange[1:',iterationSteps,']'
    !write(10,*) 'set yrange[',costValues(iterationSteps),':',costValues(1),']'
    !write(10,*) 'set yrange[',costValues(iterationSteps),':',costValues(1),']'
    write(10,*) 'set logscale x'
    write(10,*) 'set logscale y'
    write(10,*) 'set xlabel "Step"'
    write(10,*) 'set ylabel "Cost value"'
    !write(10,*) 'set xtics 1000'
    write(10,*) 'set style data lines'
    write(10,*) 'set terminal png'
    write(10,*) 'set output "plot.png"'
    write(10,*) 'plot "cost.txt" title "Cost"'
    close(10)

    call system('binary\gnuplot\wgnuplot plot.plt')
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
