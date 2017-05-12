subroutine plotCost
    use variables
    implicit none

    open(10,file='cost.txt')
    do i=1,iterationSteps
        write(10,*) costValues(i)
    end do
    close(10)

    open(10,file='plot.plt')
    write(10,*) 'set title "Cost function"'
    write(10,*) 'set xrange[1:',iterationSteps,']'
    !write(10,*) 'set yrange[',costValues(iterationSteps),':',costValues(1),']'
    write(10,*) 'set yrange[',costValues(iterationSteps),':',costValues(1),']'
    write(10,*) 'set xlabel "Step"'
    write(10,*) 'set xtics 100'
    write(10,*) 'set style data lines'
    write(10,*) 'set output plot.png'
    write(10,*) 'plot "cost.txt" title "Cost"'
    close(10)

    call system('binary\gnuplot\wgnuplot plot.plt')
end subroutine