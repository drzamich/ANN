subroutine assignRandomWeights(matrix,rows,columns,lowerRange,upperRange)
    implicit none

    integer rows, columns
    real matrix(rows,columns)
    real lowerRange, upperRange, randomFromRange
    integer i,j

    !
    ! RANDOM SEED STUFF BEGIN
    integer values(8)
    integer ka
    integer, allocatable :: seed(:)

    call date_and_time(values=values)
    call random_seed(size=ka)
    allocate(seed(ka))
    seed(:) = values(:)
    call random_seed(put=seed)

    !
    ! RANDOM SEED STUFF END
    !


    do i=1,rows
        do j=1,columns
            matrix(i,j) = randomFromRange(lowerRange,upperRange)
        end do
    end do


end subroutine
