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

real function randomFromRange(lowerRange,upperRange)
    implicit none

    real lowerRange, upperRange
    real ranNum
    real randNum

    real rangeProduct
    real wholeRange
    real probabilityOfPositive


    rangeProduct = lowerRange*upperRange


    !if both ranges have the same sign
    if(rangeProduct.gt.0.0) then

        !if both ranges are negative
        if(lowerRange.lt.0.0) then
            do
                ranNum=randNum()*lowerRange
                if(ranNum.le.upperRange) then
                    randomFromRange = ranNum
                    return
                end if
            end do

        !if both ranges are positive
        else if(lowerRange.gt.0.0) then
            do
                ranNum=randNum()*upperRange
                if(ranNum.ge.lowerRange) then
                    randomFromRange = ranNum
                    return
                end if
            end do
        end if


    !if ranges have different signs
    else if (rangeProduct.lt.0.0) then

        !we have to assure that the distribution will be uniform

        wholeRange = abs(lowerRange) + upperRange
        probabilityOfPositive = upperRange/wholeRange

        !we choose a random number and compare it with the share if the positive range in the whole range
        if(randNum().le.probabilityOfPositive) then
            randomFromRange = randNum()*upperRange
        else
            randomFromRange = randNum()*lowerRange
        end if

    !if range product equals zero = one of the ranges is equal to zero
    else
        if(lowerRange.eq.0.0) then
            randomFromRange = randNum()*upperRange
        else
            randomFromRange = randNum()*lowerRange
        end if
    end if
end function

real function randNum()
    implicit none

    call random_number(randNum)

end function
