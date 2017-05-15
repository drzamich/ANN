subroutine writeMatrixNew(matrix)
    implicit none
    real, intent(in) :: matrix(:, :)

    write(*,*) matrix
end subroutine
