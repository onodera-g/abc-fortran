program abc352b
    !
    implicit none
    character(1), allocatable::A(:, :), B(:, :)
    integer i, j, N

    !入力
    read (*, *) N
    allocate (A(N, N), B(N, N))
    do i = 1, N
        read (*, '(*(a1))') (A(i, j), j=1, N)
    end do
    do i = 1, N
        read (*, '(*(a1))') (B(i, j), j=1, N)
    end do

    do i = 1, N
        do j = 1, N
            if (A(i, j) /= B(i, j)) then
                write (*, *) i, j
                stop
            end if
        end do
    end do
end program abc352b
