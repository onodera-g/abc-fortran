program abc322a
    implicit none
    integer i, N
    character(:), allocatable :: S

    !入力
    read (*, *) N
    allocate (character(N)::S)
    read (*, *) S

    !A,B,Cのカウント
    do i = 1, N - 2
        !write (*, *) S(i:i + 2)
        if (S(i:i + 2) == 'ABC') then
            write (*, *) i
            stop
        end if
    end do
    write (*, *) - 1
end program abc322a
