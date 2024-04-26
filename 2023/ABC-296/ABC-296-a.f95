program ABC296A
    !implicit none
    integer :: N, i
    character(:), allocatable :: S
    !character:: S(5)
    !write (*, *) '>> N'
    read (*, *) N
    allocate (character(N) :: S)
    !write (*, *) '>> S'
    read (*, *) S
    !write (*, *) S
    do i = 1, N - 1, 1
        if (S(i:i) == S(i + 1:i + 1)) then
            write (*, "(a)") 'No'
            stop
        end if
        !print *, S(i:i + 1)
    end do
    write (*, "(a)") 'Yes'
end program ABC296A
