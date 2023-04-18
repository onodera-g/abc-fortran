program A
    implicit none
    integer::N, D, i
    integer, allocatable :: T(:)

    read *, N, D
    allocate (T(N))
    read *, T

    do i = 1, N - 1, 1
        !print *, T(i + 1), '-', T(i), '=', T(i + 1) - T(i)
        if (T(i + 1) - T(i) <= D) then
            write (*, "(i0)") T(i + 1)
            stop
        end if
    end do
    write (*, "(i0)") - 1

end program A
