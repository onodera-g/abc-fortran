program ABC317a
    implicit none
    integer i, Ans, N, H, X
    integer, allocatable :: P(:)

    !入力
    read (*, *) N, H, X
    allocate (P(N))
    read (*, *) P(:)

    !判定
    do i = 1, N
        Ans = H + P(i)
        if (Ans >= X) then
            write (*, '(i0)') i
            stop
        end if
    end do
end program abc317a
