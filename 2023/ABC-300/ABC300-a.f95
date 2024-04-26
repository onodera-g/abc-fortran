program ABC300a
    implicit none
    integer N, A, B, ans, i
    integer, allocatable :: C(:)
    !入力
    READ (*, *) N, A, B
    allocate (C(N))
    READ (*, *) C

    !一致するものがあるか検索
    ans = A + B
    do i = 1, N
        if (ans == C(i)) then
            write (*, "(i0)") i
            stop
        end if
    end do
end program
