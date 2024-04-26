program abc330b
    !N   :整数列Aの長さ
    !L,R :整数L、R
    !A(N):整数列
    implicit none
    integer(16) N, L, R, i
    integer(16), allocatable :: A(:)

    !入力
    read (*, *) N, L, R
    allocate (A(N))
    read (*, *) A

    !場合分けして出力
    do i = 1, N
        if (A(i) >= L .and. A(i) < R) then !L以上R未満
            write (*, '(i0,1x)', advance='no') A(i)
        elseif (A(i) < L) then !L未満
            write (*, '(i0,1x)', advance='no') L
        elseif (A(i) >= R) then !R以上
            write (*, '(i0,1x)', advance='no') R
        end if
    end do
end program abc330b
