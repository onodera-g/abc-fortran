program abc352c
    !
    implicit none
    integer, allocatable::A(:), ans(:, :)
    integer i, N, cnt

    !入力
    read (*, *) N
    allocate (A(N), ans(2, N))
    read (*, *) A(:)
    Ans = 0
    !
    ans(1, 1) = A(1)
    ans(2, 1) = 1
    cnt = 1
    do i = 2, N
        !仮で入力
        cnt = cnt + 1
        ans(1, cnt) = A(i)
        ans(2, cnt) = 1
        !連鎖のチェック
        call ball(ans, cnt, N)
    end do
    write (*, *) sum(ans(2, :))

contains
    recursive subroutine ball(ans, cnt, N)
        integer cnt, N
        integer ans(2, N)
        if (ans(1, cnt - 1) == ans(1, cnt)) then
            !入力取り消し
            ans(1, cnt) = 0
            ans(2, cnt) = 0
            !入力確定
            ans(1, cnt - 1) = ans(1, cnt - 1) + 1
            !連鎖のチェック
            cnt = cnt - 1
            if (cnt == 0) return
            call ball(ans, cnt, N)
        end if
    end subroutine
end program abc352c
