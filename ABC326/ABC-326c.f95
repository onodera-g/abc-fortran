program abc326c
    ! N         ：直線上に置かれたプレゼントの総数
    ! M         ：半開区間
    ! A         ：直線上に置かれたプレゼントの座標
    !cnt_current：現在の区間で獲得できるプレゼントの数
    !cnt_max    ：獲得できるプレゼントの最大数
    implicit none
    integer(16) N, M
    integer(16), allocatable::A(:)
    integer(16) i, j, cnt_current, cnt_max

    !入力
    read (*, *) N, M
    allocate (A(N))
    read (*, *) A(:)
    call margesort(A, N)

    !最大値の検索
    cnt_max = 0
    j = 2
    cnt_current = 1
    do i = 1, N
        do
            if (M + A(i) > A(j) .and. j <= N) then
                cnt_current = cnt_current + 1
                j = j + 1
            else
                exit
            end if
        end do
        cnt_max = max(cnt_current, cnt_max)
        cnt_current = cnt_current - 1
    end do

    !結果の出力
    write (*, *) cnt_max
contains
    subroutine margesort(x, n)
        integer(16) N
        integer(16) x(N), tmp(N)
        integer(16) start, end
        start = 1; end = N
        call loop_margesort(x, tmp, N, start, end)
    end subroutine
    recursive subroutine loop_margesort(x, tmp, N, left, right)
        integer(16) left, right, mid
        integer(16) N
        integer(16) x(N), tmp(N)
        integer(16) i, j, k

        if (left >= right) return

        mid = (left + right)/2
        call loop_margesort(x, tmp, N, left, mid)
        call loop_margesort(x, tmp, N, mid + 1, right)

        j = 0
        tmp(left:mid) = x(left:mid)
        do i = mid + 1, right
            tmp(i) = x(right - j)
            j = j + 1
        end do

        i = left
        j = right
        do k = left, right
            if (tmp(i) < tmp(j)) then
                x(k) = tmp(i)
                i = i + 1
            else if (tmp(i) == tmp(j)) then
                x(k) = tmp(i)
                i = i + 1
            else
                x(k) = tmp(j)
                j = j - 1
            end if
        end do
    end subroutine loop_margesort
end program abc326c
