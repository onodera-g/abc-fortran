program abc321c
    implicit none
    integer(8) K
    integer(8) i, j, n, x, s
    integer(8), allocatable::ans(:), tmp(:)
    ans = [integer ::]; tmp = [integer ::]

    !読み込み
    read (*, *) K

    !321-like Numberの計算
    s = 1
    do i = 2, 1023
        x = 0
        do j = 9, 0, -1
            if (btest(i, j) .eqv. .true.) x = x*10 + j
        end do
        ans = [ans, x]
    end do
    n = size(ans)

    !結果の出力
    call margesort(ans, tmp, n, s, n)
    write (*, '(i0)') ans(K)

contains
    recursive subroutine margesort(x, tmp, N, left, right)
        integer(8) left, right, mid
        integer(8) N
        integer(8) x(N), tmp(N)
        integer(8) i, j, k

        if (left >= right) return

        mid = (left + right)/2
        call margesort(x, tmp, N, left, mid)
        call margesort(x, tmp, N, mid + 1, right)

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
    end subroutine margesort
end program abc321c
