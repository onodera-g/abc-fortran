program abc318c
    implicit none
    integer(16) N, D, P
    integer(16) i, ticket, start, total, ans, diff
    integer(16), allocatable::F(:), tmp(:)

    !入力
    read (*, *) N, D, P
    allocate (F(N), tmp(N))
    read (*, *) F(:)

    !最小金額の計算
    ticket = 0; start = 1
    call margesort(F, tmp, N, start, N)
    total = sum(F)
    ans = total
    do i = 1, N, D
        diff = D - 1 + i
        if (diff > N) diff = N
        total = total - sum(F(i:diff)) + P
        ans = min(ans, total)
        !write (*, '(i0)') total
    end do

    write (*, '(i0)') ans

contains
    recursive subroutine margesort(x, tmp, N, left, right)
        integer(16) left, right, mid
        integer(16) N
        integer(16) x(N), tmp(N)
        integer(16) i, j, k

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
            if (tmp(i) > tmp(j)) then
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
end program abc318c

