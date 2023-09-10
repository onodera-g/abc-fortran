program ABC317b
    implicit none
    integer N, i
    integer, allocatable ::  A(:), tmp(:)

    !入力
    read (*, *) N
    allocate (A(N), tmp(N))
    read (*, *) A(:)

    !Aを昇順にソート
    call margesort(A, tmp, N, 1, N)

    !不連続部分を調べる
    do i = 1, N
        if (A(i) - A(i + 1) > 1) then
            write (*, '(i0)') A(i) - 1
            stop
        end if
    end do
contains
    recursive subroutine margesort(x, tmp, N, left, right)
        integer left, right, mid
        integer N
        integer x(N), tmp(N)
        integer i, j, k
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
end program abc317b
