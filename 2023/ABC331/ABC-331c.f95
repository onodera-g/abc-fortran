program abc331c
    !A(N) ：長さNの配列
    !A2(N)：ソート後の配列A(N)
    implicit none
    integer(16) N, i, j, cnt
    integer(16), allocatable::A(:), An(:), A2(:)

    read (*, *) N
    allocate (A(N), An(N), A2(N))
    read (*, *) A(:)
    do i = 1, N
        An(i) = i
    end do
    call margesort(A, An, N)

    cnt = sum(A)
    do i = 1, N
        if (i /= 1 .and. A(i) == A(i - 1)) then
            A2(i) = cnt
            cycle
        else
            cnt = cnt - A(i)
        end if

        do j = i + 1, N
            if (A(i) < A(j)) exit
            cnt = cnt - A(j)
        end do
        A2(i) = cnt
    end do

    call margesort(An, A2, N)
    do i = 1, N
        write (*, '(i0,1x)', advance='no') A2(i)
    end do

contains
    subroutine margesort(x, y, n)
        integer(16) N
        integer(16) x(N), tmp(N)
        integer(16) y(N), tmp2(N)
        integer(16) start, end
        start = 1; end = N
        call loop_margesort(x, y, tmp, tmp2, N, start, end)
    end subroutine
    recursive subroutine loop_margesort(x, y, tmp, tmp2, N, left, right)
        integer(16) left, right, mid
        integer(16) N
        integer(16) x(N), tmp(N)
        integer(16) y(N), tmp2(N)
        integer(16) i, j, k

        !これ以上2分かつできないならretrun
        if (left >= right) return

        !分割できるだけ分割する
        mid = (left + right)/2
        call loop_margesort(x, y, tmp, tmp2, N, left, mid)
        call loop_margesort(x, y, tmp, tmp2, N, mid + 1, right)

        !並び替えの下準備としてtmpに配列をコピー
        j = 0
        tmp(left:mid) = x(left:mid)
        tmp2(left:mid) = y(left:mid)
        do i = mid + 1, right
            tmp(i) = x(right - j)
            tmp2(i) = y(right - j)
            j = j + 1
        end do

        !大小比較して小さい順に入れていく
        i = left
        j = right
        !write (*, '(3x,*(f13.101x),a)', advance='no') x(left:right)
        !write (*, '(a)', advance='no') '>>'
        do k = left, right
            if (tmp(i) < tmp(j)) then
                x(k) = tmp(i)
                y(k) = tmp2(i)
                i = i + 1
            else if (tmp(i) == tmp(j) .and. tmp2(i) < tmp2(j)) then
                x(k) = tmp(i)
                y(k) = tmp2(i)
                i = i + 1
            else
                x(k) = tmp(j)
                y(k) = tmp2(j)
                j = j - 1
            end if
        end do
        !write (*, '(3x,*(f13.10,1x))') x(left:right)
    end subroutine loop_margesort
end program abc331c

