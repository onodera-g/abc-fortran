program abc329b
    !N    ：整数の総数
    !A    ：整数(A1,A2,...AN)
    !max_A：
    implicit none
    integer(16) N, i, max_A
    integer(16), allocatable :: A(:)

    !入力
    read (*, *) N
    allocate (A(N))
    read (*, *) A

    !ソート
    call margesort(A, N)

    !出力
    max_A = A(1)
    do i = 2, N
        if (max_A /= A(i)) then
            write (*, '(i0)') A(i)
            stop
        end if
    end do

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
        character(1) y(N), tmp2(N)
        integer(16) i, j, k

        !これ以上2分かつできないならretrun
        if (left >= right) return

        !分割できるだけ分割する
        mid = (left + right)/2
        call loop_margesort(x, tmp, N, left, mid)
        call loop_margesort(x, tmp, N, mid + 1, right)

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
            if (tmp(i) > tmp(j)) then
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

end program abc329b
