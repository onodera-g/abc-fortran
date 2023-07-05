module global_variable
    integer(4) N
end module
program ABC308c
    use global_variable
    implicit none
    real(16), allocatable :: A(:), B(:), D(:), tmp(:)
    integer(16), allocatable :: C(:), tmp2(:)
    integer(16) start, end, i

    !初期化
    i = 0
    read (*, *) N
    allocate (A(N), B(N), C(N), D(N))
    allocate (tmp(N), tmp2(N))
    !
    do i = 1, N
        read (*, *) A(i), B(i)
        C(i) = i
    end do
    D = (A/(A + B))
    start = 1; end = N
    tmp = 0; tmp2 = 0
    call margesort(D, C, tmp, tmp2, start, end)
    write (*, '(*(i0,1x))') C
end program
recursive subroutine margesort(x, y, tmp, tmp2, left, right)
    integer(16) left, right, mid
    integer(16) N
    real(16) x(N), tmp(N)
    integer(16) y(N), tmp2(N)
    integer(16) i, j, k

    !これ以上2分かつできないならretrun
    if (left >= right) return

    !分割できるだけ分割する
    mid = (left + right)/2
    call margesort(x, y, tmp, tmp2, left, mid)
    call margesort(x, y, tmp, tmp2, mid + 1, right)

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
end subroutine margesort

