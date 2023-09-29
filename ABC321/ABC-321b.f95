program abc321b
    implicit none
    integer N, X, total, i
    integer, allocatable::A(:), tmp(:), arr(:)

    !読み込み
    read (*, *) N, X
    allocate (A(N), tmp(N), arr(N))
    read (*, *) A(1:N - 1)

    !Nラウンド目の数値検索
    do i = 0, 100
        arr = A; arr(N) = i
        call margesort(arr, tmp, N, 1, N)
        !write (*, *) arr
        total = SUM(arr(2:N - 1))
        if (total >= X) then
            write (*, '(i0)') i
            stop
        end if
    end do
    write (*, '(i0)') - 1

contains
    recursive subroutine margesort(x, tmp, N, left, right)
        integer left, right, mid
        integer N
        integer x(N), tmp(N)
        integer i, j, k

        !これ以上2分かつできないならretrun
        if (left >= right) return

        !分割できるだけ分割する
        mid = (left + right)/2
        call margesort(x, tmp, N, left, mid)
        call margesort(x, tmp, N, mid + 1, right)

        !並び替えの下準備としてtmpに配列をコピー
        j = 0
        tmp(left:mid) = x(left:mid)
        do i = mid + 1, right
            tmp(i) = x(right - j)
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
                i = i + 1
            else if (tmp(i) == tmp(j)) then
                x(k) = tmp(i)
                i = i + 1
            else
                x(k) = tmp(j)
                j = j - 1
            end if
        end do
        !write (*, '(3x,*(f13.10,1x))') x(left:right)
    end subroutine margesort
end program abc321b
