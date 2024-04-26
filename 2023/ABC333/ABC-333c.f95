program abc333c
    !rep(cnt)           ：3つのレピュニットの和
    !rep_i, rep_j, rep_k：3つのレピュニットの値
    !N                  ：知りたいレピュニットの和の位置
    implicit none
    integer(16) N, rep_N
    integer(16) rep(1728)
    integer(16) rep_i, rep_j, rep_k
    integer(16) i, j, k, cnt

    !入力
    read (*, *) N

    !3つのレピュニットの和を列挙
    rep_i = 0; rep_j = 0; rep_k = 0
    rep = 0; cnt = 1
    do i = 0, 11
        rep_j = 0
        rep_i = rep_i + 10**i
        do j = 0, 11
            rep_j = rep_j + 10**j
            rep_k = 0
            do k = 0, 11
                rep_k = rep_k + 10**k
                rep(cnt) = rep_i + rep_j + rep_k
                cnt = cnt + 1
            end do
        end do
    end do

    !小さい順に並びかえ、被りの除去
    rep_N = 1728
    call margesort(rep, rep_N)
    call check(rep, rep_N)
    write (*, *) rep(N)
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

        !これ以上2分かつできないならretrun
        if (left >= right) return

        !分割できるだけ分割する
        mid = (left + right)/2
        call loop_margesort(x, tmp, N, left, mid)
        call loop_margesort(x, tmp, N, mid + 1, right)

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
    end subroutine loop_margesort
    subroutine check(arr, N)
        integer(16) N, cnt
        integer(16) arr(N), arr2(N)
        arr2 = 0; cnt = 1
        arr2(1) = arr(1)
        do i = 1, N
            if (arr2(cnt) /= arr(i)) then
                cnt = cnt + 1
                arr2(cnt) = arr(i)
            end if
        end do
        arr = arr2
    end subroutine check
end program abc333c

