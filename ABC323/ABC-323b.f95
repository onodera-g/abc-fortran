module global_abc323b
    ! N：プレイヤー数
    ! S：各プレイヤーの対戦結果
    implicit none
    character(:), allocatable :: S(:)
    integer N
end module
program abc323b
    ! cnt：各プレイヤーの並び順と勝利数
    ! tmp：マージソート用の仮変数
    use global_abc323b
    implicit none
    integer, allocatable:: cnt(:, :), tmp(:), tmp2(:)
    integer i

    read (*, *) N

    allocate (character(N)::S(N))
    allocate (cnt(N, 2), tmp(N), tmp2(N))
    do i = 1, N
        cnt(i, 2) = i
    end do

    read (*, *) S

    do i = 1, N
        call cnt_win(i, cnt)
    end do

    call margesort(cnt(:, 1), cnt(:, 2), tmp, tmp2, N, 1, N)

    write (*, '(*(1x,i0))') cnt(:, 2)

contains
    subroutine cnt_win(i, cnt)
        integer i, cnt(N, 2)
        integer j

        do j = 1, N
            if (S(i) (j:j) == 'o') cnt(i, 1) = cnt(i, 1) + 1
        end do
    end subroutine cnt_win

    recursive subroutine margesort(x, y, tmp, tmp2, N, left, right)
        integer(4) left, right, mid
        integer(4) N
        integer(4) x(N), tmp(N)
        integer(4) y(N), tmp2(N)
        integer(4) i, j, k

        !これ以上2分かつできないならretrun
        if (left >= right) return

        !分割できるだけ分割する
        mid = (left + right)/2
        call margesort(x, y, tmp, tmp2, N, left, mid)
        call margesort(x, y, tmp, tmp2, N, mid + 1, right)

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
end program abc323b
