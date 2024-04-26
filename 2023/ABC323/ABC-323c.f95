module global_abc323c
    ! M　　　：問題数
    ! N　　　：プレイヤー人数
    ! A(2,M)：各問題の並び順と得点
    ! S(N)  ：各プレイヤーの回答状況(回答済o,未回答x)
    implicit none
    character(:), allocatable :: S(:)
    integer, allocatable::A(:, :)
    integer N, M
end module
program abc323c
    ! submit(N,M)：各プレイヤーの回答状況(回答済1,未回答は得点)
    ! score(2,N) ：各プレイヤーの並び順と総得点
    ! cnt(N)　　　：1位になるために必要な回答数
    ! score_max  ：現時点の最高総得点
    ! tmp        ：マージソート用の仮変数
    use global_abc323c
    implicit none
    integer, allocatable:: submit(:, :), score(:, :), tmp(:), tmp2(:), tmp3(:), tmp4(:), cnt(:)
    integer i, score_max, j

    !入力
    read (*, *) N, M
    allocate (A(2, M), submit(N, M), score(2, N), tmp(N), tmp2(N), tmp3(M), tmp4(M), cnt(N))
    allocate (character(M)::S(N))
    read (*, *) A(2, :)
    read (*, *) S
    do i = 1, M
        A(1, i) = i
    end do
    do i = 1, N
        score(:, i) = i
    end do

    !得点並び替え
    call sort_margesort(A(2, :), A(1, :), tmp3, tmp4, M, 1, M)

    !現状の集計
    submit = 1
    do i = 1, N
        do j = 1, M
            if (S(i) (A(1, j):A(1, j)) == 'o') then
                score(2, i) = score(2, i) + A(2, j)
            else
                submit(i, j) = A(2, j)
            end if
        end do
        call sort_margesort(submit(i, :), cnt, tmp3, tmp4, M, 1, M)
    end do
    call sort_margesort(score(2, :), score(1, :), tmp, tmp2, N, 1, N)
    score_max = score(2, 1)

    !1位になるための問題数の算定
    cnt = 0
    do i = 2, N
        do j = 1, M
            score(2, i) = score(2, i) + submit(score(1, i), j)
            cnt(i) = cnt(i) + 1
            if (score(2, i) > score_max) exit
        end do
    end do
    call sort_margesort(score(1, :), cnt, tmp, tmp2, N, 1, N)

    !結果出力
    do i = N, 1, -1
        write (*, '(i0)') cnt(i)
    end do

contains
    !マージソート
    recursive subroutine sort_margesort(x, y, tmp, tmp2, N, left, right)
        integer(4) left, right, mid
        integer(4) N
        integer(4) x(N), tmp(N)
        integer(4) y(N), tmp2(N)
        integer(4) i, j, k

        !これ以上2分かつできないならretrun
        if (left >= right) return

        !分割できるだけ分割する
        mid = (left + right)/2
        call sort_margesort(x, y, tmp, tmp2, N, left, mid)
        call sort_margesort(x, y, tmp, tmp2, N, mid + 1, right)

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
    end subroutine sort_margesort
end program abc323c
