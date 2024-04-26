program ABC303b
    implicit none
    integer i, j, k
    integer N, M, cnt_bad, cnt_pair
    character(4) pair_current
    integer, allocatable:: a(:, :)

    character(:), allocatable ::pair_all(:, :)
    !入力
    READ (*, *) N, M
    allocate (a(N, M))
    READ (*, *) a

    !ペアの組み合わせの総数の計算
    cnt_pair = 0
    do i = N - 1, 1, -1
        cnt_pair = cnt_pair + i
    end do
    allocate (character(4) :: pair_all(cnt_pair, 2))

    !ペアの組み合わせの作成
    cnt_pair = 0
    do i = 1, N - 1
        do j = 1, N - i
            cnt_pair = cnt_pair + 1
            write (pair_all(cnt_pair, 1), '(2i2)') a(i, 1), a(i + j, 1)
            write (pair_all(cnt_pair, 2), '(2i2)') a(i + j, 1), a(i, 1)
        end do
    end do

    !不仲である可能性の判定
    cnt_bad = 0
    do i = 1, cnt_pair
        tate: do j = 1, M
            yoko: do k = 1, N - 1
                write (pair_current, '(2i2)') a(k, j), a(k + 1, j)
                if (pair_all(i, 1) == pair_current) exit tate
                if (pair_all(i, 2) == pair_current) exit tate

            end do yoko
            if (j == M) cnt_bad = cnt_bad + 1
        end do tate
    end do

    !結果の出力
    write (*, '(i0)') cnt_bad
end

