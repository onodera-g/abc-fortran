program ABC308b
    implicit none
    integer(8) i, j, total, tmp
    integer(8) N, M
    character(20), allocatable :: C(:), D(:)
    integer, allocatable :: P(:)
    !初期化
    i = 0; j = 0; total = 0; tmp = 0

    !入力
    read (*, *) N, M
    M = M + 1 !対象外の皿の分があるので＋１
    allocate (C(N), D(M), P(M))

    read (*, *) C(:)
    read (*, *) (D(i), i=2, M)
    read (*, *) P(:)

    !処理
    do i = 1, N
        tmp = P(1)
        do j = 2, M
            if (C(i) == D(j)) then
                tmp = P(j)
                exit
            end if
        end do
        total = total + tmp
    end do
    !結果の出力
    write (*, "(i0)") total
end
