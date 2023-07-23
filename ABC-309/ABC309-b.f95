program ABC309b
    implicit none
    integer i, j
    integer N
    integer, allocatable :: A(:, :), B(:, :)
    !初期化
    i = 0; j = 0; N = 0

    !入力
    read (*, *) N
    allocate (A(N, N), B(N, N))
    do i = 1, N
        read (*, '(*(i1))') (A(i, j), j=1, N) !1文字区切りで入力
    end do

    !時計回りに回転
    B = A
    do i = 1, N
        do j = 1, N
            !横の移動
            if (i == 1 .and. j /= 1) then
                B(1, j) = A(1, j - 1)
            else if (i == N .and. j /= N) then
                B(N, j) = A(N, j + 1)
            end if
            !縦の移動
            if (j == 1 .and. i /= N) then
                B(i, 1) = A(i + 1, 1)
            elseif (j == N .and. i /= 1) then
                B(i, N) = A(i - 1, N)
            end if
        end do
    end do

    !結果の出力
    do i = 1, N
        write (*, '(*(i1))') (B(i, j), j=1, N)
    end do
end
