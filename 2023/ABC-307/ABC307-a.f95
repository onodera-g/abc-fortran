program ABC307a
    implicit none
    integer(8) i, j, tmp
    integer(8) N
    integer(8), allocatable::A(:), Ans(:)

    !初期化
    i = 0; j = 1; tmp = 0

    !入力
    read (*, *) N
    allocate (A(7*N), Ans(N))
    read (*, *) (A(i), i=1, 7*N)

    !処理
    do i = 1, 7*N
        tmp = tmp + A(i)
        if (i == 7*j) then
            Ans(j) = tmp
            tmp = 0
            j = j + 1
        end if
    end do

    !結果の出力
    write (*, *) Ans
end
