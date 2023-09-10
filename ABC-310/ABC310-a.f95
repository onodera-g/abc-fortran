program ABC310a
    implicit none
    integer(8) N, P, Q, Ans, min
    integer(8) i
    integer, allocatable :: D(:)

    !初期化
    N = 0; P = 0; Q = 0
    Ans = 0; min = 10**6

    !入力
    read (*, *) N, P, Q
    allocate (D(N))
    read (*, *) D(:)

    !処理
    do i = 1, N
        if (min > D(i)) min = D(i)
    end do
    if (P > Q + min) then
        Ans = Q + min
    else
        Ans = P
    end if

    !結果の出力
    write (*, "(i0)") Ans
end
