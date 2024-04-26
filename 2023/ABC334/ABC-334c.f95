program abc333c
    !N      ：靴下の組数
    !K      ：無くした靴下の数
    !A      ：無くした靴下の種類
    !min_Ans：奇妙さの最小値
    !Ans    ：奇妙さの計算
    implicit none
    integer N, K
    integer i, Ans, min_Ans
    integer, allocatable::A(:)

    !入力
    read (*, *) N, K
    allocate (A(K))
    read (*, *) A(:)
    Ans = 0

    !最小の奇妙さの計算
    if (mod(K, 2) == 0) then !無くした靴下が偶数個の場合
        do i = 1, K - 1, 2
            Ans = Ans + (A(i + 1) - A(i))
        end do
        min_Ans = Ans
    else !無くした靴下が奇数個の場合
        !1番目の靴下を使わない
        do i = 2, K - 1, 2
            Ans = Ans + (A(i + 1) - A(i))
        end do
        min_Ans = Ans
        !3~K番目の靴下を使わない
        do i = 3, K, 2
            Ans = Ans - (A(i) - A(i - 1))
            Ans = Ans + (A(i - 1) - A(i - 2))
            min_Ans = min(min_Ans, Ans)
        end do
    end if

    !結果の出力
    write (*, *) min_Ans
end program abc333c

