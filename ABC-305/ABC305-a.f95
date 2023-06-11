program ABC305a
    implicit none
    real N, kyori_5, kyori_mod, Ans
    !入力
    READ (*, *) N
    !
    kyori_5 = int(N/5)
    Ans = N/5 - kyori_5
    kyori_mod = mod(N, 5.0)

    !最寄りの給水所の場合分け
    if (kyori_mod == 0) then
        write (*, *) int(N)
    else if (Ans > 0.5) then
        Ans = kyori_5*5 + 5
        write (*, *) int(Ans)
    else
        Ans = kyori_5*5
        write (*, *) int(Ans)
    end if
end
