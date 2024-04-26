program ABC306b
    implicit none
    integer(16) i, A(64), Ans
    !初期化
    A = 0; i = 0; Ans = 0

    !入力
    read (*, *) (A(i), i=1, 64)

    !計算部分
    do i = 1, 64
        if (A(i) == 1) Ans = Ans + A(i)*2**(i - 1)
    end do

    !結果の出力
    write (*, "(i0)") Ans
end
