program abc328a
    !N：問題数
    !S：各問題の配点
    !X：集計したい配点の閾値
    !ans：X点以下の問題の配点の合計
    implicit none
    integer(16) N, X, i, ans
    integer(16), allocatable :: S(:)

    !入力
    read (*, *) N, X
    allocate (S(N))
    read (*, *) S

    !配点の合計計算
    ans = 0
    do i = 1, N
        if (S(i) <= X) then
            ans = ans + S(i)
        end if
    end do

    !結果の出力
    write (*, '(i0)') ans
end program abc328a
