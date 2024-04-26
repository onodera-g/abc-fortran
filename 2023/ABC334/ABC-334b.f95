program abc333b
    !L：高橋君の位置
    !R：青木君の位置
    !A：基点になる座標
    !M：ツリーを建てる間隔
    implicit none
    integer(16) A, M, L, R, X, ans

    !入力
    read (*, *) A, M, L, R

    !クリスマスツリーの本数計算
    L = L - A; R = R - A
    X = 10.0**18
    L = L + X*m; R = R + X*m
    ans = (R/M) - ((L - 1)/M)

    !結果出力
    write (*, *) ans
end program abc333b
