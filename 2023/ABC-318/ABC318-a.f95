program abc318a
    implicit none
    integer N, M, P
    integer ans

    !読み込み
    read (*, *) N, M, P

    !満月の日数計算
    if (N - M < 0) then
        write (*, '(i0)') 0
        stop
    end if
    ans = (N - M)/P + 1

    !結果の出力
    write (*, '(i0)') ans
end program abc318a
