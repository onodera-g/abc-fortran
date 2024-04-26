program abc331a
    !MM   ：1年間の月数
    !DD   ：1ヶ月の日数
    !y,m,d： y 年 m 月 d 日
    !cnt  ：桁上がりの判定
    implicit none
    integer(16) MM, DD, y, m, d, cnt

    !入力
    read (*, *) MM, DD
    read (*, *) y, m, d

    !出力
    cnt = 0
    if (d >= DD) then
        d = 1
        m = m + 1
        cnt = 1
    end if
    if (m >= MM) then
        m = 1
        y = y + 1
        cnt = 1
    end if
    if (cnt == 0) then
        write (*, '(i0,1x,i0,1x,i0)') y, m, d + 1
    else
        write (*, '(i0,1x,i0,1x,i0)') y, m, d
    end if

end program abc331a
