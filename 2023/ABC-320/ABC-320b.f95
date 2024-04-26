program abc320b
    implicit none
    integer i, j, length, cnt
    character(100) S

    !読み込み
    read (*, *) S

    !回文の長さ測定
    length = 1
    do i = 1, len_trim(S)
        cnt = 1
        if (S(i:i) == '') exit
        do j = 1, len(S)
            if (i - j < 1 .or. i + j > len(S)) exit
            if (S(i - j:i - j) /= S(i + j:i + j)) exit
            cnt = cnt + 2
        end do
        length = max(length, cnt)

        if (S(i:i) == S(i + 1:i + 1)) then
            cnt = 2
        else
            cycle
        end if
        do j = 1, len(S)
            if (i - j < 1 .or. i + j + 1 > len(S)) exit
            if (S(i - j:i - j) /= S(i + j + 1:i + j + 1)) exit
            cnt = cnt + 2
        end do
        length = max(length, cnt)
    end do

    !結果の出力
    write (*, '(i0)') length

end program abc320b
