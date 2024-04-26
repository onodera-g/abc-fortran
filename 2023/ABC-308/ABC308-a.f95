program ABC308a
    implicit none
    integer(8) i, j, tmp
    integer(8) S(8)

    !初期化
    i = 0; j = 1; tmp = 0

    !入力
    read (*, *) S(:)

    !処理
    do i = 1, 8
        !S(i)<S(i+1)
        if (i < 8) then
            if (S(i) > S(i + 1)) then
                write (*, "(a)") 'No'
                stop
            end if
        end if
        if (S(i) > S(i + 1)) then
            write (*, "(a)") 'No'
            stop
        end if
        !S(i)が100以上 ,675以下
        if (S(i) < 100 .or. S(i) > 675) then
            write (*, "(a)") 'No'
            stop
        end if
        !S(i)が25の倍数
        if (mod(S(i), 25) /= 0) then
            write (*, "(a)") 'No'
            stop
        end if
    end do

    !結果の出力
    write (*, "(a)") 'Yes'
end
