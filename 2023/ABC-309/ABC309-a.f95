program ABC309a
    implicit none
    integer(8) A, B
    !初期化
    A = 0; B = 0

    !入力
    read (*, *) A, B

    !処理
    if (A /= 3 .and. A /= 6) then !3と6の時は盤面の隅なので対象外
        if (A + 1 == B) then
            write (*, "(a)") 'Yes'
            stop
        end if
    end if

    !結果の出力
    write (*, "(a)") 'No'
end
