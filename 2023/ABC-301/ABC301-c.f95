program ABC301c
    implicit none
    integer s_alphabet(122), t_alphabet(122), i, s_length, t_a, s_a, diff
    character(200000):: S, T

    !S,T の入力
    read (*, *) S, T

    !@~zのそれぞれの個数を数える
    s_length = len_trim(S)
    s_alphabet = 0
    t_alphabet = 0
    do i = 1, s_length
        s_alphabet(iachar(S(i:i))) = s_alphabet(iachar(S(i:i))) + 1
        t_alphabet(iachar(T(i:i))) = t_alphabet(iachar(T(i:i))) + 1
    end do

    !イカサマで勝てるか判定
    t_a = t_alphabet(iachar("@"))
    s_a = t_alphabet(iachar("@"))
    do i = 97, 122
        diff = s_alphabet(i) - t_alphabet(i)
        !個数一致の場合
        if (diff == 0) cycle !イカサマorそのまま
        !個数不一致の場合
        select case (i)
            !置き換えできる文字の場合
        case (ichar("a"), ichar("t"), ichar("c"), ichar("o"), ichar("d"), ichar("e"), ichar("r"))
            !TとSのどちらが不足しているか
            if (diff > 0) then !tが不足
                !置き換え可能個数(@の総数)を超えないか
                t_a = t_a - diff
                if (t_a < 0) then !置き換え可能残数0
                    write (*, '(a)') "No"
                    stop
                end if
            else !sが不足
                !置き換え可能個数(@の総数)を超えないか
                s_a = s_a - diff
                if (s_a < 0) then !置き換え可能残数0
                    write (*, '(a)') "No"
                    stop
                end if
            end if
            !置き換え出来ない文字の場合
        case default
            write (*, '(a)') "No"
            stop
        end select
    end do

    write (*, '(a)') "Yes"
end
