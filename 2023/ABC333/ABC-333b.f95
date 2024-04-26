program abc333b
    !S1S2    ：S1と点S2を結ぶ線分
    !T1T2    ：T1と点T2を結ぶ線分
    !nagasa_a：線分S1S2の長さ(1:短い,0：長い)
    !nagasa_b：線分T1T2の長さ(1:短い,0：長い)
    implicit none
    character(2) S1S2, T1T2
    integer nagasa_a, nagasa_b

    !入力
    read (*, *) S1S2
    read (*, *) T1T2

    !線分の長さチェック
    call check_nagasa(S1S2, nagasa_a)
    call check_nagasa(T1T2, nagasa_b)

    !結果の出力
    if (nagasa_a == nagasa_b) then
        write (*, '(a)') 'Yes'
    else
        write (*, '(a)') 'No'
    end if
contains
    subroutine check_nagasa(line, nagasa)
        !line  ：線分
        !nagasa：線分の長さ(1:短い,0：長い)
        character(2) line
        integer nagasa

        !線分の長さ判定
        nagasa = 0
        select case (line)
        case ("AB")
            nagasa = 1
        case ("BC")
            nagasa = 1
        case ("CD")
            nagasa = 1
        case ("DE")
            nagasa = 1
        case ("EA")
            nagasa = 1
        case ("AE")
            nagasa = 1
        case ("ED")
            nagasa = 1
        case ("DC")
            nagasa = 1
        case ("CB")
            nagasa = 1
        case ("BA")
            nagasa = 1
        end select
    end subroutine check_nagasa
end program abc333b
