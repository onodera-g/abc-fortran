program ABC301a
    implicit none
    integer N, cnt_T, cnt_A, i, cnt
    character(:), allocatable :: S, T
    !入力
    READ (*, *) S, T

    !勝ち数集計
    cnt_A = 0
    cnt_T = 0
    do i = 1, N
        if (S(i:i) == "A") then
            cnt_A = cnt_A + 1
        else
            cnt_T = cnt_T + 1
        end if
    end do

    !結果判定
    if (cnt_A > cnt_T) then !A勝ち
        write (*, "(a)") 'A'
        stop
    else if (cnt_T > cnt_A) then !T勝ち
        write (*, "(a)") 'T'
        stop
    else if (cnt_T == cnt_A) then !勝ち数同じ
        cnt = cnt_A
        cnt_A = 0
        cnt_T = 0
        do i = 1, N
            if (S(i:i) == "A") then
                cnt_A = cnt_A + 1
            else
                cnt_T = cnt_T + 1
            end if
            if (cnt_A == cnt) then
                write (*, "(a)") 'A'
                stop
            else if (cnt_T == cnt) then
                write (*, "(a)") 'T'
                stop
            end if
        end do
    end if
end program
