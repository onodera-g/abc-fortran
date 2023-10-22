module global_abc324c
    !cnt   ：SとTが一致している数
    !tf    ：SとTが一致するかの判定用(1なら一致,0なら不一致)
    !ans(N)：SとTが一致するかを記録(1なら一致,0なら不一致)
    implicit none
    integer(8) cnt, tf
    integer(8), allocatable ::ans(:)
end module
program abc324c
    !T    ：送信した文字列
    !N    ：受信した文字列の数
    !S(N) ：受信した文字列
    !c    ：入力用文字列(1文字分)
    !tl,sl：TとSの文字の長さ
    use global_abc324c
    implicit none
    character(500000) T, S
    character(1) c
    integer(16) N
    integer(16) i, j, tl, sl

    read (*, *) N, T
    allocate (ans(N))
    cnt = 0; ans = 0; i = 1
    tl = len_trim(T)
    do j = 1, N
        !入力
        S(1:i) = ' '
        i = 1
        do
            read (*, '(a1)', advance="no") c
            if (c == " ") exit
            S(i:i) = c
            i = i + 1
        end do

        !T = T'一致判定
        sl = i - 1
        if (abs(sl - tl) > 1) cycle
        select case (sl - tl)
        case (1)
            call check2(S(1:sl), T(1:tl), sl, tl)
        case (-1)
            call check3(S(1:sl), T(1:tl), sl, tl)
        case (0)
            call check4(S(1:sl), T(1:tl), sl, tl)
        end select
        if (tf == 1) then
            cnt = cnt + 1
            ans(j) = 1
        end if
    end do

    !結果の出力
    write (*, '(i0)') cnt
    do i = 1, N
        if (ans(i) == 1) write (*, '(i0,1x)', advance='no') i
    end do
contains
    !条件2：Sに１文字挿入されている
    subroutine check2(S, T, sl, tl)
        !T    ：送信した文字列
        !S    ：受信した文字列
        !miss：文字が不一致の回数
        !tl,sl：TとSの文字の長さ
        integer(16) sl, tl, i, j, miss
        character(sl + 1) S
        character(tl + 1) T
        i = 1; j = 1; tf = 0; miss = 0
        do
            if (T(i:i) == S(j:j)) then
                i = i + 1
                j = j + 1
            else
                j = j + 1
                miss = miss + 1
                if (miss > 1) return
            end if
            if (i == tl + 1 .or. j == sl + 1) exit
        end do
        tf = 1
    end subroutine check2

    !条件3：Sが１文字削除されている
    subroutine check3(S, T, sl, tl)
        !T    ：送信した文字列
        !S    ：受信した文字列
        !miss：文字が不一致の回数
        !tl,sl：TとSの文字の長さ
        integer(16) sl, tl, i, j, miss
        character(sl + 1) S
        character(tl + 1) T
        i = 1; j = 1; tf = 0; miss = 0
        do
            if (T(i:i) == S(j:j)) then
                i = i + 1
                j = j + 1
            else
                i = i + 1
                miss = miss + 1
                if (miss > 1) return
            end if
            if (i == tl + 1 .or. j == sl + 1) exit
        end do
        tf = 1
    end subroutine check3

    !条件4：SとTが完全一致
    subroutine check4(S, T, sl, tl)
        !T    ：送信した文字列
        !S    ：受信した文字列
        !miss：文字が不一致の回数
        !tl,sl：TとSの文字の長さ
        integer(16) sl, tl, i, j, miss
        character(sl + 1) S
        character(tl + 1) T
        i = 1; j = 1; tf = 0; miss = 0
        do i = 1, tl
            if (T(i:i) /= S(i:i)) then
                miss = miss + 1
                if (miss > 1) return
            else
            end if
        end do
        tf = 1
    end subroutine check4
end
