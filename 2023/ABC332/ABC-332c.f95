program abc332c
    !N    ：予定の日数
    !M    ：無地のTシャツの所有枚数
    !S    ：各予定の内容
    !tmp_M：必要な無地Tシャツの枚数(暫定)
    !tmp_T：必要なロゴTシャツの枚数(暫定)
    !max_T：必要なロゴTシャツの最大数
    implicit none
    integer i
    integer N, M
    character(:), allocatable ::S
    integer tmp_M, tmp_T, max_T

    !入力
    read (*, *) N, M
    allocate (character(N)::S)
    read (*, *) S
    max_T = 0; tmp_T = 0; tmp_M = 0

    !Tシャツの集計
    do i = 1, N
        select case (S(i:i))
        case ("0") !予定なし(洗濯)
            call wash_shirt(tmp_T, tmp_M, max_T, M)
            tmp_T = 0; tmp_M = 0; 
        case ("1") !食事の予定
            tmp_M = tmp_M + 1
        case ("2") !プログラミングのイベント
            tmp_T = tmp_T + 1
        end select
    end do
    call wash_shirt(tmp_T, tmp_M, max_T, M)

    !結果の出力
    write (*, *) max_T
contains
    subroutine wash_shirt(tmp_T, tmp_M, max_T, M)
        !tmp_M：0で囲まれた区間で必要な無地Tシャツの枚数(暫定)
        !tmp_T：0で囲まれた区間で必要なロゴTシャツの枚数(暫定)
        !max_T：必要なロゴTシャツの最大数
        !cnt_T：0で囲まれた区間で必要な無地Tシャツの枚数(確定)
        !cnt_M：0で囲まれた区間で必要な無地Tシャツの枚数(確定)
        !M    ：無地のTシャツの所有枚数
        integer tmp_M, tmp_T, max_T, M
        integer cnt_T, cnt_M
        cnt_T = 0; cnt_M = 0

        !無地Tの集計
        if (tmp_M > M) then !無地Tの所持数より必要数が多い
            cnt_T = tmp_M - M
            cnt_M = 0
        else !無地Tの所持数より必要数が少ない
            cnt_M = M - tmp_M
            cnt_T = 0
        end if

        !ロゴTの集計
        cnt_T = cnt_T + tmp_T

        !最大値の更新
        if (max_T < cnt_T) max_T = cnt_T
    end subroutine wash_shirt
end program abc332c

