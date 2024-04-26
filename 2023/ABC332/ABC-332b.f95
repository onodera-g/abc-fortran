program abc332b
    !G        ：グラスの容量
    !M        ：マグカップの容量
    !current_G：現在グラスに入っている水の量
    !current_M：現在マグカップ入っている水の量
    !K        ：繰り返し回数
    !cnt      ：現在の繰り返し回数
    implicit none
    integer G, M
    integer current_G, current_M
    integer K
    integer cnt
    !入力
    read (*, *) K, G, M
    current_G = 0; current_M = 0; cnt = 0

    !水の移し替え
    do
        !グラスが満杯なら水を捨てる
        if (current_G == G) then
            current_G = 0
            cnt = cnt + 1
            if (cnt >= K) exit
        end if

        ! マグカップが空なら水を入れる
        if (current_M == 0) then
            current_M = M
            cnt = cnt + 1
            if (cnt >= K) exit
        end if

        !マグカップからグラスに水を移す
        select case (current_G)
        case (0) !グラスが空
            if (current_M > G) then !マグカップの残量がグラスの残量より多い
                current_M = current_M - G
                current_G = G
            else !マグカップの残量がグラスの残量より少ない
                current_G = current_M
                current_M = 0
            end if
        case (1:) !グラスが空でない
            if (current_M > G) then !マグカップの残量がグラスの残量より多い
                current_M = current_M - (G - current_G)
                current_G = G
            else !マグカップの残量がグラスの残量より少ない
                current_M = 0
                current_G = current_G + current_M
            end if
        end select
        cnt = cnt + 1
        if (cnt >= K) exit
    end do
    write (*, '(i0,1x,i0)') current_G, current_M
end program abc332b
