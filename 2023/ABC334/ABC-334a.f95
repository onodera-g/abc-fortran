program abc333a
    !B:バットの値段
    !G:グローブの値段
    implicit none
    integer B, G

    !入力
    read (*, *) B, G

    !出力
    if (B > G) then
        write (*, *) "Bat"
    else
        write (*, *) "Glove"
    end if
end program abc333a
