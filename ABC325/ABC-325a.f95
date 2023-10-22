program abc325a
    ! S：名字
    ! T：名前
    implicit none
    character(10) S, T

    !入力
    read (*, *) S, T

    !出力
    write (*, ('(a,1x,a)')) trim(S), 'san'
end program abc325a
