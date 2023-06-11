program ABC305b
    implicit none
    integer i, j
    integer Ans
    character(1) p, q, kari
    integer kyori(6)
    data kyori/3, 1, 4, 1, 5, 9/
    !入力
    read (*, *) p, q
    !昇順に並び替え
    if (iachar(p) > iachar(q)) then
        kari = p
        p = q
        q = kari
    end if

    !距離の計算
    Ans = 0
    do i = 65, 71
        if (iachar(p) == i) then
            do j = i + 1, 71
                if (iachar(q) == j) then
                    Ans = Ans + kyori(j - 65)
                    write (*, *) Ans
                    stop
                else
                    Ans = Ans + kyori(j - 65)
                end if
            end do
        end if
    end do
end
