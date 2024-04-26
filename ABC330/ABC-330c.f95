program abc330c
    !D    ：正整数D
    !x    ：非負整数x
    !y    ：非負整数y
    implicit none
    integer(8) x, y, ans, D

    !入力
    read (*, *) D

    !最小値の検索
    ans = D
    y = 2*10**6
    do x = 0, 2*10**6
        do
            if (y > 0 .and. x*x + y*y > D) then
                y = y - 1
            else
                exit
            end if
        end do
        ans = min(ans, abs(x*x + y*y - D))
        ans = min(ans, abs(x*x + (y + 1)*(y + 1) - D))
    end do

    !出力
    write (*, *) ans
end program abc330c
