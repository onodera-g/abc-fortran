program abc327b
    !B：整数
    !ans：a**a
    implicit none
    integer(16) B, ans, a

    !入力
    read (*, *) B

    !判定
    do a = 1, B
        ans = a**a
        if (ans == B) then
            write (*, '(i0)') a
            stop
        elseif (ans > B) then
            exit
        end if

    end do
    write (*, '(i0)') - 1
end program abc327b
