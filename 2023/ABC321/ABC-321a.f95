program abc321a
    implicit none
    integer N, x(6), i

    !読み込み
    read (*, *) N

    !各桁の計算
    x(6) = 0
    x(5) = N/10000
    x(4) = N/1000 - x(5)*10
    x(3) = N/100 - x(5)*100 - x(4)*10
    x(2) = N/10 - x(5)*1000 - x(4)*100 - x(3)*10
    x(1) = N/1 - x(5)*10000 - x(4)*1000 - x(3)*100 - x(2)*10

    !321-like Numberの判定
    do i = 5, 2, -1
        if (x(i) == 0 .and. x(i + 1) == 0) cycle
        if (x(i) <= x(i - 1)) then
            write (*, '(a)') 'No'
            stop
        end if
    end do
    write (*, '(a)') 'Yes'
end program abc321a
