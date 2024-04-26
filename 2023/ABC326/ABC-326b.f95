program abc326b
    !N    ：入力した整数
    !x    ：Nを桁ごとに分割した数
    !check：百の位の数と十の位の数の積
    implicit none
    integer N, i
    integer x(3), check

    !入力
    read (*, *) N

    !判定
    do i = N, 919
        x(3) = i/100
        x(2) = i/10 - x(3)*10
        x(1) = i - x(3)*100 - x(2)*10
        check = x(3)*x(2)
        !write (*, *) i, x(3), x(2), x(1)
        if (check == x(1)) then
            write (*, '(i0)') i
            stop
        end if
    end do
end program abc326b
