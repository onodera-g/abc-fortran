program abc328b
    !N  ：１年の月数
    !D  ：各月の日数
    !m3 ：各月の1,2,3桁目の数
    !d3 ：各日の1,2,3桁目の数
    !ans：月日がゾロ目になる合計回数
    implicit none
    integer(16) N, i, j, ans, d3(3), m3(3)
    integer(16), allocatable :: D(:)

    !入力
    read (*, *) N
    allocate (D(N))
    read (*, *) D

    !判定
    ans = 0
    do i = 1, N
        if (i == 100) cycle
        do j = 1, D(i)
            if (j == 100) cycle
            if (i < 10) then
                if (j < 10 .and. j == i) then
                    ans = ans + 1
                    !write (*, *) i, j
                else
                    d3(3) = j/100
                    d3(2) = j/10 - d3(3)*10
                    d3(1) = j - d3(3)*100 - d3(2)*10
                    if (d3(1) == i .and. d3(2) == i) then
                        ans = ans + 1
                        !write (*, *) i, j
                    end if
                end if
            else
                m3(3) = i/100
                m3(2) = i/10 - m3(3)*10
                m3(1) = i - m3(3)*100 - m3(2)*10
                if (m3(1) == m3(2)) then
                    if (j < 10 .and. m3(1) == j) then
                        ans = ans + 1
                        !write (*, *) i, j
                    else
                        d3(3) = j/100
                        d3(2) = j/10 - d3(3)*10
                        d3(1) = j - d3(3)*100 - d3(2)*10
                        if (d3(1) == m3(1) .and. d3(2) == m3(1)) ans = ans + 1
                    end if
                end if
            end if
        end do
    end do

    !出力
    write (*, '(i0)') ans
end program abc328b
