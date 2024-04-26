program abc329a
    !S:文字列
    implicit none
    integer i
    character(100) S

    !入力
    read (*, *) S

    !結果の出力
    do i = 1, 100
        if (S(i + 1:i + 1) == '') then
            write (*, '(a1)') S(i:i)

            stop
        else
            write (*, '(a1,1x)', advance='no') S(i:i)
        end if
    end do
end program abc329a
