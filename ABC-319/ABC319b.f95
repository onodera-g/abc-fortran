program abc319b
    implicit none
    integer N, i, j

    !読み込み
    read (*, *) N

    !処理
    write (*, '(i0)', advance='no') 1
    do i = 1, N - 1
        do j = 1, 10
            if (j == 10) then
                write (*, '(a)', advance='no') '-'
                exit
            end if
            if (mod(N, j) == 0) then
                if (mod(i, N/j) == 0) then
                    write (*, '(i0)', advance='no') j
                    exit
                end if
            else
                cycle
            end if
        end do
    end do

    !結果の出力
    write (*, '(i0)', advance='no') 1
end program abc319b
