program abc319b
    implicit none
    integer N, i, j

    read (*, *) N

    write (*, '(i0)', advance='no') 1
    do i = 1, N - 1
        do j = 1, 10
            if (j == 10) then
                !write (*, '(i0,1x,a)', advance='no') i, '-' !改行なし
                write (*, '(a)', advance='no') '-'
                exit
            end if
            if (mod(N, j) == 0) then
                if (mod(i, N/j) == 0) then
                    !write (*, '(i0,1x,i0)', advance='no') i, j !改行なし
                    write (*, '(i0)', advance='no') j
                    exit
                end if
            else
                cycle
            end if
        end do
    end do
    write (*, '(i0)', advance='no') 1
end program abc319b
