program abc333a
    !N：整数N
    implicit none
    integer N, i

    !入力
    read (*, *) N

    !出力
    do i = 1, N
        write (*, '(i0)', advance='no') N
    end do
end program abc333a
