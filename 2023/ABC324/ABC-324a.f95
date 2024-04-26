program abc324a
    ! A(N)：整数A1,A2,...AN
    ! N   ：整数の個数
    implicit none
    integer N, i
    integer, allocatable::A(:)

    !入力
    read (*, *) N
    allocate (A(N))
    read (*, *) A

    !判定
    do i = 1, N - 1
        if (A(i) == A(i + 1)) then
            cycle
        else
            write (*, '(a)') 'No'
            stop
        end if
    end do
    write (*, '(a)') 'Yes'
end program abc324a
