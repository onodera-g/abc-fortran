program ABC311a
    implicit none
    integer i
    integer N, a, b, c
    character(:), allocatable :: S

    !初期化
    a = 0; b = 0; c = 0
    N = 0; i = 0

    !入力
    read (*, *) N
    allocate (character(N)::S)
    read (*, *) S

    !A,B,Cのカウント
    do i = 1, N
        if (S(i:i) == 'A') a = 1
        if (S(i:i) == 'B') b = 1
        if (S(i:i) == 'C') c = 1
        if (a == 1 .and. b == 1 .and. c == 1) then
            write (*, *) i
            stop
        end if
    end do
end program abc311a
