program abc322b
    implicit none
    integer N, M
    character(:), allocatable :: S, T
    integer check1, check2

    check1 = 0; check2 = 0

    !入力
    read (*, *) N, M
    allocate (character(N)::S)
    allocate (character(M)::T)
    read (*, *) S, T

    !接頭辞判定
    if (T(1:N) == S(1:N)) check1 = 1
    if (T(M - N + 1:M) == S(1:N)) check2 = 1

    if (check1 == 1) then
        if (check2 == 1) then
            write (*, *) 0
        else
            write (*, *) 1
        end if
    else if (check2 == 1) then
        write (*, *) 2
    else
        write (*, *) 3
    end if
end program abc322b
