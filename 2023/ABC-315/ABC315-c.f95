program ABC315c
    implicit none
    integer(16) i
    integer(16) N, max_i, max_S, max_F, azi, max_azi
    integer(16), allocatable ::F(:), S(:)
    !初期化
    max_i = 0; max_S = 0; max_F = 0

    !入力
    read (*, *) N
    allocate (F(N), S(N))
    do i = 1, N
        read (*, *) F(i), S(i)
        if (max_S < S(i)) then
            max_i = i
            max_S = S(i)
            max_F = F(i)
        end if
    end do

    !最大値の検索
    do i = 1, N
        if (i == max_i) cycle
        if (max_F == F(i)) then
            azi = max_S + S(i)/2
        else
            azi = max_S + S(i)
        end if
        max_azi = max(max_azi, azi)
    end do

    !結果の出力
    write (*, '(i0)') max_azi
end program abc315c
