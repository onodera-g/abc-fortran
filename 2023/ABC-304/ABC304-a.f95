program ABC304a
    implicit none
    integer(8) i
    integer(8) N, min_num, min_i
    character(10), allocatable :: Name(:)
    integer, allocatable:: num(:)
    !入力
    READ (*, *) N
    allocate (Name(N), num(N))

    !最年少の検索
    min_num = 1*10**9
    do i = 1, N
        read (*, *) Name(i), num(i)
        if (num(i) < min_num) then
            min_num = num(i)
            min_i = i
        end if
    end do

    !結果の表示
    do i = min_i, N
        write (*, "(a)") Name(i)
    end do
    do i = 1, min_i - 1
        write (*, "(a)") Name(i)
    end do
end
