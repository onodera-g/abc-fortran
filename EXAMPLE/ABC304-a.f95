program ABC304a
    implicit none
    integer i, j, k
    integer N, kari_i
    character(10) kari_c
    character(10), allocatable :: Name(:)
    integer, allocatable:: zyunban(:)
    !入力
    READ (*, *) N
    allocate (Name(N), zyunban(N))
    do i = 1, N
        read (*, *) Name(i), zyunban(i)
    end do

    do i = 1, N - 1
        do j = i + 1, N
            if (zyunban(i) > zyunban(j)) then
                kari_i = zyunban(i)
                zyunban(i) = zyunban(j)
                zyunban(j) = kari_i
                !文字
                kari_c = Name(i)
                Name(i) = Name(j)
                Name(j) = kari_c
            end if
        end do
    end do
    do i = 1, N
        write (*, *) Name(i)
    end do
end
