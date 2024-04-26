program abc322c
    implicit none
    integer(8) N, M, check, i, ans
    integer(8), allocatable::A(:)

    !読み込み
    read (*, *) N, M
    allocate (A(M))
    read (*, *) A(1:M)

    !次の花火の日検索
    check = 1
    do i = 1, N
        call search_ff(i, check, A)
        ans = A(check) - i
        write (*, *) ans !, check
    end do
contains
    subroutine search_ff(i, check, A)
        implicit none
        integer(8) i, check, j
        integer(8) A(:)
        do j = check, size(A)
            if (i <= A(j)) then
                check = j
                return
            end if
        end do
    end subroutine search_ff
end program abc322c
