module ABC311c_global
    implicit none
    ! グローバル変数
    integer, allocatable:: ans(:)
end module

program ABC311c
    use ABC311c_global
    implicit none
    integer(4) i, j
    integer(4) N
    integer(4), allocatable::A(:), check(:, :)

    !初期化
    i = 0; j = 0; check = 0

    !入力
    read (*, *) N
    allocate (A(N), check(2, N), ans(N))
    read (*, *) A(:)

    !ループの調査
    do i = 1, N
        call LOOP_CHECK(A, check, i, j, N)
    end do

contains
!SUB:ループしてるかチェック
    recursive subroutine LOOP_CHECK(A, check, i, j, N)
        integer(4) i, j, k, N
        integer(4) A(:), check(:, :)
        if (check(1, i) == 2) then
            write (*, '(i0)') j
            do k = 1, j
                write (*, '(1x,i0)', advance='no') A(Ans(k)) ! 改行しない
            end do
            stop
            check = 0
            return
        else
            if (check(1, i) == 1) then
                j = j + 1
                Ans(j) = i
                check(2, i) = j
            end if
            check(1, i) = check(1, i) + 1
            ! write (*, *) A(i), check(1, i), j
            i = A(i)
            call LOOP_CHECK(A, check, i, j, N)
        end if
    end subroutine
end

