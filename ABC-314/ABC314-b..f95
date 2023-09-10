program ABC314b
    implicit none
    integer i, j
    integer N, X, min_C, cnt
    integer, allocatable :: A(:, :), check_hit(:), C(:)

    !初期化
    cnt = 0; check_hit = 0; min_C = 99

    !入力1
    read (*, *) N
    allocate (A(N, 99), check_hit(N), c(N))
    A = 99
    do i = 1, N
        read (*, *) C(i)
        read (*, *) (A(i, j), j=1, C(i))
    end do
    read (*, *) X

    !Xを含む最小のCを調べる
    do i = 1, N
        if (any(A(i, :) == X)) then
            check_hit(i) = 1
            min_C = min(min_C, C(i))
        end if
    end do

    !結果の出力
    if (min_C == 99) then
        write (*, '(i0)') 0
    else
        do i = 1, N
            if (C(i) == min_C .and. check_hit(i) == 1) cnt = cnt + 1
        end do
        write (*, '(i0)') cnt
        do i = 1, N
            if (C(i) == min_C .and. check_hit(i) == 1) write (*, '(i0,1x)', ADVANCE='NO') i
        end do
    end if
end program abc314b
