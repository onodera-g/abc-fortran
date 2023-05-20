program ABC301b
    implicit none
    integer N, i, check, tuika, j, l, cnt
    integer, allocatable :: A(:), Ans(:)
    Ans = [integer ::]

    !入力
    READ (*, *) N
    allocate (A(N))
    READ (*, *) A

    !最初から全部絶対値１かチェック
    check = 0
    do i = 1, N - 1
        if (abs(A(i) - A(i + 1)) /= 1) then
            check = 1
            exit
        end if
    end do

    !最初から全部絶対値１なら終了
    if (check == 0) then
        write (*, '(*(i0, 1x))') A
        stop
    end if

    !数列の操作
    do i = 1, N - 1
        Ans = [Ans, A(i)]
        if (abs(A(i) - A(i + 1)) /= 1) then
            tuika = abs(A(i + 1) - A(i))
            l = 0
            if (A(i) < A(i + 1)) then
                do j = 1, tuika - 1
                    l = l + 1
                    Ans = [Ans, A(i) + l]
                end do
            elseif (A(i) > A(i + 1)) then
                do j = 1, tuika - 1
                    l = l + 1
                    Ans = [Ans, A(i) - l]
                end do
            end if
        end if
        !write (*, *) Ans
    end do
    Ans = [Ans, A(N)]
    write (*, '(*(i0, 1x))') Ans
end program
