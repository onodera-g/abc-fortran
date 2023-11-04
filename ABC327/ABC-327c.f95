program abc327c
    !A        ：9x9のマス目
    !A_yoko   ：Aのi行目(長さ9)
    !A_tate   ：Aのi列目(長さ9)
    !A_box    ：Aから3x3を抽出し、一列に並べた形
    !check_arr：条件1~3の判定用(1~9が連番で格納されてる)
    integer(16) A(9, 9)
    integer(16) A_yoko(9, 9), A_tate(9, 9), A_box(9), check_arr(9)
    integer(16) i, n

    !入力
    do i = 1, 9
        read (*, *) A(i, :)
    end do
    read (*, *) A(i, :)
    n = 9
    data check_arr/1, 2, 3, 4, 5, 6, 7, 8, 9/

    !条件1
    A_yoko = A
    do i = 1, 9
        call margesort(A_yoko(i, :), n)
        if (any(check_arr /= A_yoko(i, 1:9))) then
            write (*, '(a)') 'No'
            stop
        end if
    end do

    !条件2
    A_tate = A
    do i = 1, 9
        call margesort(A_tate(:, i), n)
        if (any(check_arr /= A_tate(1:9, i))) then
            write (*, '(a)') 'No'
            stop
        end if
    end do

    !条件3
    do j = 1, 9, 3
        do i = 1, 9, 3
            A_box(1:3) = A(j, i:i + 2)
            A_box(4:6) = A(j + 1, i:i + 2)
            A_box(7:9) = A(j + 2, i:i + 2)
            call margesort(A_box, n)
            if (any(check_arr /= A_box)) then
                write (*, '(a)') 'No'
                stop
            end if
        end do
    end do

    !結果の出力
    write (*, '(a)') 'Yes'
contains
    subroutine margesort(x, n)
        integer(16) N
        integer(16) x(N), tmp(N)
        integer(16) start, end
        start = 1; end = N
        call loop_margesort(x, tmp, N, start, end)
    end subroutine
    recursive subroutine loop_margesort(x, tmp, N, left, right)
        integer(16) left, right, mid
        integer(16) N
        integer(16) x(N), tmp(N)
        integer(16) i, j, k

        if (left >= right) return

        mid = (left + right)/2
        call loop_margesort(x, tmp, N, left, mid)
        call loop_margesort(x, tmp, N, mid + 1, right)

        j = 0
        tmp(left:mid) = x(left:mid)
        do i = mid + 1, right
            tmp(i) = x(right - j)
            j = j + 1
        end do

        i = left
        j = right
        do k = left, right
            if (tmp(i) < tmp(j)) then
                x(k) = tmp(i)
                i = i + 1
            else if (tmp(i) == tmp(j)) then
                x(k) = tmp(i)
                i = i + 1
            else
                x(k) = tmp(j)
                j = j - 1
            end if
        end do
    end subroutine loop_margesort
end program abc327c
