module ABC319c_mod
    implicit none
    ! グローバル変数
    integer arr(9), order(9)
    real(16) ans, cnt
end module

program abc319c
    use ABC319c_mod
    implicit none
    integer i
    logical check, check2
    integer C(3, 3)

    !入力
    do i = 1, 3
        read (*, *) C(i, :)
    end do

    !
    arr(1:3) = C(1, 1:3); arr(4:6) = C(2, 1:3); arr(7:9) = C(3, 1:3)
    order = (/1, 2, 3, 4, 5, 6, 7, 8, 9/)

    !がっかりする回数の測定
    check = .false.; cnt = 0d0
    do
        !がっかりするかを判定
        check2 = .false.
        call check_gakkari(1, 2, 3, check2); call check_gakkari(4, 5, 6, check2); call check_gakkari(7, 8, 9, check2)
        call check_gakkari(1, 4, 7, check2); call check_gakkari(2, 5, 8, check2); call check_gakkari(3, 6, 9, check2)
        call check_gakkari(1, 5, 9, check2); call check_gakkari(3, 5, 7, check2)

        !次の組み合わせを計算
        call next_permutation(order, 9, check)
        if (check .eqv. .true.) exit
    end do

    !結果の出力
    ans = 1d0 - cnt/362880d0
    write (*, *) ans

contains
    subroutine next_permutation(order, n, check)
        integer n, k, kari, order(n), tmp(n), i, l
        logical check

        do k = n - 1, 1, -1
            if (order(k) < order(k + 1)) exit
            if (k == 1) then
                check = .true.
                exit
            end if
        end do

        do l = n, k + 1, -1
            if (order(k) < order(l)) exit
        end do

        kari = order(k); order(k) = order(l); order(l) = kari

        if (k + 1 /= n) then
            do i = 1, k
                tmp(i) = order(i)
            end do
            do i = 0, n - (k + 1)
                tmp(n - i) = order(k + 1 + i)
            end do
            order = tmp
        end if
    end subroutine

    subroutine check_gakkari(o1, o2, o3, check2)
        integer o1, o2, o3
        integer x(3), y(3), tmp(3), tmp2(3)
        logical check2
        tmp = 0; tmp2 = 0

        if (check2 .eqv. .true.) return

        x(1) = order(o1); x(2) = order(o2); x(3) = order(o3)
        y(1) = arr(o1); y(2) = arr(o2); y(3) = arr(o3)
        call margesort(x, y, tmp, tmp2, 3, 1, 3)

        if (y(1) == y(2)) then
            cnt = cnt + 1d0
            check2 = .true.
        end if
    end subroutine

    recursive subroutine margesort(x, y, tmp, tmp2, N, left, right)
        integer left, right, mid
        integer i, j, k, N
        integer x(N), tmp(N), y(N), tmp2(N)

        if (left >= right) return

        mid = (left + right)/2
        call margesort(x, y, tmp, tmp2, N, left, mid)
        call margesort(x, y, tmp, tmp2, N, mid + 1, right)

        j = 0
        tmp(left:mid) = x(left:mid)
        tmp2(left:mid) = y(left:mid)
        do i = mid + 1, right
            tmp(i) = x(right - j)
            tmp2(i) = y(right - j)
            j = j + 1
        end do

        i = left
        j = right
        do k = left, right
            if (tmp(i) < tmp(j)) then
                x(k) = tmp(i)
                y(k) = tmp2(i)
                i = i + 1
            else if (tmp(i) == tmp(j)) then
                x(k) = tmp(i)
                y(k) = tmp2(i)
                i = i + 1
            else
                x(k) = tmp(j)
                y(k) = tmp2(j)
                j = j - 1
            end if
        end do
    end subroutine margesort
end
