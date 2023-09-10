program abc319c
    implicit none
    integer i, c(3, 3), c2(9), tmp(9), diff, c1(9), tmp2(9)
    real(16) kakuritu, cnt, ans

    do i = 1, 3
        read (*, *) C(i, :)
    end do

    kakuritu = 9*8*7
    c2(1:3) = C(1, 1:3); c2(4:6) = C(2, 1:3); c2(7:9) = C(3, 1:3)
    do i = 1, 9
        c1(i) = i
    end do
    call margesort(c2, c1, tmp, tmp2, 9, 1, 9)

    cnt = 0
    do i = 1, 8
        diff = c2(i + 1) - c2(i)
        if (diff == 0) then
            if (abs(c1(i + 1) - c1(i)) == 3) then
                write (*, *) c2(i + 1), c2(i)
                cnt = cnt + 14
                cycle
            end if
            if (abs(c1(i + 1) - c1(i)) == 1) then
                if (c1(i) == 3 .and. c1(i + 1) == 4) cycle
                if (c1(i) == 4 .and. c1(i + 1) == 3) cycle
                if (c1(i) == 6 .and. c1(i + 1) == 7) cycle
                if (c1(i) == 7 .and. c1(i + 1) == 6) cycle
                write (*, *) c2(i + 1), c2(i)
                cnt = cnt + 14
            end if
        end if
    end do
    Ans = (kakuritu - cnt)/kakuritu
    write (*, *) Ans

contains
    recursive subroutine margesort(x, y, tmp, tmp2, N, left, right)
        integer left, right, mid
        integer N
        integer x(N), tmp(N), y(N), tmp2(N)
        integer i, j, k

        !これ以上2分かつできないならretrun
        if (left >= right) return

        !分割できるだけ分割する
        mid = (left + right)/2
        call margesort(x, y, tmp, tmp2, N, left, mid)
        call margesort(x, y, tmp, tmp2, N, mid + 1, right)

        !並び替えの下準備としてtmpに配列をコピー
        j = 0
        tmp(left:mid) = x(left:mid)
        tmp2(left:mid) = y(left:mid)
        do i = mid + 1, right
            tmp(i) = x(right - j)
            tmp2(i) = y(right - j)
            j = j + 1
        end do

        !大小比較して小さい順に入れていく
        i = left
        j = right
        !write (*, '(3x,*(f13.101x),a)', advance='no') x(left:right)
        !write (*, '(a)', advance='no') '>>'
        do k = left, right
            if (tmp(i) > tmp(j)) then
                x(k) = tmp(i)
                y(k) = tmp2(i)
                i = i + 1
            else if (tmp(i) == tmp(j) .and. tmp2(i) < tmp2(j)) then
                x(k) = tmp(i)
                y(k) = tmp2(i)
                i = i + 1
            else
                x(k) = tmp(j)
                y(k) = tmp2(j)
                j = j - 1
            end if
        end do
        !write (*, '(3x,*(f13.10,1x))') x(left:right)
    end subroutine margesort
end program
