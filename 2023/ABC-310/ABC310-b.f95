program ABC310b
    implicit none
    integer(16) i, j, k, ii, jj, s, kk
    integer(16) N, M
    integer(16), allocatable :: P(:), C(:), f(:, :), cnt(:)
    !初期化
    i = 0; j = 0; N = 0; M = 0

    !入力
    read (*, *) N, M
    allocate (P(N), C(N), f(N, M), cnt(N))
    p = 0
    do i = 1, N
        read (*, *) P(i), C(i), (f(i, j), j=1, C(i))
        cnt(i) = i
    end do

    !値段を昇順に並び替え
    s = 1
    call quicksort(P, cnt, s, N, N)

    !
    do i = 1, N
        ii = cnt(i)
        do j = 1, N
            jj = cnt(j)
            !write (*, '(i0,1x,i0)') P(i), P(j)
            !P(i)の方が安い
            if (P(i) < P(j)) then
                !write (*, '(4x,a4,i0,a3,i0)') 'P   ', P(i), ' < ', P(j)
                exit
            else
                !Cの数が少ない
                if (C(ii) >= C(jj)) then
                    !write (*, '(4x,a4,i0,a3,*(i0))') 'C   ', C(ii), ' > ', C(jj)
                    cycle
                else
                    !P(j)が安く、かつC(j)が多い
                    yoko: do k = 1, C(ii)
                        !一致する機能があるか
                        s = 1
                        do kk = 1, C(jj)
                            if (f(ii, k) == f(jj, kk)) then
                                s = 0
                                exit
                            end if
                        end do
                        if (s == 1) then
                            !write (*, '(4x,a4,*(i0))') 'F   ', f(ii, 1:C(ii) + 1)
                            !write (*, '(4x,a4,*(i0))') '    ', f(jj, 1:C(jj) + 1)
                            exit yoko
                        end if
                    end do yoko
                    if (s == 0) then
                        ! write (*, '(4x,a4,*(i0))') 'F   ', f(ii, 1:C(ii) + 1)
                        !write (*, '(4x,a4,*(i0))') '    ', f(jj, 1:C(jj) + 1)
                        write (*, *) 'Yes'
                        stop
                    end if
                end if
            end if
        end do
    end do
    write (*, *) 'No'
end
recursive subroutine quicksort(a, b, first, last, N)
    implicit none
    integer(16) N
    integer(16) a(N), b(N), c(N, 100), i, j, tmp1, tmp2
    integer(16) first, last, center
    i = first
    j = last
    center = a(int((first + last)/2))
    do
        do while (a(i) < center)
            i = i + 1
        end do
        do while (center < a(j))
            j = j - 1
        end do
        if (i > j) then
            exit
        else if (i == j) then
            exit
        end if
        tmp1 = a(i); a(i) = a(j); a(j) = tmp1
        tmp2 = b(i); b(i) = b(j); b(j) = tmp2
        i = i + 1
        j = j - 1
    end do
    if (first < i - 1) call quicksort(a, b, first, i - 1, N)
    if (j + 1 < last) call quicksort(a, b, j + 1, last, N)
end subroutine quicksort
