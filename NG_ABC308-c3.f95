program ABC308c
    implicit none
    integer(16) i, j, s
    integer(16) N
    real(16), allocatable :: A(:), B(:), C(:), D(:)

    !初期化
    i = 0; j = 0

    read (*, *) N
    allocate (A(N), B(N), C(N), D(N))

    !
    do i = 1, N
        read (*, *) A(i), B(i)
        d(i) = i
    end do

    s = 1
    call quicksort(A, B, C, D, S, N)

    !結果の出力
    write (*, *) int(d)

end

recursive subroutine quicksort(a, b, c, d, first, last)
    implicit none
    real(16) a(*), b(*), c(*), d(*)
    integer(16) a_center, b_center
    integer(16) first, last, center
    integer(16) i, j, k
    integer(16) t, t2, t3, t4, tmp1, tmp2

    !
    t = 0; t2 = 0; t3 = 0; t4 = 0; tmp1 = 0; tmp2 = 0; 
    c(first:last) = 0

    !基準点の設定
    center = (first + last)/2
    a_center = a(center)
    b_center = b(center)

    !
    do k = first, last
        c(k) = (a(k)*(a_center + b_center))*10d0**20d0
    end do
    i = first
    j = last

    !比較
    do
        do while (c(i) > (a_center*(a(i) + b(i)))*10d0**20d0)
            i = i + 1
        end do
        do while ((a_center*(a(j) + b(j)))*10d0**20d0 > c(j))
            j = j - 1
        end do
        if (i >= j) exit
        tmp1 = a(i)*(a(j) + b(j))
        tmp2 = a(j)*(a(i) + b(i))
        if (tmp1 /= tmp2) then
            t = a(i); a(i) = a(j); a(j) = t
            t2 = b(i); b(i) = b(j); b(j) = t2
            t3 = c(i); c(i) = c(j); c(j) = t3
            t4 = d(i); d(i) = d(j); d(j) = t4
        elseif (tmp1 == tmp2 .and. d(i) < d(j)) then
            t = a(i); a(i) = a(j); a(j) = t
            t2 = b(i); b(i) = b(j); b(j) = t2
            t3 = c(i); c(i) = c(j); c(j) = t3
            t4 = d(i); d(i) = d(j); d(j) = t4
        end if
        i = i + 1
        j = j - 1
    end do

    if (first < i - 1) call quicksort(a, b, c, d, first, i - 1)
    if (j + 1 < last) call quicksort(a, b, c, d, j + 1, last)
end subroutine quicksort
