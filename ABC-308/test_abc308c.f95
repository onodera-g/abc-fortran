module global_variable
    integer(4) N
end module
program ABC308c
    use global_variable
    implicit none
    real(16), allocatable :: A(:), B(:), D(:), tmp(:)
    integer(16), allocatable :: C(:), tmp2(:)
    integer(16) start, end, i, j, k
    integer, parameter :: rn = 100
    real :: x(rn), y(rn)
    !初期化
    i = 0
    !read (*, *) N
    N = 100
    allocate (A(N), B(N), C(N), D(N))
    allocate (tmp(N), tmp2(N))
    !
    do j = 1, 1000
        !
        call random_number(x)
        call random_number(y)
        x = x*100
        y = y*100
        A = int(x)
        B = int(y)
        !
        do i = 1, N
            !read (*, *) A(i), B(i)
            C(i) = i
        end do
        D = (A/(A + B))
        start = 1; end = N
        tmp = 0; tmp2 = 0
        call margesort(D, C, tmp, tmp2, start, end)
        ! write (*, '(*(i0,1x))') C
        do i = 1, N - 1
            if (D(i) > D(i + 1)) cycle
            if (D(i) == D(i + 1) .and. C(i) > C(i + 1)) then
                !write (*, *) 'No'
                write (*, '(*(i30,1x))') (int(C(k)), k=i, i + 1)
                write (*, '(*(f30.28,1x))') (D(k), k=i, i + 1)
                !stop
            end if
        end do
    end do
    write (*, *) 'Yes'
end program
recursive subroutine margesort(x, y, tmp, tmp2, left, right)
    integer(16) left, right, mid
    integer(16) N
    real(16) x(N), tmp(N)
    integer(16) y(N), tmp2(N)
    integer(16) i, j, k

    if (left >= right) return

    mid = (left + right)/2
    !分割できるだけ分割するa
    call margesort(x, y, tmp, tmp2, left, mid)
    call margesort(x, y, tmp, tmp2, mid + 1, right)
    !
    j = 0
    tmp(left:mid) = x(left:mid)
    tmp2(left:mid) = y(left:mid)

    do i = mid + 1, right
        tmp(i) = x(right - j)
        tmp2(i) = y(right - j)
        j = j + 1
    end do
    !
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
            !j = j - 1
            j = j + 1
        end if
    end do
    !write (*, '(3x,*(f13.10,1x))') x(left:right)
end subroutine margesort

