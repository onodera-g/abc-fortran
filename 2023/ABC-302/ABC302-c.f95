program ABC302c
    implicit none
    logical check
    integer N, M, i, j, diff, k
    character(:), allocatable :: S(:)
    integer, allocatable :: S_order(:)
    !入力
    READ (*, *) N, M
    allocate (character(M) :: S(N))
    allocate (S_order(N), source=[(i, i=1, n)])

    READ (*, *) S
    do k = 1, gamma(dble(N + 1))
        diff = 0
        check = .true.
        do i = 1, N - 1
            do j = 1, M
                if (S(S_order(i)) (j:j) /= S(S_order(i + 1)) (j:j)) diff = diff + 1
            end do
            if (diff > 1) then
                check = .false.
                exit
            else
                !write (*, *) 'S_order = (', S_order, ' )', S(S_order(i)), ' > ', S(S_order(i + 1)), i + 1
            end if
            diff = 0
        end do
        if (check .eqv. .true.) then
            write (*, '(a)') "Yes"
            stop
        end if
        call next_permutation(S_order, N)
    end do
    write (*, '(a)') "No"

end program

subroutine next_permutation(arr, n)

    integer i, n, k, l, kari, arr(n), tmp(n)

    do k = n - 1, 1, -1
        if (arr(k) < arr(k + 1)) exit
        if (k == 1) then
            !stop
        end if
    end do

    !STEP 2
    do l = n, k + 1, -1
        if (arr(k) < arr(l)) exit
    end do

    !STEP 3
    kari = arr(k)
    arr(k) = arr(l)
    arr(l) = kari

    !STEP 4
    if (k + 1 /= n) then
        do i = 1, k
            tmp(i) = arr(i)
        end do
        do i = 0, n - (k + 1)
            tmp(n - i) = arr(k + 1 + i)
        end do
        arr = tmp
    end if
end subroutine
