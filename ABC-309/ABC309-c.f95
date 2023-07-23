
program ABC309c
    implicit none
    integer(8) i
    integer(8) N, K, tmp
    integer(8), allocatable ::a(:), b(:)

    !初期化
    i = 0; tmp = 0

    !入力
    read (*, *) N, K
    allocate (a(N), b(N))
    do i = 1, N
        read (*, *) a(i), b(i)
        tmp = tmp + b(i)
    end do

    !昇順にソート
    call quicksort(a, b, 1, N, N)

    ! 1 日目：飲む薬がK以下かどうか
    if (tmp <= K) then
        write (*, '(i0)') 1
        stop
    end if

    ! i 日目：飲む薬がK以下かどうか
    do i = 1, N
        tmp = tmp - b(i)
        if (tmp <= K) then
            write (*, '(i0)') a(i) + 1
            stop
        end if
    end do
end program

!sub:クイックソート
recursive subroutine quicksort(a, b, first, last, N)
    implicit none
    integer(8) N
    integer(8) a(N), b(N), i, j, tmp1, tmp2
    integer(8) first, last, center
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
