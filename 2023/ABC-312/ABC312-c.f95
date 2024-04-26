program ABC312c
    implicit none
    integer(8) i, j, cnt_A, cnt_B, start
    integer(8) N, M
    integer(8), allocatable::  A(:), B(:), events(:)
    character(1), allocatable::events_AB(:)

    !初期化
    i = 0; j = 0

    !入力
    read (*, *) N, M
    allocate (A(N), B(M))
    allocate (events_AB(N + M), events(N + M))
    read (*, *) A(:)
    read (*, *) B(:)

    !読み出し順の作成
    B = B + 1
    events(1:N) = A; events(N + 1:M) = B
    events_AB = 'B'; events_AB(1:N) = 'A'
    start = 1
    call quicksort(events, events_AB, start, N + M)

    !判定
    cnt_A = 0; cnt_B = M
    do i = 1, N + M
        if (events_AB(i) == 'A') then
            cnt_A = cnt_A + 1
        else
            cnt_B = cnt_B - 1
        end if
        if (cnt_A >= cnt_B) then
            write (*, '(i0)') events(i)
            stop
        end if
    end do
contains
!sub:クイックソート
    recursive subroutine quicksort(a, b, first, last)
        implicit none
        integer(8) a(:), i, j, t
        character(1) b(:), t2
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

            t = a(i); a(i) = a(j); a(j) = t
            t2 = b(i); b(i) = b(j); b(j) = t2
            i = i + 1
            j = j - 1
        end do
        if (first < i - 1) call quicksort(a, b, first, i - 1)
        if (j + 1 < last) call quicksort(a, b, j + 1, last)
    end subroutine quicksort
end program abc312c
