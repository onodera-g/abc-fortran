program ABC306c
    integer(8) i, j, N
    integer(8), allocatable:: A(:), cnt(:), A2(:), Ans(:)
    !初期化
    i = 0; j = 0; N = 0

    !入力
    read (*, *) N
    allocate (A(3*N), cnt(N), A2(N), Ans(N))
    read (*, *) (A(i), i=1, 3*N)
    do i = 1, N
        Ans(i) = i
    end do

    !1~Nの出現回数のカウント
    do i = 1, 3*N
        j = A(i)
        cnt(j) = cnt(j) + 1
        if (cnt(j) == 2) then
            A2(A(i)) = i
        end if
    end do

    !登場順に並び替え
    call quicksort(A2, Ans, 1, N)

    !結果の出力
    write (*, *) Ans
end

!sub:クイックソート
recursive subroutine quicksort(a, b, first, last)
    implicit none
    integer*8 a(*)
    integer*8 b(*)
    integer*8 first, last
    integer(8) i, j
    real*8 x, t, t2
    x = a((first + last)/2)
    i = first
    j = last
    do
        do while (a(i) < x)
            i = i + 1
        end do
        do while (x < a(j))
            j = j - 1
        end do
        if (i >= j) exit
        t = a(i); a(i) = a(j); a(j) = t
        t2 = b(i); b(i) = b(j); b(j) = t2
        i = i + 1
        j = j - 1
    end do
    if (first < i - 1) call quicksort(a, b, first, i - 1)
    if (j + 1 < last) call quicksort(a, b, j + 1, last)
end subroutine quicksort
