program abc318b
    implicit none
    integer i, j, N, cnt
    logical sheet(101, 101)
    integer, allocatable :: A(:), B(:), C(:), D(:)

    !読み込み
    read (*, *) N
    allocate (A(N), B(N), C(N), D(N))
    do i = 1, N
        read (*, *) A(i), B(i), C(i), D(i)
    end do

    !シートの作成
    A = A + 1; C = C + 1
    cnt = 0; sheet = .false.
    do i = 1, N
        do j = C(i), D(i)
            sheet(j, A(i):B(i)) = .true.
        end do
    end do

    !結果の出力
    cnt = count(sheet)
    write (*, '(i0)') cnt
end program abc318b
