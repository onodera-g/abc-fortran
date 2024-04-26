program ABC315b
    implicit none
    integer i
    integer M, sum, ave
    integer(4), allocatable ::D(:)
    !初期化
    sum = 0; ave = 0

    !入力
    read (*, *) M
    allocate (D(M))
    read (*, *) D(:)

    !１年の日数の計算
    do i = 1, M
        sum = sum + D(i)
    end do
    ave = (sum + 1)/2

    !真ん中の日数を調べる
    do i = 1, M
        if (ave - D(i) > 0) then
            ave = ave - D(i)
        else
            write (*, *) i, ave
            stop
        end if
    end do
end program abc315b
