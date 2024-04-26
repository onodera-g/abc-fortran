program abc330a
    !N   :受験者数
    !A(N):N人分の試験の点数
    !L   :合格点
    !cnt :合格者数
    implicit none
    integer i, N, L, cnt
    integer, allocatable::A(:)

    !入力
    read (*, *) N, L
    allocate (A(N))
    read (*, *) A(1:N)

    !カウント
    cnt = 0
    do i = 1, N
        if (A(i) >= L) cnt = cnt + 1
    end do

    !結果の出力
    write (*, *) cnt
end program abc330a
