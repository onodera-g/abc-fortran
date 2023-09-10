program ABC311b
    implicit none
    integer i, j, k
    integer N, D, cnt_day, cnt_free, ans
    character(1), allocatable :: S(:, :)
    !初期化
    i = 0; j = 0; k = 0; 
    N = 0; D = 0; ans = 0
    cnt_day = 0; cnt_free = 0

    !入力
    read (*, *) N, D
    allocate (S(N, D))
    do i = 1, N
        read (*, '(*(a1))') S(i, :)
    end do

    !スケジュールの確認
    do i = 1, D
        do j = 1, N
            if (S(j, i) == 'o') cnt_free = cnt_free + 1
        end do
        if (cnt_free == N) then
            cnt_day = cnt_day + 1
            if (ans < cnt_day) ans = cnt_day
        else
            cnt_day = 0
        end if
        cnt_free = 0
    end do

    !結果の出力
    write (*, *) ans
end program abc311b
