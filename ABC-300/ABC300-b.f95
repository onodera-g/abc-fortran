program ABC300b
    implicit none
    integer H, W, i, j, move_v, move_h
    character(3), allocatable :: A(:, :), B(:, :), ans_v(:, :), ans_h(:, :)
    !H,W の入力
    read (*, *) H, W
    allocate (A(H, W))
    allocate (B(H, W))
    allocate (ans_v(H, W))
    allocate (ans_h(H, W))

    !A,Bの入力
    do i = 1, H
        read (*, '(*(a1))') (A(i, j), j=1, W)
    end do
    do i = 1, H
        read (*, '(*(a1))') (B(i, j), j=1, W)
    end do

    !縦にmove_v移動する
    do move_v = 0, H - 1
        !縦の移動を全マス反映
        do i = 1, H
            do j = 1, W
                if (i + move_v > H) then
                    ans_v(i, j) = A(i + move_v - H, j)
                else
                    ans_v(i, j) = A(i + move_v, j)
                end if
            end do
        end do
        !横にmove_h移動
        do move_h = 0, W - 1
            !横の移動を全マス反映
            do j = 1, W
                do i = 1, H
                    if (j + move_h > W) then
                        ans_h(i, j) = ans_v(i, j + move_h - W)
                    else
                        ans_h(i, j) = ans_v(i, j + move_h)
                    end if
                end do
            end do
            !結果確認用
            !write (*, *) 'v=', move_v, 'h=', move_h
            !do i = 1, H
            !   write (*, *) (ans_h(i, j), j=1, W)
            !end do
            !結果判定
            if (all(ans_h == B)) then
                write (*, "(a)") "Yes"
                stop
            end if
        end do
    end do
    write (*, "(a)") "No"
end program
