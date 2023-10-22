program abc324b
    !N    ：作りたい目的の整数
    !xa,ya：2**x,3**y
    !ans  ：xa+ya
    implicit none
    integer(8) N
    integer(8) x, y, ans, xa, ya

    !入力
    read (*, *) N
    x = 0; y = 0
    if (N == 1) then
        write (*, '(a)') 'Yes'
        stop
    end if

    !x基準
    xloop: do
        !初期値
        x = x + 1; y = 1
        !xの値更新
        xa = 2**x
        ans = xa
        if (xa >= N) exit xloop
        !yの値更新
        yloop: do
            ans = xa*(3**y)
            if (ans >= N) exit yloop
            y = y + 1
        end do yloop
        if (ans == N) exit
    end do xloop

    !結果の出力
    if (ans == N) then
        write (*, '(a)') 'Yes'
        stop
    end if

    !y基準
    x = 0; y = 0
    y2loop: do
        !初期値
        y = y + 1; x = 1
        !yの値更新
        ya = 3**y
        ans = ya
        if (ya >= N) exit y2loop
        !xの値更新
        x2loop: do
            ans = ya*(2**x)
            if (ans >= N) exit x2loop
            x = x + 1
        end do x2loop
        if (ans == N) exit
    end do y2loop

    !結果の出力
    if (ans == N) then
        write (*, '(a)') 'Yes'
    else
        write (*, '(a)') 'No'
    end if
end program abc324b
