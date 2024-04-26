program ABC302a
    implicit none
    integer(16) i, A, B, j, cnt_mae, cnt_usiro, Ans, hantei, sa
    !入力
    READ (*, *) A, B

    !確認
    i = 0
    Ans = 0
    do
        i = i + 1
        Ans = A - B**i
        if (Ans <= 0) then
            cnt_mae = (B**(i - 1))
            cnt_usiro = (B**i)
            do
                hantei = cnt_mae + cnt_usiro
                !write (*, *) (cnt_mae + cnt_usiro)/2
                hantei = hantei/2
                if (A >= hantei) then
                    cnt_mae = hantei
                else
                    cnt_usiro = hantei
                end if
                !write (*, *) cnt_mae, '<', A, '<', cnt_usiro
                sa = cnt_usiro - cnt_mae
                if (sa <= B) then
                    if (cnt_mae == A) then
                        write (*, *) cnt_mae/B
                    else
                        write (*, *) cnt_mae/B + 1
                    end if
                    stop
                end if
            end do
        end if
    end do
end program

