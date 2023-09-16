program abc320c
    implicit none
    integer M, t1, t2, t3, tmp(3), ans
    character(:), allocatable:: S(:)
    character(1) slot(3)

    !読み込み
    read (*, *) M
    allocate (character(M):: S(3))
    read (*, *) S(1)
    read (*, *) S(2)
    read (*, *) S(3)

    !スロットを回す
    ans = 777777777
    do t1 = 0, 300
        tmp(1) = mod(t1, M) + 1
        slot(1) = S(1) (tmp(1):tmp(1))
        do t2 = 0, 300
            if (t1 == t2) cycle
            tmp(2) = mod(t2, M) + 1
            slot(2) = S(2) (tmp(2):tmp(2))
            if (slot(1) /= slot(2)) cycle
            do t3 = 0, 300
                if (t1 == t3) cycle
                if (t2 == t3) cycle
                tmp(3) = mod(t3, M) + 1
                slot(3) = S(3) (tmp(3):tmp(3))
                if (slot(2) /= slot(3)) cycle
                ans = min(ans, max(t1, t2, t3))
            end do
        end do
    end do

    !結果の出力
    if (ans == 777777777) then
        write (*, '(i0)') - 1
    else
        write (*, '(i0)') ans
    end if
end program abc320c
