program ABC299c
    implicit none
    character(:), allocatable :: S
    integer N, i, cnt, ans
    READ (*, *) N
    allocate (character(N) :: S)
    READ (*, *) S
    ans = 0
    cnt = 0
    do i = 1, N
        if (S(i:i) == "o") then
            cnt = cnt + 1
        else
            if (cnt > ans) ans = cnt
            cnt = 0
        END IF
        if (CNT > ans .AND. S(1:1) == '-') ans = cnt
    end do
    !結果出力
    if (ans .ne. 0) then
        write (*, "(i0)") ans
    else
        write (*, "(i0)") - 1
    end if

end program
