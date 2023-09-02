program ABC315a
    implicit none
    integer i
    character(100) :: S

    !入力
    read (*, *) S

    !判定
    do i = 1, len(S)
        if (S(i:i) == 'a') cycle
        if (S(i:i) == 'e') cycle
        if (S(i:i) == 'i') cycle
        if (S(i:i) == 'o') cycle
        if (S(i:i) == 'u') cycle
        write (*, '(a)', advance='no') S(i:i)
    end do
end program abc315a
