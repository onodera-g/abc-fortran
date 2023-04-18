program ABC297b
    implicit none
    integer(8) i, j, k, K1
    integer(8) B2(2), R2(2)
    character(8):: S

    j = 1
    k = 1
    !write (*, *) '>> S'
    read (*, *) S

    do i = 1, 8
        if (S(i:i) == 'B') then
            B2(j) = mod(i, 2)
            j = j + 1
        else if (S(i:i) == 'K') then
            K1 = i
        end if
        if (S(i:i) == 'R') then
            R2(k) = i
            k = k + 1
        end if
    end do

    !print *, 'R=', R2; print *, 'B=', B2; print *, 'K=', K
    if (R2(1) < K1 .and. K1 < R2(2) .and. B2(1) .ne. B2(2)) then
        write (*, "(a)") 'Yes'
    else
        write (*, "(a)") 'No'
    end if
end program
