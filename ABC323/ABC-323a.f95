program abc323a
    ! S(16)：文字列S
    implicit none
    character(16) s
    integer i

    read (*, *) S

    do i = 1, len_trim(S)
        if (mod(i, 2) /= 0) cycle
        if (S(i:i) == '1') then
            write (*, '(a)') 'No'
            stop
        end if
    end do
    write (*, '(a)') 'Yes'
end program abc323a
