program ABC312a
    implicit none
    character(3) :: S

    !入力
    read (*, *) S

    !判定
    if (S == 'ACE') then
        write (*, *) 'Yes'
        stop
    end if
    if (S == 'BDF') then
        write (*, *) 'Yes'
        stop
    end if
    if (S == 'CEG') then
        write (*, *) 'Yes'
        stop
    end if
    if (S == 'DFA') then
        write (*, *) 'Yes'
        stop
    end if
    if (S == 'EGB') then
        write (*, *) 'Yes'
        stop
    end if
    if (S == 'FAC') then
        write (*, *) 'Yes'
        stop
    end if
    if (S == 'GBD') then
        write (*, *) 'Yes'
        stop
    end if
    write (*, *) 'No'

end program abc312a
