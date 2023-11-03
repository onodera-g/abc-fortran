program abc326a
    ! X：現在階
    ! Y：目的階
    implicit none
    integer x, y

    !入力
    read (*, *) X, Y

    !出力
    if (Y > X) then
        if (X + 2 >= Y) then
            write (*, '(a)') 'Yes'
        else
            write (*, '(a)') 'No'
        end if
    else
        if (X - 3 <= Y) then
            write (*, '(a)') 'Yes'
        else
            write (*, '(a)') 'No'
        end if
    end if
end program abc326a
