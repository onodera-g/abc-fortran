program ABC297c
    implicit none
    integer H, W, i, j
    character(:), allocatable :: S(:)
    read (*, *) H, W
    allocate (character(W) :: S(H))
    read (*, *) S
    do i = 1, H
        do j = 1, W - 1
            if (S(i) (j:j + 1) == 'TT') then
                !print *, 'i=', i, 'j=', j, S(i) (j:j + 1)
                S(i) (j:j + 1) = 'PC'
            end if
        end do
    end do
    do i = 1, H
        write (*, "(a)") S(i)
    end do
end program
