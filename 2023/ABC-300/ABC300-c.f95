program ABC300c
    implicit none
    integer H, W, i, j, k, size
    character(1), allocatable :: C(:, :)
    integer, allocatable::cnt(:)

    !H,W の入力
    read (*, *) H, W
    allocate (C(H, W))
    size = min(H, W)
    allocate (cnt(size))

    !Cの入力
    do i = 1, H
        read (*, '(*(a1))') (C(i, j), j=1, W)
    end do

    !#をカウント
    cnt = 0
    do i = 2, H - 1
        do j = 2, W - 1
            if (C(i, j) == '#') then
                do k = 1, size
                    if (C(i - k, j - k) == '#' .and. C(i - k, j + k) == '#' &
                        .and. C(i + k, j - k) == '#' .and. C(i + k, j + k) == '#') then
                        cnt(k) = cnt(k) + 1
                        cnt(k - 1) = cnt(k - 1) - 1
                    else
                        exit
                    end if
                end do
            end if
        end do
    end do

    !結果出力
    write (*, '(*(i0, 1x))') cnt
end
