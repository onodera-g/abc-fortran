program ABC302b
    implicit none
    integer i, j, H, W, k, l, n
    integer Ans(5, 2)
    character(:), allocatable :: S(:) !配列数がn、n文字入る
    character(5) ::snuke = "snuke"

    Ans = 0
    !入力
    READ (*, *) H, W
    allocate (character(W) :: S(H))
    READ (*, *) S

    do i = 1, H
        do j = 1, W
            !sがあるか
            if (S(i) (j:j) == snuke(1:1)) then
                !sの周囲８マスにnがあるか
                do k = -1, 1
                    do l = -1, 1
                        if (i + 4*k > H .or. j + 4*l > W &
                            .or. i + 4*k < 1 .or. j + 4*l < 1) cycle
                        if (S(i + k) (j + l:j + l) == snuke(2:2)) then
                            if (S(i + 2*k) (j + 2*l:j + 2*l) == snuke(3:3)) then
                                if (S(i + 3*k) (j + 3*l:j + 3*l) == snuke(4:4)) then
                                    if (S(i + 4*k) (j + 4*l:j + 4*l) == snuke(5:5)) then
                                        do n = 0, 4
                                            write (*, '(i0, 1x, i0)') i + n*k, j + n*l
                                        end do
                                        stop
                                    end if
                                end if
                            end if
                        end if
                    end do
                end do
            end if
        end do
    end do
end
