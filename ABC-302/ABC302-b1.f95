program ABC302b
    implicit none
    integer i, j, H, W, k, l, m, n
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
                Ans(1, 1) = i
                Ans(1, 2) = j
                !sの周囲８マスにnがあるか
                do k = -1, 1
                    do l = -1, 1
                        if (S(i + k) (j + l:j + l) == snuke(2:2)) then
                            if (i + 4*k > H .or. j + 4*l > W &
                                .or. i + 4*k < 1 .or. j + 4*l < 1) cycle
                            Ans(2, 1) = i + k
                            Ans(2, 2) = j + l
                            do m = 2, 4
                                if (S(i + m*k) (j + m*l:j + m*l) == snuke(m + 1:m + 1)) then
                                    Ans(1 + m, 1) = i + m*k
                                    Ans(1 + m, 2) = j + m*l
                                else
                                    exit
                                end if
                            end do
                            if (Ans(5, 2) /= 0) then
                                do n = 1, 5
                                    write (*, '(i0, 1x, i0)') ans(n, 1), ans(n, 2)
                                end do
                                stop
                            end if
                        end if
                    end do
                end do
            end if
        end do
    end do
end
