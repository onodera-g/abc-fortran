program ABC305c
    implicit none
    integer i, j
    integer H, W, cnt_min, cnt_max
    character(:), allocatable :: c(:)
    integer, allocatable:: cnt_cookie(:, :)

    !値の入力
    read (*, *) H, W
    allocate (character(W) :: c(H))
    allocate (cnt_cookie(H, 2))
    read (*, *) (c(i), i=1, H)

    !初期化
    cnt_cookie = 0
    cnt_min = 100000
    cnt_max = 0

    !クッキーの位置の収集
    do i = 1, H
        do j = 1, W
            if (c(i) (j:j) == '#') then
                call cookie(j, W, c(i), cnt_cookie(i, 1), cnt_cookie(i, 2))
                if (cnt_min > cnt_cookie(i, 1)) cnt_min = cnt_cookie(i, 1)
                if (cnt_max < cnt_cookie(i, 2)) cnt_max = cnt_cookie(i, 2)
                exit
            end if
        end do
    end do

    !欠けたクッキーの検索
    do i = 1, H
        if (cnt_cookie(i, 1) /= 0) then
            if (cnt_cookie(i, 1) /= cnt_min .or. cnt_cookie(i, 2) /= cnt_max) then
                do j = cnt_min, cnt_max
                    if (c(i) (j:j) /= '#') then
                        write (*, *) i, j
                        stop
                    end if
                end do
            end if
        end if
    end do
contains

    !クッキーのスタート位置、エンド位置の検索
    subroutine cookie(j, W, c, start, end)
        integer j, k, W, start, end
        character(W) c
        start = j
        do k = j, W
            if (c(k:k) == '.') then
                end = k - 1
                return
            end if
        end do
        end = W
    end subroutine
end program
