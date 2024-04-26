module global_abc325c
    !S         ：センサの配置
    !cnt       ：センサの数
    !H,W       ：センサのサイズ
    !check_root：チェックしたマスの管理
    implicit none
    character(1), allocatable :: S(:, :)
    integer, allocatable::check_root(:, :)
    integer cnt, H, W
end module
program abc325c
    use global_abc325c
    implicit none
    integer i, j

    !入力
    read (*, *) H, W
    allocate (S(H, W), check_root(H, W))
    do i = 1, H
        read (*, '(*(a1))') S(i, 1:W)
    end do

    !センサの個数の集計
    cnt = 0
    do i = 1, H
        do j = 1, W
            if (S(i, j) == '#' .and. check_root(i, j) == 0) then
                !write (*, *) 'A', i, j
                cnt = cnt + 1
                call move(i, j)
            end if
        end do
    end do

    !結果の出力
    write (*, *) cnt

contains
    recursive subroutine move(i, j)
        integer i, j
        if (i > 0 .and. i <= H .and. j <= W .and. j > 0) then
            !移動先が壁
            if (S(i, j) == '.') return
            !移動先が訪問済：終了
            if (check_root(i, j) == 1) then
                return
            else
                !移動先が道
                !write (*, *) i, j
                check_root(i, j) = 1
                call move(i + 1, j) !下
                call move(i - 1, j) !上
                call move(i, j + 1) !右
                call move(i, j - 1) !左
                call move(i - 1, j - 1) !左斜め上
                call move(i - 1, j + 1) !右斜め上
                call move(i + 1, j - 1) !左斜め下
                call move(i + 1, j + 1) !右斜め下
            end if
        else
            return
        end if
    end subroutine
end
