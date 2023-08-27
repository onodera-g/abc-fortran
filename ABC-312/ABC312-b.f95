program ABC312b
    implicit none
    integer i, j
    integer N, M
    character(1), allocatable::  S(:, :)

    !初期化
    i = 0; j = 0
    N = 0; M = 0
    !入力
    read (*, *) N, M
    allocate (S(N, M))
    do i = 1, N
        read (*, '(*(a1))') (S(i, j), j=1, M)
    end do

    !判定
    do i = 1, N
        do j = 1, M
            !黒マス判定
            if (any(S(i:i + 2, j:j + 2) /= '#')) cycle
            if (any(S(i + 6:i + 8, j + 6:j + 8) /= '#')) cycle
            !シロマス判定
            !縦
            if (any(S(i:i + 2, j + 3) /= '.')) cycle
            if (any(S(i + 6:i + 8, j + 5) /= '.')) cycle
            !横
            if (any(S(i + 3, j:j + 3) /= '.')) cycle
            if (any(S(i + 5, j + 5:j + 8) /= '.')) cycle
            !結果の出力(随時)
            write (*, '(i0,1x,i0)') i, j
        end do
    end do

end program abc312b
