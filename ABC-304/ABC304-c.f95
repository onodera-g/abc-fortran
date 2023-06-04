module ABC304c_global
    implicit none
    ! グローバル変数
    real, save, allocatable:: x(:), y(:), z(:)
    integer N

end module

program ABC304a
    use ABC304c_global
    implicit none
    integer i, D

    !入力
    READ (*, *) N, D
    allocate (y(N), x(N), z(N))
    do i = 1, N
        read (*, *) x(i), y(i)
    end do
    z = 0

    !感染判定
    z(1) = 1
    do i = 2, N
        call DFS(1, i)
    end do

    !結果の出力
    do i = 1, N
        if (z(i) == 1) then
            write (*, "(a)") 'Yes'
        else
            write (*, "(a)") 'No'
        end if
    end do
contains
    !深さ優先探索
    recursive subroutine DFS(i, j)
        integer i, j, k
        real kyori
        kyori = sqrt((x(i) - x(j))**2 + (y(i) - y(j))**2)
        if (kyori <= D) then
            z(j) = 1
            do k = 2, N
                if (z(k) == 0) call DFS(j, k)
            end do
        end if
    end subroutine
end
