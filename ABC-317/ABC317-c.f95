module ABC317c_mod
    implicit none
    ! グローバル変数
    integer(16), allocatable :: A(:), B(:), C(:)
    integer(16) distance_max, M, N
end module

program ABC317c
    use ABC317c_mod
    implicit none
    integer(16) i, distance
    integer(16), allocatable ::  visited(:)

    !入力
    read (*, *) N, M
    allocate (A(M), B(M), C(M), visited(N))
    do i = 1, M
        read (*, *) A(i), B(i), C(i)
    end do

    !DFS
    distance_max = 0
    do i = 1, M
        distance = 0; visited = 0; visited(A(i)) = 1
        call move(B(i), i, distance, visited)
        distance = 0; visited = 0; visited(B(i)) = 1
        call move(A(i), i, distance, visited)
    end do
    write (*, '(i0)') distance_max

contains

    !深さ優先探索
    recursive subroutine move(current, route, distance, visited)
        implicit none
        integer(16) current, route, distance, visited(N)
        integer(16) i, tmp(N), tmp2

        !移動先が訪問済かどうか
        visited(current) = 1
        distance = distance + C(route)
        distance_max = max(distance_max, distance)

        !次の訪問先を調べる
        do i = 1, M
            if (current == A(i) .and. visited(B(i)) == 0) then
                tmp = visited; tmp2 = distance
                call move(B(i), i, distance, visited)
                visited = tmp; distance = tmp2
            end if
            if (current == B(i) .and. visited(A(i)) == 0) then
                tmp = visited; tmp2 = distance
                call move(A(i), i, distance, visited)
                visited = tmp; distance = tmp2
            end if
        end do
    end subroutine
end program abc317c
