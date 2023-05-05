program ABC299a
    implicit none
    character(:), allocatable :: S
    integer N, i, j, k
    !データ読み込み
    READ (*, *) N
    allocate (character(N) :: S)
    READ (*, *) S
    !|*|　の検索
    do i = 1, N - 1, 1
        if (S(i:i) == "|") then !　検索：始点地点 "|"
            do J = I + 1, N, 1
                if (S(J:J) == "*") then !　検索：中間地点 "*"
                    do K = J + 1, N, 1
                        if (S(K:K) == "|") then !　検索：終点地点 "|"
                            !ここまで来たら成立しているので"in"にして終了
                            write (*, "(a)") 'in'
                            stop
                        end if
                    end do
                end if
            end do
        end if
    end do
    write (*, "(a)") "out" !doループを抜けている＝”|*|”がないのでout
end program
