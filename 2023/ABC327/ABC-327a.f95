program abc327a
    !N：文字列の長さ
    !S：文字列
    implicit none
    integer(16) N, i
    character(:), allocatable :: S

    !入力
    read (*, *) N
    allocate (character(N)::S)
    read (*, *) S

    !隣接判定
    do i = 1, N - 1
        if (S(i:i + 1) == 'ab' .or. S(i:i + 1) == 'ba') then
            write (*, '(a)') 'Yes'
            stop
        end if
    end do

    !結果の出力
    write (*, '(a)') 'No'
end program abc327a
