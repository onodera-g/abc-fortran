program ABC303a
    implicit none
    integer i, N, diff
    character(:), allocatable:: S, T

    !入力
    READ (*, *) N
    allocate (character(N) :: S)
    allocate (character(N) :: T)
    READ (*, *) S, T

    !似た文字列の判定する
    diff = 0
    do i = 1, N
        if (S(i:i) /= T(i:i)) then
            diff = diff + 1
            if (S(i:i) == '1' .and. T(i:i) == 'l') diff = diff - 1
            if (S(i:i) == 'l' .and. T(i:i) == '1') diff = diff - 1
            if (S(i:i) == '0' .and. T(i:i) == 'o') diff = diff - 1
            if (S(i:i) == 'o' .and. T(i:i) == '0') diff = diff - 1
        end if
    end do

    !結果の出力
    if (diff == 0) then
        write (*, "(a)") 'Yes'
    else
        write (*, "(a)") 'No'
    end if
end
