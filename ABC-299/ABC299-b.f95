program ABC299b
    !変数定義
    implicit none
    integer, allocatable :: C(:), R(:)
    integer N, T, i, Check2
    integer MaxNumber, MaxUser

    !入力
    READ (*, *) N, T
    allocate (C(N))
    allocate (R(N))
    READ (*, *) C
    READ (*, *) R

    !2があるか検索
    Check2 = 0
    MaxNumber = 0
    MaxUser = 0
    do i = 1, N
        if (C(i) == T) then
            Check2 = 1
            exit
        end if
    end do

    !最大値検索
    if (Check2 == 1) then ! 2がある場合
        do i = 1, N
            if (C(i) == T .and. R(i) > MaxNumber) then
                MaxNumber = R(i)
                MaxUser = i
            end if
        end do
    else ! 2がない場合
        do i = 1, N
            if (C(i) == C(1) .and. R(i) > MaxNumber) then
                MaxNumber = R(i)
                MaxUser = i
            end if
        end do
    end if

    !結果出力
    write (*, "(i0)") MaxUser
end program
