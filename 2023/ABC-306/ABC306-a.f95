program ABC306a
    implicit none
    integer(8) i, N
    character(:), allocatable :: S
    character(1), allocatable ::S2(:)
    !入力
    READ (*, *) N
    allocate (character(N)::S)
    S2 = [character ::]
    READ (*, *) S

    !２倍して格納
    do i = 1, N
        S2 = [S2, S(i:i)]
        S2 = [S2, S(i:i)]
    end do

    !結果の出力
    write (*, *) S2
end
