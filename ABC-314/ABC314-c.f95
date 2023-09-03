program ABC314c
    !implicit none
    integer(16) N, M, i, tmp
    integer(16), allocatable::  C(:), cnt(:), cnt_max(:)
    character(:), allocatable :: S
    type :: array
        character(1), allocatable :: arr(:)
        character(1), allocatable :: arr2(:)
    end type
    type(array), allocatable :: arr_of_arr(:), arr2_of_arr(:)
    !１次元の配列(arr_of_arr)の中に1次元の配列(arr)が入っています。
    !arr_of_arr(:)で縦の長さを可変長、arr(:)で横の長さを可変長にしています。

    read (*, *) N, M
    allocate (character(N) :: S)
    allocate (cnt_max(M), cnt(M)); cnt_max(M) = 0; cnt(M) = 0
    allocate (arr_of_arr(M), arr2_of_arr(M))

    read (*, *) S
    allocate (C(N))
    read (*, *) C(:)

    !各色の出現回数の取得
    do i = 1, N
        cnt(C(i)) = cnt(C(i)) + 1
    end do
    do i = 1, M
        allocate (arr_of_arr(i)%arr(cnt(i)))
        allocate (arr2_of_arr(i)%arr2(cnt(i)))
    end do
    cnt = 0
    do i = 1, N
        cnt(C(i)) = cnt(C(i)) + 1
        arr_of_arr(C(i))%arr(cnt(C(i))) = S(i:i)
    end do

    !巡回シフト
    cnt_max = cnt
    cnt = 0
    do i = 1, N
        cnt(C(i)) = cnt(C(i)) + 1
        tmp = mod(cnt(C(i)), cnt_max(C(i))) + 1
        arr2_of_arr(C(i))%arr2(tmp) = arr_of_arr(C(i))%arr(cnt(C(i)))
    end do

    !結果の出力
    cnt = 0
    do i = 1, N
        cnt(C(i)) = cnt(C(i)) + 1
        write (*, '(a)', advance='no') arr2_of_arr(C(i))%arr2(cnt(C(i)))
    end do
end
