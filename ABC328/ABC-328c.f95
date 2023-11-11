program abc328c
    !S  ：文字列
    !N  ：Sの文字数
    !Q  ：Sに関する質問の数
    !R  ：隣り合う文字の一致判定をしたい範囲の始点
    !L  ：隣り合う文字の一致判定をしたい範囲の終点
    !cnt：隣り合う文字の一致個数
    !ans：各質問ごとに集計した隣り合う文字の一致個数
    integer N, Q
    integer, allocatable::cnt(:)
    character(:), allocatable::S
    integer ans
    integer, allocatable::R(:), L(:)

    !入力
    read (*, *) N, Q
    allocate (character(N)::S)
    allocate (R(Q), L(Q), cnt(N))
    read (*, *) S
    do i = 1, Q
        read (*, *) L(i), R(i)
    end do

    !隣り合う文字の一致判定
    cnt = 0
    if (S(1:1) == S(2:2)) cnt(1) = 1
    do i = 2, N - 1
        cnt(i) = cnt(i - 1)
        if (S(i:i) == S(i + 1:i + 1)) cnt(i) = cnt(i) + 1
    end do

    !結果の出力
    ans = 0
    do i = 1, Q
        ans = cnt(R(i) - 1) - cnt(L(i) - 1)
        write (*, '(i0)') ans
    end do
end program abc328c
