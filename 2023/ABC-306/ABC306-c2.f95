program ABC306c
    integer i, N, j
    integer, allocatable:: A(:), cnt(:), Ans(:)
    !初期化
    i = 0
    Ans = [integer ::]

    !入力
    read (*, *) N
    allocate (A(3*N), cnt(N))
    read (*, *) (A(i), i=1, 3*N)

    !1~nの出現地位の検出
    do i = 1, 3*N
        j = A(i)
        cnt(j) = cnt(j) + 1
        !カウント２ならAnsに追加
        if (cnt(j) == 2) then
            Ans = [Ans, j]
        end if
        ![TLE回避策①]最後１つだけ見つかってないのなら、最後尾確定なのでAnsに追加
        if (cnt(j) == 1 .and. size(Ans) == N - 1) Ans = [Ans, j]
        ![TLE回避策②]Ansの要素数がNなら、全部見つかっているので終了
        if (size(Ans) == N) exit
    end do
    !結果の出力
    write (*, *) Ans
end
