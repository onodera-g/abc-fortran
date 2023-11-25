program abc329c
    !N    ：文字列の長さ
    !S    ：文字列
    !ascll：各文字の連続回数の最大値を記録(97:a, 122:z)
    !cnt  ：各文字の連続回数を一時保存
    !tmp  ：Sの各文字をアスキー文字に変換した結果
    implicit none
    integer N, tmp, cnt, i, j
    integer ascll(122)
    character(:), allocatable::S

    !入力
    read (*, *) N
    allocate (character(N)::S)
    read (*, *) S

    !集計
    ascll = 0
    i = 1
    do
        tmp = iachar(S(i:i))
        !初回カウント
        if (ascll(tmp) == 0) ascll(tmp) = 1
        !連続カウント
        cnt = 1
        do j = i + 1, N
            if (iachar(S(j:j)) == tmp) then
                cnt = cnt + 1
            else
                exit
            end if
        end do
        !MAX更新
        ascll(tmp) = MAX(ascll(tmp), cnt)
        i = j
        if (i > N) exit
    end do

    !結果出力
    cnt = 0
    do i = 97, 122
        cnt = ascll(i) + cnt
    end do
    write (*, *) cnt

end program abc329c
