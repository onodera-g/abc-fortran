
program abc325b
    !W         ：拠点にいる社員の数
    !X         ：拠点のタイムゾーン
    !N         ：拠点の数
    !tmp_member：各時間での参加できる社員の数
    !max_member：参加できる社員の最大人数
    !time      ：タイムゾーンを考慮した会議の開催時間
    implicit none
    integer(16) N, i, j, tmp_member, max_member, time
    integer(16), allocatable::W(:), X(:)

    !入力
    read (*, *) N
    allocate (W(N), X(N))
    do i = 1, N
        read (*, *) W(i), X(i)
    end do

    !会議に参加できる人数の集計
    tmp_member = 0; max_member = 0
    do i = 0, 23
        tmp_member = 0
        do j = 1, N
            time = X(j) + i
            if (time >= 24) time = time - 24
            if (9 <= time .and. time + 1 <= 18) then
                tmp_member = tmp_member + W(j)
            else
            end if
        end do
        max_member = max(tmp_member, max_member)
    end do

    !結果の出力
    write (*, *) max_member

end program abc325b
