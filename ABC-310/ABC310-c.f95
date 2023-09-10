program ABC310c
    implicit none
    integer(8) i, j, k, check, cnt
    integer(8) N, tmp
    character(10), allocatable :: S(:)
    character(10) tmp1, tmp2

    !初期化
    i = 0; tmp = 0; cnt = 0
    !入力
    read (*, *) N
    allocate (S(N))
    do i = 1, N
        read (*, *) S(i)
    end do

    !前後判定の検証
    do i = 1, N
        !write (*, '(i0,4x*(a))') i, S(i)
        do j = 1, N
            !write (*, '(4x,i0,4x*(a))') j, S(j)
            !例外を弾く
            if (i == j) cycle
            if (len_trim(S(i)) /= len_trim(S(j))) cycle
            !比較:回文
            check = 1
            tmp1 = S(i)
            tmp2 = adjustr(S(j))
            do k = 1, len_trim(S(i))
                if (tmp1(k:k) /= tmp2(11 - k:11 - k)) then
                    check = 0
                    exit
                end if
            end do
            if (check == 1) then
                write (*, *) i, j
                cnt = cnt + 1
            end if
            !比較:完全一致
            check = 1
            tmp2 = S(j)
            do k = 1, len_trim(S(i))
                if (tmp1(k:k) /= tmp2(k:k)) then
                    check = 0
                    exit
                end if
            end do
            if (check == 1) then
                write (*, *) i, j
                cnt = cnt + 1
            end if
        end do
    end do
    write (*, *) cnt
end
