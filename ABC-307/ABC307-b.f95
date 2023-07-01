program ABC307b
    implicit none
    integer(8) i, j, k, l
    integer(8) N, nagasa, guki
    character(50), allocatable :: S(:)
    character(100) S2
    character(1) mae, usiro

    !初期化
    i = 0; j = 0; k = 0

    !入力
    read (*, *) N
    allocate (S(N))
    read (*, *) (S(i), i=1, N)

    !処理
    do i = 1, N
        do j = 1, N
            if (i == j) cycle
            S2 = trim(S(i))//trim(S(j))
            nagasa = len(trim(S2))
            guki = mod(nagasa, 2)
            !write (*, '(a)') S2
            kaibun: do k = 1, nagasa
                l = nagasa + (1 - k)
                mae = S2(k:k)
                usiro = S2(l:l)
                if (mae == usiro) then
                    !write (*, '(a,1x,a)') S2(k:k), S2(l:l)
                    if (l - k == 1 .and. guki == 0) then
                        write (*, "(a)") 'Yes'
                        stop
                    else if (l - k == 2 .and. guki == 1) then
                        write (*, "(a)") 'Yes'
                        stop
                    end if
                else
                    exit kaibun
                end if
            end do kaibun
        end do
    end do

    !結果の出力
    write (*, "(a)") 'No'
end
