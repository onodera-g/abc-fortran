program abc331b
    !S,M,L   ：各パックの値段
    !sn,mn,ln：各パックの購入個数
    !N       ：卵の必要個数
    !nn      ：購入した卵の個数
    !p       ：卵をnn個購入した際の金額
    !minp    ：条件を満たす最小の購入金額
    implicit none
    integer(16) N, S, M, L
    integer(16) sn, mn, ln, minp, p, i, j, k, nn

    read (*, *) N, S, M, L

    minp = 1000000000
    i = 0
    do
        ln = i
        nn = ln*12
        if (nn >= N) then
            p = ln*L
            minp = min(minp, p)
            exit
        end if
        j = 0
        do
            mn = j
            nn = ln*12 + mn*8
            if (nn >= N) then
                p = ln*L + mn*M
                minp = min(minp, p)
                exit
            end if
            k = 0
            do
                sn = k
                nn = ln*12 + mn*8 + sn*6
                if (nn >= N) then
                    p = ln*L + mn*M + sn*S
                    minp = min(minp, p)
                    exit
                end if
                k = k + 1
            end do
            j = j + 1
        end do
        i = i + 1
    end do
    write (*, *) minp
end program abc331b
