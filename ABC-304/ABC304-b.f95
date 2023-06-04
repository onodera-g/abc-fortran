program ABC304b
    implicit none
    integer(8) N

    !入力
    READ (*, *) N

    !Nの近似値判定
    if (N <= 10**3 - 1) then !N が 10^3 −1 以下
        write (*, "(i0)") N
        stop
    end if
    if (N >= 10**3 .and. N <= 10**4 - 1) then !N が 10^3 以上、10^4 −1 以下
        N = N/(10**1)
        N = N*(10**1)
        write (*, "(i0)") N
        stop
    end if
    if (N >= 10**4 .and. N <= 10**5 - 1) then !N が 10^4 以上、10^5 −1 以下
        N = N/(10**2)
        N = N*(10**2)
        write (*, "(i0)") N
        stop
    end if
    if (N >= 10**5 .and. N <= 10**6 - 1) then !N が 10^5 以上、10^6 −1 以下
        N = N*(10**3)
        write (*, "(i0)") N
        stop
    end if
    if (N >= 10**6 .and. N <= 10**7 - 1) then !N が 10^6 以上、10^7 −1 以下
        N = N/(10**4)
        N = N*(10**4)
        write (*, "(i0)") N
        stop
    end if
    if (N >= 10**7 .and. N <= 10**8 - 1) then !N が 10^7 以上、10^8 −1 以下
        N = N/(10**5)
        N = N*(10**5)
        write (*, "(i0)") N
        stop
    end if
    if (N >= 10**8 .and. N <= 10**9 - 1) then !N が 10^8 以上、10^9 −1 以下
        N = N/(10**6)
        N = N*(10**6)
        write (*, "(i0)") N
        stop
    end if
end
