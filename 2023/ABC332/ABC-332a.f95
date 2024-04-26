program abc332a
    !P(N) ：商品の値段
    !Q(N) ：商品の購入個数
    !S    ：送料の無料ライン
    !K    ：送料
    !price：支払い金額
    implicit none
    integer i
    integer N, S, K
    integer, allocatable:: P(:), Q(:)
    integer price

    !入力
    read (*, *) N, S, K
    allocate (P(N), Q(N))
    do i = 1, N
        read (*, *) P(i), Q(i)
    end do

    !購入金額の計算
    do i = 1, N
        price = price + P(i)*Q(i)
    end do

    !送料の計算
    if (price < S) price = price + K

    !結果の出力
    write (*, *) price
end program abc332a
