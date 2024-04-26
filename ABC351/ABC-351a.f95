program abc351a
    !
    implicit none
    integer A(9), B(8)

    !入力
    A = 0; B = 0
    read (*, *) A(:)
    read (*, *) B(:)

    !送料の計算
    write (*, *) sum(A) - sum(B) + 1
end program abc351a
