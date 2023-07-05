program ABC308c
    !================================================================
    ! N   ：総人数
    ! A   ：表の回数
    ! B   ：裏の回数
    ! C   ：人数
    ! D   ：成功率
    !================================================================
    implicit none

    integer(16) i, j
    integer N
    real(16), allocatable ::  D(:), A(:), B(:), C(:)

    integer, parameter :: rn = 10
    real :: x(rn), y(rn)
    N = 10 !read (*, *) N
    allocate (A(N), B(N), C(N), D(N))
    do j = 1, 100
        call random_number(x)
        call random_number(y)
        x = x*100
        y = y*100
        !----------------------
        !     N,A,Bの入力
        !----------------------
        i = 0
        !N = 10 !read (*, *) N
        !allocate (A(N), B(N), C(N), D(N))
        A = int(x)
        B = int(y)
        do i = 1, N
            !   read (*, *) A(i), B(i)
            C(i) = i
        end do

        !----------------------
        !     成功率の計算
        !----------------------
        D = (A/(A + B))
        call hsort(N, D, C)

        !----------------------
        !     結果の出力
        !----------------------
        !write (*, '(*(i0,1x))') int(C)
        do i = 1, N - 1
            if (D(i) > D(i + 1)) cycle
            if (D(i) == D(i + 1) .and. C(i) > C(i + 1)) then
                write (*, *) 'No'
                write (*, '(*(i0,1x))') int(C)
                write (*, '(*(f20.18,1x))') D
                !stop
            end if
        end do
        write (*, *) 'Yes'
        !write (*, '(*(f10.8,1x))') D
    end do
end

subroutine hsort(n, x, y)
    !================================================================
    ! Ref: Numerical Recipes in Fortran 77, 2nd ed.
    ! n   ：配列の最大数
    ! x   ：ノードの配列(成功率)
    ! y   ：ノードの配列(人)
    !================================================================
    implicit none
    integer, intent(IN) :: n
    real(16), intent(INOUT) :: x(n), y(n)
    ! local
    integer k
    real(16) :: tmp_x, tmp_y

    !----------------------
    !     ヒープの構成
    !----------------------
    do k = n/2, 1, -1
        tmp_x = x(k)
        tmp_y = y(k)
        call mkheap(x, y, k, n, tmp_x, tmp_y)
    end do

    !----------------------
    !      ヒープソート
    !----------------------
    do k = n, 2, -1
        tmp_x = x(k) !*一番上は一番でかいはずなので、一番後ろに入れる。次のcallで末尾をk-1にして外すため。
        x(k) = x(1) ! *x(1)がx(k)のままなのでソートできない様に見えるが、実際は親と子の比較はvalとx(j)でするのでx(1)を使うことはない。
        tmp_y = y(k) ! *値自体も最後にmaheapでx(i)=valで書き換えるので大丈夫。
        y(k) = y(1)
        call mkheap(x, y, 1, k - 1, tmp_x, tmp_y)
    end do

    return
end subroutine hsort

subroutine mkheap(x, y, root, leaf, val_x, val_y)
    !================================================================
    ! Ref: Numerical Recipes in Fortran 77, 2nd ed.
    ! root ：入れ替え対象のノード番号
    ! leaf ：ノードの最大番号(配列の末尾を指定)
    ! x    ：ノードの配列(成功率)
    ! val_x：入れ替え対象のノードの値(成功率)
    ! y    ：ノードの配列(人)
    ! val_y：入れ替え対象のノードの値(人)
    !※L92,L89の比較を大小逆にすると昇順、降順を逆に出来る。
    !================================================================
    implicit none
    integer, intent(IN)    ::  root, leaf
    real(16), intent(INOUT) :: x(1:leaf), y(1:leaf)
    !local
    integer :: i, j
    real(16) :: val_x, val_y

    i = root
    j = i*2
    do
        if (j > leaf) exit ! *子がなくなったら終了
        if (j < leaf) then
            if (x(j) >= x(j + 1)) j = j + 1 ! *子が2つある場合は、大きい方を x(j) とする。
        end if

        if (x(j) < val_x) then
            ! *子の方が大きければ x(j) を x(i) に上げた後
            ! *i=jにして、1つ下を検討候補に入れる。
            ! *一つ下がなければ、j>leafとなり配列の参照先がない=終了となる。
            x(i) = x(j)
            y(i) = y(j)
            i = j
            j = i*2
        else if (x(j) == val_x .and. val_y > y(j)) then
            ! *子の方が大きければ x(j) を x(i) に上げた後
            ! *i=jにして、1つ下を検討候補に入れる。
            ! *一つ下がなければ、j>leafとなり配列の参照先がない=終了となる。
            x(i) = x(j)
            y(i) = y(j)
            i = j
            j = i*2
        else
            ! *親の方が大きければ何もしないので、leaf+1して次のexitの判定で、do文の外に出す
            j = leaf + 1
        end if
    end do

    ! *値を入れ替える(子jを一つ上に上げる処理、L95)をすると、親iの値が消えてしまうので、本来は直後にx(j)にx(i)の値を入れておかなければいけない。
    ! *しかし、下に下がる親iの値は常に同じなので、入れ替えの処理をせずにvalに入れて管理している。
    ! *入れ替える対象(子j)が無くなった時点で、場所がrootの位置が確定するので、最後にx(i)=valで値を入れている。
    x(i) = val_x
    y(i) = val_y
    return
end subroutine mkheap
