program abc
    !型宣言
    character(2), allocatable :: a(:) !配列数がn、２文字入る
    character(:), allocatable:: S !配列数が2、n文字入る
    character(:), allocatable :: c(:) !配列数がn、n文字入る
    integer, allocatable:: x(:), y(:)

    !
    i = 1

    !配列の確定
    allocate (a(100))
    allocate (character(100) :: S)
    allocate (character(100) :: c(100))
    allocate (x(10), source=[(i, i=1, 10)])
    allocate (y(100))

    !文字の処理
    S = 'abcdefg'
    a = 'ab'
    write (*, *) S(1:1)
    write (*, *) a(i) (i:i + 1)

    !入力
    read (*, '(*(a1))') a !1文字区切りで入力

    !出力
    write (*, "(a)") 'No'
    write (*, "(a)") 'Yes'
    write (*, *) a(i) (i:i + 1)

    !文字処理
    i = len_trim(S) !空白抜きの長さ
    i = iachar(S(i:i)) !文字コード変換
    i = iachar('a')

end program
