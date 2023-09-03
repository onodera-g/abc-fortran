program ABC314a
    implicit none
    character(100) pi
    integer N

    !初期化
    pi = '1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679'

    !入力
    read (*, *) N

    !出力
    write (*, '(a2)', ADVANCE='NO') '3.'
    write (*, '(a)') pi(1:N)

end program abc314a
