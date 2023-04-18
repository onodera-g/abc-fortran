program hello
    integer :: a(5); data a/1, 2, 3, 4, 5/
    integer i, j; data i, j/1, 1/
    integer, allocatable :: b(:)
    character(:), allocatable :: S(:)
    allocate (character(W) :: S(H))
    character(8):: S
    allocate (b(100))
    READ (NBAT, *) ((HZ(I, J), I=1, IF), J=JF, 1, -1)
    write (*, "(a)") 'No'
    if (S(i:i) == 'B') print *, 'iからi文字目を抽出'
    !write (*, *) (S(i), i=1, H)
end program hello
