program abc319a
    implicit none
    character(10) S, top_name(10)
    integer top_num(10), i

    !TOP10の定義
    top_name(1) = 'tourist'; top_num(1) = 3858
    top_name(2) = 'ksun48'; top_num(2) = 3679
    top_name(3) = 'Benq'; top_num(3) = 3658
    top_name(4) = 'Um_nik'; top_num(4) = 3648
    top_name(5) = 'apiad'; top_num(5) = 3638
    top_name(6) = 'Stonefeang'; top_num(6) = 3630
    top_name(7) = 'ecnerwala'; top_num(7) = 3613
    top_name(8) = 'mnbvmar'; top_num(8) = 3555
    top_name(9) = 'newbiedmy'; top_num(9) = 3516
    top_name(10) = 'semiexp'; top_num(10) = 3481

    !読み込み、結果の出力
    read (*, *) S
    do i = 1, 10
        if (trim(S) == top_name(i)) then
            write (*, '(i0)') top_num(i)
            stop
        end if
    end do
end program abc319a
