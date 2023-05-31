module mod_hash_map
    implicit none
    integer, parameter :: int_max_value = 2147483647 ! integer(4)の最大値
    integer, parameter :: default_initial_capacity = 16 ! デフォルトの初期容量
    integer, parameter :: maximum_capacity = lshift(1, 30) ! ハッシュセットに保持できる最大の要素数
    real, parameter :: default_load_factor = 0.75 ! デフォルトの負荷係数

    type t_entry ! キーと値のペアを保持するノード
        integer :: key ! キー
        integer :: val ! 値
        type(t_entry), pointer :: next => null() ! このノードに繋がっている次のノード
        integer :: hash ! ハッシュ値
    end type

    type t_entry_ptr ! ノードのポインタ
        type(t_entry), pointer :: ref => null()
    end type

    type t_hash_map ! ハッシュマップ
        ! tableはキーと値のペアのノードを管理する配列
        type(t_entry_ptr), private, allocatable :: table(:)
        integer :: size = 0
        ! thresholdはtableのサイズを大きくするかの基準
        integer, private :: threshold = int(default_initial_capacity*default_load_factor)
        ! load_factorはthresholdを決めるための負荷係数
        real, private :: load_factor = default_load_factor
    contains
        procedure :: is_empty => is_empty
        procedure :: is_not_empty => is_not_empty
        procedure :: put => put
        procedure :: get => get
        procedure :: get_or_default => get_or_default
        procedure :: remove => remove
        procedure :: clear => clear
        procedure :: contains_key => contains_key
        procedure :: contains_val => contains_val
    end type

    interface hash_map ! ハッシュマップのコンストラクタ
        module procedure :: newhm0, newhm1, newhm2
    end interface

contains

    ! ノードのコンストラクタ
    function new_entry(key, val, h) result(res)
        integer, intent(in) :: key
        integer, intent(in) :: val
        integer, intent(in) :: h
        type(t_entry), pointer :: res
        allocate (res)
        res%key = key
        res%val = val
        res%hash = h
    end

    ! integer(4)のハッシュ値を計算する関数
    ! integer(4)だとf(n) = nの形になっていて無駄に見えますが、
    ! integer(8)やreal・character等の場合は、
    ! それらの値に応じて固有のinteger(4)を計算するために必要です。
    ! 例えばinteger(8)の場合はres = xor(i, shr(i, 32))など。
    ! 注：shrは下で宣言されている関数です。
    integer function hash_code(i) result(res)
        integer, intent(in) :: i
        res = i
    end

    ! ハッシュマップのコンストラクタ
    type(t_hash_map) function newhm0() result(res)
        allocate (res%table(default_initial_capacity))
    end

    ! ハッシュマップのコンストラクタ
    type(t_hash_map) function newhm1(initial_capacity) result(res)
        integer, intent(in) :: initial_capacity
        res = newhm2(initial_capacity, default_load_factor)
    end

    ! ハッシュマップのコンストラクタ
    type(t_hash_map) function newhm2(initial_capacity, load_factor) result(res)
        integer, intent(in) :: initial_capacity
        real, intent(in) :: load_factor
        integer :: capacity
        if (initial_capacity < 0) then
            capacity = default_initial_capacity
        else
            capacity = 1
            do while (capacity < min(initial_capacity, maximum_capacity))
                capacity = lshift(capacity, 1)
            end do
        end if

        if (load_factor <= 0 .or. load_factor /= load_factor) then
            res%load_factor = default_load_factor
        else
            res%load_factor = load_factor
        end if

        res%threshold = int(capacity*res%load_factor)
        allocate (res%table(capacity))
    end

    ! ビットシフト関数(Javaの>>>に相当)
    integer function shr(i, n) result(res)
        integer, intent(in) :: i, n
        if (n == 0) then
            res = i
        else
            res = rshift(ibclr(rshift(i, 1), 31), n - 1)
        end if
    end

    ! tableのインデックスに用いるハッシュ値生成関数
    integer function hash(i) result(res)
        integer, intent(in) :: i
        integer :: h
        h = i
        h = xor(h, xor(shr(h, 20), shr(h, 12)))
        res = xor(h, xor(shr(h, 7), shr(h, 4)))
    end

    ! ハッシュ値を1~size(table)に圧縮する関数
    integer function index_for(h, length) result(res)
        integer, intent(in) :: h, length
        res = and(h, length - 1) + 1
    end

    ! ハッシュマップが空かどうか
    logical function is_empty(this) result(res)
        class(t_hash_map), intent(in) :: this
        res = this%size == 0
    end

    ! ハッシュマップに要素が1つ以上入っているかどうか
    logical function is_not_empty(this) result(res)
        class(t_hash_map), intent(in) :: this
        res = this%size /= 0
    end

    ! キーに対応する値を取り出す
    ! キーが存在しない場合は適当に0を返す
    integer function get(this, key) result(res)
        class(t_hash_map), intent(in) :: this
        integer, intent(in) :: key
        integer :: h
        type(t_entry), pointer :: e
        h = hash(hash_code(key))
        e => this%table(index_for(h, size(this%table)))%ref
        do while (associated(e))
            if (e%hash == h .and. e%key == key) then
                res = e%val
                return
            end if
            e => e%next
        end do
        res = 0
    end

    ! キーに対応する値を取り出す
    ! キーが存在しない場合はdefを返す
    integer function get_or_default(this, key, def) result(res)
        class(t_hash_map), intent(in) :: this
        integer, intent(in) :: key
        integer, intent(in) :: def
        integer :: h
        type(t_entry), pointer :: e
        h = hash(hash_code(key))
        e => this%table(index_for(h, size(this%table)))%ref
        do while (associated(e))
            if (e%hash == h .and. e%key == key) then
                res = e%val
                return
            end if
            e => e%next
        end do
        res = def
    end

    ! キーが存在するかどうか
    logical function contains_key(this, key) result(res)
        class(t_hash_map), intent(in) :: this
        integer, intent(in) :: key
        type(t_entry), pointer :: e
        e => get_entry(this, key)
        res = associated(e)
    end

    ! キーに対応するノードを取得する
    ! キーが存在しない場合はnullを返す
    function get_entry(this, key) result(res)
        class(t_hash_map), intent(in) :: this
        integer, intent(in) :: key
        integer :: h
        type(t_entry), pointer :: e
        type(t_entry), pointer :: res
        h = hash(hash_code(key))
        e => this%table(index_for(h, size(this%table)))%ref
        do while (associated(e))
            if (e%hash == h .and. e%key == key) then
                res => e
                return
            end if
            e => e%next
        end do
        res => null()
    end

    ! ハッシュマップにキーと値のペアを登録する
    ! すでにキーが存在する場合は値を新しい値に上書きする
    subroutine put(this, key, val)
        class(t_hash_map), intent(inout) :: this
        integer, intent(in) :: key
        integer, intent(in) :: val
        integer :: h, i
        type(t_entry), pointer :: e
        h = hash(hash_code(key))
        i = index_for(h, size(this%table))
        e => this%table(i)%ref
        do while (associated(e))
            if (e%hash == h .and. e%key == key) then ! すでにキーが存在する場合
                e%val = val
                return
            end if
            e => e%next
        end do
        call add_entry(this, key, val, h, i) ! ハッシュマップに新しいキーと値のペアを追加する
    end

    ! ハッシュマップに新しいキーと値のペアを追加する
    ! ハッシュが衝突した際の処理は連鎖法となっている。連鎖法については以下を参照。
    ! https://ja.wikipedia.org/wiki/%E3%83%8F%E3%83%83%E3%82%B7%E3%83%A5%E3%83%86%E3%83%BC%E3%83%96%E3%83%AB#%E9%80%A3%E9%8E%96%E6%B3%95
    subroutine add_entry(this, key, val, h, idx)
        class(t_hash_map), intent(inout) :: this
        integer, intent(in) :: key
        integer, intent(in) :: val
        integer, intent(in) :: h, idx
        type(t_entry), pointer :: e
        e => this%table(idx)%ref
        this%table(idx)%ref => new_entry(key, val, h)
        this%table(idx)%ref%next => e
        this%size = this%size + 1
        if (this%size >= this%threshold) call resize(this, 2*size(this%table))
    end

    ! tableがthreshold以上の個数の要素を保持している場合にtableを拡張する
    subroutine resize(this, new_capacity)
        class(t_hash_map), intent(inout) :: this
        integer, intent(in) :: new_capacity
        integer :: capacity, i, j
        type(t_entry), pointer :: e, next
        type(t_entry_ptr) :: table(new_capacity)
        capacity = size(this%table)
        if (capacity == maximum_capacity) then
            this%threshold = int_max_value
            return
        end if

        do j = 1, capacity
            e => this%table(j)%ref
            if (associated(e)) then
                this%table(j)%ref => null()
                do
                    next => e%next
                    i = index_for(e%hash, new_capacity)
                    e%next => table(i)%ref
                    table(i)%ref => e
                    e => next
                    if (.not. associated(e)) exit
                end do
            end if
        end do

        deallocate (this%table)
        allocate (this%table(new_capacity))
        do j = 1, new_capacity
            this%table(j)%ref => table(j)%ref
        end do
        this%threshold = int(new_capacity*this%load_factor)
    end

    ! キーに対応するノードを削除する
    ! キーが存在しない場合は何もしない
    subroutine remove(this, key)
        class(t_hash_map), intent(inout) :: this
        integer, intent(in) :: key
        integer :: h, i
        type(t_entry), pointer :: e, prev, next
        h = hash(hash_code(key))
        i = index_for(h, size(this%table))
        prev => this%table(i)%ref
        e => prev
        do while (associated(e))
            next => e%next
            if (e%hash == h .and. e%key == key) then
                this%size = this%size - 1
                if (associated(prev, e)) then
                    this%table(i)%ref => next
                else
                    prev%next => next
                end if
                return
            end if
            prev => e
            e => next
        end do
    end

    ! 全要素を削除する
    subroutine clear(this)
        class(t_hash_map), intent(inout) :: this
        deallocate (this%table)
        allocate (this%table(default_initial_capacity))
        this%size = 0
    end

    ! 対応する値がvalに等しいキーが存在するかどうか
    logical function contains_val(this, val) result(res)
        class(t_hash_map), intent(in) :: this
        integer, intent(in) :: val
        integer :: i
        type(t_entry), pointer :: e
        do i = 1, size(this%table)
            e => this%table(i)%ref
            do while (associated(e))
                if (e%val == val) then
                    res = .true.
                    return
                end if
                e => e%next
            end do
        end do
        res = .false.
    end
end module mod_hash_map

program ABC303c
    use mod_hash_map
    implicit none
    integer(4) N, M, H, K
    integer(4) x, y, xy_heal, xy_current
    integer(4) i
    character(:), allocatable:: S
    type(t_hash_map) :: map
    map = hash_map()

    !入力
    READ (*, *) N, M, H, K
    allocate (character(N) :: S)
    READ (*, *) S
    do i = 1, M
        read (*, *) x, y
        xy_heal = (x*10**5 + y)
        call put(map, xy_heal, 1)
        !write (*, *) xy
    end do

    !マス移動
    xy_heal = 0
    x = 0; y = 0
    do i = 1, N
        !移動
        select case (S(i:i))
        case ('R'); x = x + 1
        case ('L'); x = x - 1
        case ('U'); y = y + 1
        case ('D'); y = y - 1
        end select
        xy_heal = (x*10**5 + y)
        !write (*, *) xy

        !HP判定B
        H = H - 1
        if (H < 0) then
            write (*, "(a)") 'No'
            stop
        end if

        !回復判定
        if (H < K) then
            xy_current = map%get(xy_heal)
            !write (*, *) z
            if (xy_current == 1) then
                !write (*, *) H, '<', K
                H = K
                call map%remove(xy_heal)
            end if
        end if
    end do
    write (*, "(a)") 'Yes'
end

