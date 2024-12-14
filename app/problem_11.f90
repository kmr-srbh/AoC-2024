program problem_11

    use aoc_utilities

    implicit none

    integer(ip),dimension(:),allocatable :: array
    integer :: iunit
    integer(ip) :: i

    type :: stone
        integer(ip) :: value = 0 !! the value of this stone
        integer(ip) :: count = 0  !! the number of times this stone appears
    end type stone
    type(stone),dimension(:),allocatable :: unique_stones

    call clk%tic()

    ! open(newunit=iunit, file='inputs/day11_test.txt', status='OLD')
    open(newunit=iunit, file='inputs/day11.txt', status='OLD')
    array = int(split(trim(adjustl(read_line(iunit))), ' '))
    close(iunit)

    unique_stones = [(stone(array(i), 1), i = 1, size(array))] ! initialize

    do i = 1, 25
        unique_stones = blink(unique_stones)
    end do
    write(*,*) '11a:', sum(unique_stones%count)

    do i = 26, 75
        unique_stones = blink(unique_stones)
    end do
    write(*,*) '11b:', sum(unique_stones%count)

    call clk%toc('11')

    contains

        function blink(stones) result(stones_new)
        !! blink and update the list
        type(stone),dimension(:),allocatable,intent(in) :: stones
        type(stone),dimension(:),allocatable :: stones_new

        integer(ip) :: n, itop, ibottom
        integer(ip) :: i

        allocate(stones_new(0))

        do i = 1_ip, size(stones)
            associate(val => stones(i)%value, icount => stones(i)%count)
                if (val==0) then !0 -> 1
                    call put(stones_new, 1_ip, icount)
                else
                    n = num_digits(val)
                    if (mod(n,2_ip)==0) then
                        n = n/2_ip
                        ! split number: aaabbb -> aaa bbb
                        itop    = floor(real(val, wp)/10_ip**n,ip)
                        ibottom = val - itop*10_ip**n
                        call put(stones_new, itop, icount)
                        call put(stones_new, ibottom, icount)
                    else
                        call put(stones_new, val*2024_ip, icount)
                    end if
                end if
            end associate
        end do

    end function blink

    subroutine put(s, val, icount)
        !! put the stone value into the list
        type(stone),dimension(:),allocatable,intent(inout) :: s !! stones to update
        integer(ip),intent(in) :: val
        integer(ip),intent(in) :: icount !! the current count of val
        integer(ip) :: iloc
        iloc = findloc(s%value, val, dim=1) ! is it already there?
        if (iloc==0) then
            s = [s, stone(val, icount)] ! add a new one
        else
            s(iloc)%count = s(iloc)%count + icount  ! increment the count of existing one
        end if
    end subroutine put

end program problem_11

