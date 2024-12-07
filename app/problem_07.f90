program problem_07

use aoc_utilities

implicit none

integer :: iunit, n, i
character(len=:),allocatable :: line
integer(ip) :: isum, isum2, iresult, n_combos, ires, n_spaces, j
integer(ip),dimension(:),allocatable :: ivals
type(string),dimension(:),allocatable :: vals, vals2
integer,dimension(:),allocatable :: ioperators

integer,parameter :: PLUS = 0
integer,parameter :: TIMES = 1

call clk%tic()

open(newunit=iunit, file='inputs/day7.txt', status='OLD')
n = number_of_lines_in_file(iunit)
isum = 0
do i = 1, n
    ! 161011: 16 10 13
    line = read_line(iunit)
    vals = split(line,': ')
    iresult = vals(1)%to_int_64()  ! the results of the calculation
    vals2 = split(vals(2), ' ')
    ivals = vals2%to_int_64()      ! array of values to do the calculation

    !print*, iresult, ':', ivals

    n_spaces = size(ivals)-1
    n_combos = 2**(n_spaces) ! number of operator combinations
    !print*, n_combos
    if (allocated(ioperators)) deallocate(ioperators)
    allocate(ioperators(n_spaces)) !; ioperators = PLUS
    ! print*, 'n_spaces = ', n_spaces
    do j = 1, n_combos
        call permute(j)  ! permute for next one
        ires = evaluate()
        ! print*, '   ', ires
        if (ires == iresult) then
            !print*, '++++++found: ', ires, ' ops: ', ioperators
            !print*, ''
            ! this one works
            isum = isum + iresult
            exit  ! don't need to try any more (some can have more than one option)
        end if
    end do

end do


write(*,*) '7a:', isum


! write(*,*) '7b:', isum2

call clk%toc('7')

contains
    function evaluate() result(isum)
        ! evaluate, given the values and operators
        integer(ip) :: isum
        integer(ip) :: i
        isum = ivals(1)
        !print*, 'ioperators: ', ioperators
        do i = 2, size(ivals)
            select case (ioperators(i-1))
            case(PLUS);  isum = isum + ivals(i)
            case(TIMES); isum = isum * ivals(i)
            case default; error stop 'invalid operator'
            end select
        end do
    end function evaluate

    subroutine permute(icase)
        integer(ip),intent(in) :: icase ! generate this permutation of the operators

        !note: there's some way to do this with btest ???
        !      this way is using strings and it probably wildly inefficient

        ! this won't work for part 2 since there are now 3 operators ...

        integer :: k, kk

        character(64) :: gchar

        write(gchar,'(B64.64)') icase-1
        ! print*, '['//gchar//']', icase-1
        kk = 1
        do k=64,1,-1
            !print*, ' ->', gchar(k:k)
            if (gchar(k:k)=='1') then
                !print*, 'set', kk, ' to', TIMES
                ioperators(kk)=TIMES
            else
                !print*, 'set', kk, ' to', PLUS
                ioperators(kk)=PLUS
            end if
            kk = kk+1
            if (kk>n_spaces) exit
        end do
        ! print*, 'try: ', ioperators

    end subroutine permute

end program problem_07
