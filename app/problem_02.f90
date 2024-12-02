program problem_2

use aoc_utilities

implicit none

integer :: iunit, i, j, isum, n, del, isum2
logical :: status_ok
character(len=:),allocatable :: line
integer,dimension(:),allocatable :: ints, ints_tmp

call clk%tic()

open(newunit=iunit, file='inputs/day2.txt', status='OLD')
n = number_of_lines_in_file(iunit)

isum = 0
isum2 = 0
do i = 1, n
    line = read_line(iunit,status_ok)
    ints = parse_ints(line)
    if (safe(ints)) then ! already safe
        isum = isum + 1
        isum2 = isum2 + 1
    else
        !part 2:
        do j = 1, size(ints)
            ! is it safe with jth element removed?
            ints_tmp = [ints(1:j-1), ints(j+1:size(ints))]
            if (safe(ints_tmp)) then
                isum2 = isum2 + 1
                exit
            end if
        end do
    end if
end do
close(iunit)

write(*,*) '2a:', isum
write(*,*) '2b:', isum2
call clk%toc('2')

contains

    logical function safe(ints)
        !! is it safe?

        integer,dimension(:),intent(in) :: ints

        integer :: j
        logical :: increasing, decreasing, diffcheck

        increasing = .true.
        decreasing = .true.
        diffcheck = .true.
        do j = 1, size(ints)-1
            ! The levels are either all increasing or all decreasing.
            if (ints(j+1) > ints(j)) decreasing = .false.
            if (ints(j+1) < ints(j)) increasing = .false.
            ! Any two adjacent levels differ by at least one and at most three.
            del = abs(ints(j+1) - ints(j))
            if (del==0 .or. del>3) diffcheck = .false.
            safe = ((increasing .or. decreasing) .and. diffcheck)
            if (.not. safe) exit
        end do

    end function safe

end program problem_2