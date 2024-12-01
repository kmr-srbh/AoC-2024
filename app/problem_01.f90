program problem_1

use iso_fortran_env
use aoc_utilities

implicit none

integer :: iunit, i, isum, n
logical :: status_ok
character(len=:),allocatable :: line
integer,dimension(:),allocatable :: a, b  ! the arrays

call clk%tic()

open(newunit=iunit, file='inputs/day1.txt', status='OLD')
n = number_of_lines_in_file(iunit)
allocate(a(n), b(n))
do i = 1, n
    line = read_line(iunit,status_ok)
    read(line, *) a(i), b(i)
end do
close(iunit)

! -------- part 1 -----------
call sort(a)
call sort(b)
isum = sum(abs(b - a))
write(*,*) '1a: sum:', isum

! -------- part 2 -----------
isum = 0
do i = 1, n
    isum = isum + a(i)*count(a(i)==b)
end do
write(*,*) '1b: sum:', isum

call clk%toc('1')

end program problem_1