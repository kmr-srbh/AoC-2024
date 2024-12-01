program problem_1

use iso_fortran_env
use aoc_utilities

implicit none

integer :: iunit
logical :: status_ok
character(len=:),allocatable :: line
integer :: n_lines
integer :: i, j, isum
integer,dimension(:),allocatable :: a, b
type(string),dimension(:),allocatable :: vals

call clk%tic()

! -------- part 1 -----------
isum = 0
open(newunit=iunit, file='inputs/day1.txt', status='OLD')
n_lines = number_of_lines_in_file(iunit)
allocate(a(n_lines), b(n_lines))
do i = 1, n_lines
    line = read_line(iunit,status_ok)
    read(line, *) a(i), b(i)
end do
call sort(a)
call sort(b)
isum = 0
do i = 1, n_lines
    isum = isum + abs(b(i) - a(i))
end do
write(*,*) '1a: sum:', isum
close(iunit)

! -------- part 2 -----------
isum = 0
do i = 1, n_lines
    isum = isum + a(i)*count(a(i)==b)
end do
write(*,*) '1b: sum:', isum

call clk%toc('1')

end program problem_1