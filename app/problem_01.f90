program problem_1

use iso_fortran_env
use aoc_utilities

implicit none

integer :: iunit
logical :: status_ok
character(len=:),allocatable :: line
integer :: n_lines
integer :: i, j, k, n, isum, ifirst, ilast, idxfirst, idxlast

character(len=*),dimension(9),parameter :: ichars = [ &
    'one  ', &
    'two  ', &
    'three', &
    'four ', &
    'five ', &
    'six  ', &
    'seven', &
    'eight', &
    'nine ']

call clk%tic()

isum = 0
open(newunit=iunit, file='inputs/day1.txt', status='OLD')
n_lines = number_of_lines_in_file(iunit)
do i = 1, n_lines

    ! remove all the chars before the first and after the last number
    line = read_line(iunit,status_ok)
    n = len(line)

end do
write(*,*) '1a: sum:', isum
close(iunit)


! -------- part 2 -----------

open(newunit=iunit, file='inputs/day1.txt', status='OLD')
n_lines = number_of_lines_in_file(iunit)
isum = 0
do i = 1, n_lines

    line = read_line(iunit,status_ok)
    n = len(line)


end do
write(*,*) '1b: sum:', isum

call clk%toc('1')

end program problem_1