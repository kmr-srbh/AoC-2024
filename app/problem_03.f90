program problem_3

use iso_fortran_env
use aoc_utilities

implicit none

integer(ip):: iunit, i, j, k, m, isum, n, op, cl
character(len=:),allocatable :: line, str
type(string),dimension(:),allocatable:: vals
integer(ip) :: istat1, istat2, i1, i2

call clk%tic()

line = read_file_to_string('inputs/day3.txt')

isum = 0
k = 1
do
    ! print*, 'k= ', k
    ! k
    ! 123mul(111,222)
    !    |          |
    !   op          |
    !               cl

    ! find next mul:
    op = index(line(k:), 'mul(')
    if (op<1) exit ! done
    op = k + op - 1 ! convert to abs index

    ! find next ) to enclose values:
    cl = index(line(op:), ')')
    if (cl<1) exit ! done
    cl = op + cl - 1  ! convert to abs index

    str = line(op+4:cl-1)
    ! print*, 'str= '//str
    if (len(str)<2) then
        !must be at least x,x
        k = op+4  !cl+1 ! set up for next loop
        cycle
    end if
    if (index(str,',')<1) then
        ! no comma
        k = op+4  !cl+1  ! set up for next loop
        cycle
    end if
    ! now, split the values from str:
    vals = split(str, ',')
    !print*, vals(1)%str, vals(2)%str
    if (size(vals)<1 .or. size(vals)>2) then
        ! there may be a value packet in there somewhere
        k = op+4  ! set up for next loop
        cycle
    end if
    ! note: the reads will still work with extra spaces... so check for that after the fact...
    read(vals(1)%str, *, iostat=istat1) i1
    read(vals(2)%str, *, iostat=istat2) i2
    if (istat1/=0 .or. istat2/=0 .or. index(vals(1)%str, ' ')>0 .or. index(vals(2)%str, ' ')>0) then
        k = op+4  ! set up for next loop
        cycle
    end if

    ! success
    isum = isum + i1*i2
    k = cl+1  ! set up for next loop
    if (k>=len(line)) exit

end do
write(*,*) '3a:', isum




call clk%toc('3')



end program problem_3