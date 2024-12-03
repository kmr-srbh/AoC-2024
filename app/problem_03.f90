program problem_3

use iso_fortran_env
use aoc_utilities

implicit none

integer(ip):: isum
character(len=:),allocatable :: line
integer :: idont, ido

call clk%tic()

line = read_file_to_string('inputs/day3.txt')

! part 1 is the full line:
isum = compute(line)
write(*,*) '3a:', isum

! for part 2, we just look for the don't() and do()
! and process chunks of the line accordingly.
! it starts in do() mode
isum = 0
do
    ! sum from start until the next don't()
    idont = index(line, 'don''t()')
    if (idont<1) then
        isum = isum + compute(line) ! sum until the end
        exit
    end if
    isum = isum + compute(line(:idont-1))
    line = line(idont+6:) ! cut out the part already summed
    ! go to next do or the end
    ido = index(line, 'do()')
    if (ido<1) exit ! done
    line = line(ido+4:) ! cut out the don't part until the next do
end do
write(*,*) '3b:', isum

call clk%toc('3')

contains

    function compute(line) result(isum)
        !! compute the sum for the line
        character(len=*),intent(in) :: line
        integer :: isum

        integer(ip):: i, j, k, m, n, op, cl
        character(len=:),allocatable :: str
        type(string),dimension(:),allocatable:: vals
        integer(ip) :: istat1, istat2, i1, i2

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

    end function compute

end program problem_3