program problem_06

use aoc_utilities

implicit none

integer :: iunit, isum, i, j, inew, jnew, isum2, iobs, jobs, istart, jstart
character(len=1),dimension(:,:),allocatable :: array
integer,dimension(:,:),allocatable :: idirections
integer :: direction, direction_start
logical :: loop

call clk%tic()

! add a space as a border
array = read_file_to_char_array('inputs/day6.txt', '+')
! array = read_file_to_char_array('inputs/day6_test.txt', '+')

allocate(idirections(1:size(array,1), 1:size(array,2)))
idirections = 0
main : do i = 1, size(array,1)  ! row
    do j = 1, size(array,2)     ! col
        if (any(array(i,j)==['^','v','>','<'])) exit main
    end do
end do main
istart = i
jstart = j
!current position is istart,jstart
select case (array(istart,jstart))
case('^'); direction = 1 ! up
case('v'); direction = 2 ! down
case('>'); direction = 3 ! right
case('<'); direction = 4 ! left
end select
direction_start = direction  ! starting direction

! part 1
call run(isum, loop)
write(*,*) '6a:', isum

! for part 2, just try all the possible locations of the obstruction
isum2 = 0
do iobs = 1, size(array,1)  ! row
    do jobs = 1, size(array,2)  ! col
        if (array(iobs,jobs)=='.') then
            ! try an obstruction here
            direction = direction_start ! reset back to how it started
            call run(isum, loop, iobs, jobs)
            if (loop) isum2 = isum2 + 1
        end if
    end do
end do
write(*,*) '6b:', isum2

call clk%toc('6')

contains

    subroutine run(isum, loop, iobs, jobs)
        integer,intent(out) :: isum
        logical,intent(out) :: loop !! if we entered a loop
        integer,intent(in),optional :: iobs, jobs !! where to put the obstruction

        character(len=1),dimension(:,:),allocatable :: a
        integer :: i,j

        a = array
        i = istart
        j = jstart
        loop = .false.
        if (present(iobs)) a(iobs, jobs) = '#'
        a(i,j) = 'X' ! mark this square
        idirections(i,j) = direction ! record how we first got here [for part 2]
        isum = 1
        do

            select case (direction)
            case(1); inew = i-1; jnew = j
            case(2); inew = i+1; jnew = j
            case(3); inew = i;   jnew = j+1
            case(4); inew = i;   jnew = j-1
            end select
            if (a(inew,jnew)=='+') exit ! done
            if (a(inew,jnew)=='#') then
                call rotate()
                cycle
            end if
            !move to the new location
            i = inew
            j = jnew
            if (a(i,j)/='X') then
                isum = isum + 1 ! if we haven't already visited it
                idirections(i,j) = direction ! record how we first got here [for part 2]
                a(i,j) = 'X' ! mark this square
            else
                ! have we been here before in the same direction?
                loop = idirections(i,j) == direction
                if (loop) return
                idirections(i,j) = direction  ! update... we want the last time it was here
            end if
        end do

    end subroutine run

    subroutine rotate()
        select case (direction)
        case(1); direction = 3 ! up    -> right
        case(2); direction = 4 ! down  -> left
        case(3); direction = 2 ! right -> down
        case(4); direction = 1 ! left  -> up
        end select
    end subroutine rotate

end program problem_06
