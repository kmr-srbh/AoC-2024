program problem_06

use aoc_utilities

implicit none

integer :: isum, i, j, inew, jnew, isum2, iobs, jobs, istart, jstart
character(len=1),dimension(:,:),allocatable :: array
integer,dimension(:,:),allocatable :: idirections
integer :: direction, direction_start
logical :: loop
character(len=1),dimension(:,:),allocatable :: a, array_original_marked ! temp arrays

character(len=1),parameter :: BORDER      = '+'  ! border just to make the end check easier
character(len=1),parameter :: OBSTRUCTION = '#'
character(len=1),parameter :: EMPTY       = '.'
character(len=1),parameter :: MARKED      = 'X'
character(len=1),dimension(*),parameter :: DIRECTIONS = ['^','>','v','<'] ! in the rotation order (see rotate)
integer,parameter :: UP    = 1 ! the indices in the above vector
integer,parameter :: RIGHT = 2
integer,parameter :: DOWN  = 3
integer,parameter :: LEFT  = 4

call clk%tic()

! add a space as a border
array = read_file_to_char_array('inputs/day6.txt', BORDER)
! array = read_file_to_char_array('inputs/day6_test.txt', BORDER)

allocate(idirections(size(array,1), size(array,2)))
idirections = 0
main : do i = 1, size(array,1)  ! row
    do j = 1, size(array,2)     ! col
        if (any(array(i,j)==DIRECTIONS)) exit main
    end do
end do main
istart = i; jstart = j ! current position is istart,jstart
! get direction from the char (index in this array):
direction = findloc(DIRECTIONS, array(istart,jstart), dim=1)
direction_start = direction  ! starting direction

! --- part 1
call run(isum, loop)
write(*,*) '6a:', isum

! --- part 2
array_original_marked = a ! save this - we only need to try obstructions on the places we visited in part 1 !
array_original_marked(istart, jstart) = array(istart, jstart) ! restore initial marker
isum2 = 0
do iobs = 1, size(array,1)  ! row
    do jobs = 1, size(array,2)  ! col
        if (array_original_marked(iobs,jobs)==MARKED) then ! if it was visited in part 1
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

        integer :: i,j

        a = array
        i = istart
        j = jstart
        loop = .false.
        if (present(iobs)) a(iobs, jobs) = OBSTRUCTION ! add the obstruction here
        a(i,j) = MARKED ! mark first square
        isum = 1
        idirections(i,j) = direction ! record how we first got here [for part 2]
        do
            ! square to move to, given current direction
            select case (direction)
            case(UP)   ; inew = i-1; jnew = j
            case(DOWN) ; inew = i+1; jnew = j
            case(RIGHT); inew = i;   jnew = j+1
            case(LEFT) ; inew = i;   jnew = j-1
            end select
            if (a(inew,jnew)==BORDER) exit ! done
            if (a(inew,jnew)==OBSTRUCTION) then
                ! have to rotate and try again
                call rotate()
                cycle
            end if
            !move to the new location
            i = inew
            j = jnew
            if (a(i,j)/=MARKED) then
                isum = isum + 1 ! if we haven't already visited it
                idirections(i,j) = direction ! record how we first got here [for part 2]
                a(i,j) = MARKED ! mark this square
            else
                ! have we been here before in the same direction?
                loop = idirections(i,j) == direction
                if (loop) return
                idirections(i,j) = direction  ! update... we want the last time it was here
            end if
        end do

    end subroutine run

    subroutine rotate()
        direction = mod(direction,4)+1  ! 1,2,3,4
    end subroutine rotate

end program problem_06
