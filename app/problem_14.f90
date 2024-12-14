program problem_14

    use aoc_utilities

    implicit none

    integer :: iunit, n_lines
    integer :: i, j, icount, icount_max, icount_max_index
    character(len=:),allocatable :: line
    integer,dimension(:,:),allocatable :: r,v   ! x,y and vx,vy
    integer,dimension(:,:),allocatable :: rf    ! final x,y
    type(string),dimension(:),allocatable :: vals
    integer,dimension(4) :: quadrant_count
    integer,dimension(:,:),allocatable :: r_xmas    ! corrdinates for an x-mas tree image

    ! grid size:
    ! integer,parameter :: nrows = 7    ! for test
    ! integer,parameter :: ncols = 11
    integer,parameter :: nrows = 103
    integer,parameter :: ncols = 101

    call clk%tic()

    ! open(newunit=iunit, file='inputs/day14_test.txt', status='OLD')
    open(newunit=iunit, file='inputs/day14.txt', status='OLD')
    n_lines = number_of_lines_in_file(iunit)
    allocate(r(n_lines,2),v(n_lines,2), rf(n_lines,2))
    do i = 1, n_lines
        line = read_line(iunit)   ! p=9,5 v=-3,-3
        vals = split(line, ' ')
        r(i,:) = int(split(vals(1)%str(3:),','))+1    ! get initial state: let's make it 1-based index
        v(i,:) = int(split(vals(2)%str(3:),','))
        rf(i,:) = advance(r(i,:),v(i,:),100)
    end do
    close(iunit)
    quadrant_count(1) = count(rf(:,1)>=1         .and. rf(:,1)<=ncols/2 .and. rf(:,2)>=1         .and. rf(:,2)<=nrows/2)
    quadrant_count(2) = count(rf(:,1)>=ncols/2+2 .and. rf(:,1)<=ncols   .and. rf(:,2)>=1         .and. rf(:,2)<=nrows/2)
    quadrant_count(3) = count(rf(:,1)>=1         .and. rf(:,1)<=ncols/2 .and. rf(:,2)>=nrows/2+2 .and. rf(:,2)<=nrows)
    quadrant_count(4) = count(rf(:,1)>=ncols/2+2 .and. rf(:,1)<=ncols   .and. rf(:,2)>=nrows/2+2 .and. rf(:,2)<=nrows)

    write(*,*) '14a:', product(quadrant_count)

    ! part 2
    ! just run until we max out the number of contiguous horizontal lines
    icount_max = -1
    icount_max_index = -1
    do j = 1, 10000   ! upper bound is arbitrary
        do i = 1, n_lines
            r(i,:) = advance(r(i,:),v(i,:),1)  ! advance by one time step
        end do
        icount = count_contiguous_horiz_lines(r)
        if (icount > icount_max) then ! a new best
            icount_max = icount
            icount_max_index = j
            call print_board(r)
        end if
    end do
    write(*,*) '14b:', icount_max_index

    call clk%toc('14')

    contains

        pure function advance(r,v,t) result (rf)
            ! rf = r + v*t with wrapping
            !
            !         cols
            !       o-->x
            ! rows  |
            !       V
            !       y
            integer,dimension(2),intent(in) :: r,v ! current position,velocity
            integer,intent(in) :: t  ! time step
            integer,dimension(2) :: rf ! final position
            rf = modulo((r + v*t)-1,[ncols,nrows])+1 ! [note: assuming 1-based indices here]
        end function advance

        subroutine print_board(r)
            integer,dimension(:,:),intent(in) :: r ! x,y coordinates
            integer :: i
            character,dimension(:,:),allocatable :: map
            allocate(map(nrows, ncols)); map = ''
            do i = 1, size(r,1)
                map(r(i,2), r(i,1)) = '#'
            end do
            do i = 1, nrows
                write(*, '(*(a1))') map(i,:)
            end do
        end subroutine print_board

        function count_contiguous_horiz_lines(r) result(icount)
            !! the idea here is to count the number of elements
            !! that are part of contiguous horizontal lines
            !! a xmas tree should have a lot of those...
            integer,dimension(:,:),intent(in) :: r ! x,y coordinates
            integer :: icount
            integer :: i, j
            logical,dimension(:,:),allocatable :: map
            allocate(map(nrows, ncols)); map = .false.
            do i = 1, size(r,1)
                map(r(i,2), r(i,1)) = .true.
            end do
            icount = 0
            do i = 1+1, nrows
                do j = 1+1, ncols
                    if (map(i, j) .and. map(i,j-1)) icount = icount+1
                end do
            end do
        end function count_contiguous_horiz_lines


end program problem_14