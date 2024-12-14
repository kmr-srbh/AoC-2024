program problem_14

    use aoc_utilities

    implicit none

    integer :: i, iunit, n_lines
    character(len=:),allocatable :: line
    integer,dimension(:,:),allocatable :: r,v   ! x,y and vx,vy
    integer,dimension(:,:),allocatable :: rf    ! final x,y
    type(string),dimension(:),allocatable :: vals
    integer,dimension(4) :: quadrant_count

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
        r(i,:) = int(split(vals(1)%str(3:),','))+1    ! let's make it 1-based index
        v(i,:) = int(split(vals(2)%str(3:),','))

        !         cols
        !       o-->x
        ! rows  |
        !       V
        !       y

        ! rf = r + v*t
        rf(i,1) = modulo((r(i,1) + v(i,1) * 100)-1,ncols)+1 ! x final position
        rf(i,2) = modulo((r(i,2) + v(i,2) * 100)-1,nrows)+1 ! y final position
    end do
    close(iunit)

    quadrant_count(1) = count(rf(:,1)>=1         .and. rf(:,1)<=ncols/2 .and. rf(:,2)>=1         .and. rf(:,2)<=nrows/2)
    quadrant_count(2) = count(rf(:,1)>=ncols/2+2 .and. rf(:,1)<=ncols   .and. rf(:,2)>=1         .and. rf(:,2)<=nrows/2)
    quadrant_count(3) = count(rf(:,1)>=1         .and. rf(:,1)<=ncols/2 .and. rf(:,2)>=nrows/2+2 .and. rf(:,2)<=nrows)
    quadrant_count(4) = count(rf(:,1)>=ncols/2+2 .and. rf(:,1)<=ncols   .and. rf(:,2)>=nrows/2+2 .and. rf(:,2)<=nrows)

    write(*,*) '14a:', product(quadrant_count)

    ! write(*,*) '14b:', isum

    call clk%toc('14')

end program problem_14