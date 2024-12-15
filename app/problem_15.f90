program problem_15

    use aoc_utilities

    implicit none

    integer :: iunit, i, j, k, n_lines, nrows, ncols, isum
    character(len=:),allocatable :: line
    type(string),dimension(:),allocatable :: board_strs !! array of strings for the board
    logical :: reading_moves
    character(len=:),allocatable :: moves
    character(len=1),dimension(:,:),allocatable,target :: array
    integer,dimension(2) :: iloc
    integer,dimension(:),allocatable :: ibox, jbox

    character(len=1),parameter :: SPACE = '.'
    character(len=1),parameter :: FISH  = '@'
    character(len=1),parameter :: WALL  = '#'
    character(len=1),parameter :: BOX   = 'O'

    call clk%tic()

    ! load file and parse the inputs:
    ! open(newunit=iunit, file='inputs/day15_test_2.txt', status='OLD')
    open(newunit=iunit, file='inputs/day15.txt', status='OLD')
    n_lines = number_of_lines_in_file(iunit)
    reading_moves = .false.
    allocate(board_strs(0))
    moves = ''
    do i = 1, n_lines
        line = read_line(iunit)
        if (line=='') then
            reading_moves = .true.
            cycle
        end if
        if (reading_moves) then ! read moves into single string
            moves = moves // line
        else  ! accumulte the board as a list of strings first
            board_strs = [board_strs, string(line)]
        end if
    end do
    close(iunit)
    ! convert board to normal char array:
    nrows = size(board_strs)
    ncols = len(board_strs(1)%str)
    allocate(array(nrows,ncols))
    do i = 1, nrows
        array(i,:) = [(board_strs(i)%str(j:j), j = 1, ncols)]
    end do
    iloc = findloc(array, FISH) ! find the fish

    ! do all the moves:
    i = iloc(1); j = iloc(2)  ! staring position of fish
    do k = 1, len(moves)
        call advance(i,j,moves(k:k))
        !call print_board()
    end do

    ibox = pack(spread([(k, k = 1, size(array,1))], dim=2, ncopies=size(array,2)), mask=array==BOX) ! indices of all the boxes
    jbox = pack(spread([(k, k = 1, size(array,2))], dim=1, ncopies=size(array,1)), mask=array==BOX)
    isum = sum(100*(ibox-1) + (jbox-1)) ! gps sum

    write(*,'(a,i20)')   '15a:', isum
    !write(*,'(a,f20.0)') '15b:' !, isum2

    call clk%toc('15')

contains

    subroutine advance(i,j,dir)
        !! advance one step
        integer,intent(inout) :: i,j
        character(len=1),intent(in) :: dir !! direction
        select case (dir)
        case('^'); call move_fish(i,j,i-1,j) ! up
        case('v'); call move_fish(i,j,i+1,j) ! down
        case('<'); call move_fish(i,j,i,j-1) ! left
        case('>'); call move_fish(i,j,i,j+1) ! right
        case default; error stop 'error: invalid direction: '//dir
        end select
    end subroutine advance

    subroutine move_fish(i,j,inew,jnew)
        ! potentially make a move from i,j to inew,jnew. update the board and i,j.
        integer,intent(inout) :: i,j ! input: current position, output: new position [of the fish]
        integer,intent(in) :: inew,jnew ! position to potentially move to

        character(len=1),pointer :: c ! character in the way
        integer :: itmp, jtmp ! for searching for the end of the boxes

        c => array(inew,jnew) ! a pointer
        if (c==WALL) then
            return  ! can't move
        else if (c==SPACE) then
            ! move into the space
            array(i,j) = SPACE ! move the fish
            c = FISH
            i = inew; j = jnew
        elseif (c==BOX) then
            !is there an empty space past this box, or block of boxes?
            !
            !1! #
            !2! .      <-itmp,jtmp
            !3! O
            !4! O      <-inew,jnew
            !5! @  ^   <-i,j
            itmp = inew; jtmp = jnew
            do
                itmp = itmp + (inew-i)  ! note: only one of them is really changing
                jtmp = jtmp + (jnew-j)
                if (array(itmp,jtmp)==SPACE) then
                    ! we can move these
                    array(i,j)       = SPACE ! move the fish
                    c                = FISH
                    array(itmp,jtmp) = BOX   ! put box in the last space
                    i = inew; j = jnew
                    exit
                else if (array(itmp,jtmp)==WALL) then
                    exit  ! can't move anything
                else if (array(itmp,jtmp)==BOX) then
                    ! keep searching
                end if
            end do
        else
            error stop 'invalid character: '//c
        end if
    end subroutine move_fish

    subroutine print_board()
        integer :: i
        do i = 1, size(array,1)
            print*, array(i,:)
        end do
        print*, ''
    end subroutine print_board

end program problem_15