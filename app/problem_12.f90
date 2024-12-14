program problem_12

    use aoc_utilities

    implicit none

    character(len=1),dimension(:,:),allocatable :: array
    integer :: isum, i, j, nrows, ncols, iarea, iperimeter
    logical,dimension(:,:),allocatable :: checked
    integer,dimension(:,:),allocatable :: price

    call clk%tic()

    ! array = read_file_to_char_array('inputs/day12_test.txt', border='.')
    array = read_file_to_char_array('inputs/day12.txt', border='.')
    nrows = size(array,1)
    ncols = size(array,2)
    allocate(checked(nrows, ncols)); checked = .false. ! to keep track of cells already checked
    allocate(price(nrows, ncols)); price = 0 ! to store the prices

    ! loop through each cell and recursively
    ! search to accumulate the area and perimeter
    do i = 1, nrows
        do j = 1, ncols
            iarea = 0
            iperimeter = 0
            call go(array(i,j),i,j)
            price(i,j) = iarea*iperimeter
        end do
    end do

    write(*,*) '12a:', sum(price)

    ! write(*,*) '12b:', isum

    call clk%toc('12')

    contains

        recursive subroutine go(c,i,j)
            ! recursive search of the cells
            character(len=1),intent(in) :: c !! the character we are searching for
            integer,intent(in) :: i,j
            if (array(i,j)=='.') return
            if (array(i,j)/=c) return
            if (checked(i,j)) return  ! if already checked
            checked(i,j) = .true.  ! found a new one
            iarea = iarea + 1  ! area is just a count of the ones with the same char
            call check(c,i-1,j) ! check adjacent ones
            call check(c,i+1,j)
            call check(c,i,j-1)
            call check(c,i,j+1)
        end subroutine go

        recursive subroutine check(c,i,j)
            ! check the adjacent one to either continue the search or update the perimeter
            character(len=1),intent(in) :: c !! the charactder we are searching for
            integer,intent(in) :: i,j
            if (array(i,j)==c) then
                if (.not. checked(i,j)) call go(c,i,j)  ! another element
            else
                iperimeter = iperimeter + 1  ! adjacent to another char, so it's part of the perimeter
            end if
        end subroutine check

end program problem_12