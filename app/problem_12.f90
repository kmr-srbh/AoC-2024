program problem_12

    use aoc_utilities

    implicit none

    character(len=1),dimension(:,:),allocatable :: array
    integer :: isum, i, j, nrows, ncols, iarea, iperimeter
    logical,dimension(:,:),allocatable :: checked
    integer,dimension(:,:),allocatable :: price, price2
    integer,dimension(:,:,:),allocatable :: perimeter

    call clk%tic()

    ! array = read_file_to_char_array('inputs/day12_test.txt', border='.')
    array = read_file_to_char_array('inputs/day12.txt', border='.')
    nrows = size(array,1)
    ncols = size(array,2)
    allocate(checked(nrows, ncols)); checked = .false. ! to keep track of cells already checked
    allocate(price(nrows, ncols), price2(nrows, ncols)); price = 0; price2 = 0 ! to store the prices

    ! loop through each cell and recursively
    ! search to accumulate the area and perimeter
    do i = 1, nrows
        do j = 1, ncols
            iarea = 0
            iperimeter = 0 ! for part 1
            if (allocated(perimeter)) deallocate(perimeter)  ! start over each time
            call go(array(i,j),i,j)
            price(i,j) = iarea*iperimeter
            price2(i,j) = iarea*compute_n_sides()
        end do
    end do

    write(*,*) '12a:', sum(price)
    write(*,*) '12b:', sum(price2)

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
            if (.not. allocated(perimeter)) then
                allocate(perimeter(4, nrows, ncols)); perimeter = 0 ! 0 indicates no side found yet
            end if
            call check(c,i-1,j,perimeter(1,i,j)) ! check adjacent ones
            call check(c,i+1,j,perimeter(2,i,j))
            call check(c,i,j-1,perimeter(3,i,j))
            call check(c,i,j+1,perimeter(4,i,j))
        end subroutine go

        recursive subroutine check(c,i,j,p)
            !! check the adjacent one to either continue the search or update the perimeter
            character(len=1),intent(in) :: c !! the character we are searching for
            integer,intent(inout) :: p
            integer,intent(in) :: i,j
            if (array(i,j)==c) then
                if (.not. checked(i,j)) call go(c,i,j)  ! another element
            else
                iperimeter = iperimeter + 1  ! adjacent to another char, so it's part of the perimeter
                p = 1  ! mark this as a preliminary perimeter edge
            end if
        end subroutine check

        function compute_n_sides() result(iper)
            !! given the perimeter array, compute the part 2 perimeter (number of sides)
            integer :: iper
            integer :: i,j
            if (.not. allocated(perimeter)) then  ! no box found here
                iper = 0
                return
            end if
            ! compress adjacent sides so they are only counted once
            do i = 2, nrows-1
                do j = 2, ncols-1
                    ! mark the ones adjacent to another as deleted (-1)
                    if (abs(perimeter(1,i,j))==1 .and. abs(perimeter(1,i,j+1))==1) perimeter(1,i,j+1) = -1
                    if (abs(perimeter(2,i,j))==1 .and. abs(perimeter(2,i,j+1))==1) perimeter(2,i,j+1) = -1
                    if (abs(perimeter(3,i,j))==1 .and. abs(perimeter(3,i+1,j))==1) perimeter(3,i+1,j) = -1
                    if (abs(perimeter(4,i,j))==1 .and. abs(perimeter(4,i+1,j))==1) perimeter(4,i+1,j) = -1
                end do
            end do
            iper = count(perimeter==1) ! number of sides
        end function compute_n_sides

end program problem_12