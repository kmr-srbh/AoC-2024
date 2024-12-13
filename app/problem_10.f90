program problem_10

    use aoc_utilities

    implicit none

    integer,dimension(:,:),allocatable :: array
    integer :: it, k, isum
    integer,dimension(:),allocatable :: itraillhead, jtraillhead, scores, ratings
    integer,dimension(:),allocatable :: iends_found, jends_found

    call clk%tic()

    ! array = read_file_to_int_array('inputs/day10_test.txt')
    array = read_file_to_int_array('inputs/day10.txt')

    ! first get the trailhead indices:
    itraillhead = pack(spread([(k, k = 1, size(array,1))], dim=2, ncopies=size(array,2)), mask=array==0)
    jtraillhead = pack(spread([(k, k = 1, size(array,2))], dim=1, ncopies=size(array,1)), mask=array==0)
    allocate(scores(size(itraillhead))); scores = 0
    allocate(ratings(size(itraillhead))); ratings = 0

    do it = 1, size(jtraillhead)
        isum = 0
        if (allocated(iends_found)) deallocate(iends_found)
        if (allocated(jends_found)) deallocate(jends_found)
        allocate(iends_found(0), jends_found(0)) ! keep a list of the end points reached
        call search(itraillhead(it), jtraillhead(it), -1)
        scores(it) = size(iends_found)
        ratings(it) = isum
    end do

    write(*,*) '10a:', sum(scores)
    write(*,*) '10b:', sum(ratings)

    call clk%toc('10')

    contains

        recursive subroutine search(i,j,prev_value)
            integer,intent(in) :: i,j
            integer,intent(in) :: prev_value  !! the value from the previous step
            if (array(i,j)==prev_value+1) then
                if (array(i,j)==9) then
                    isum = isum + 1  ! for ratings, consider all possible paths to 9
                    if (.not. any(iends_found==i .and. jends_found==j)) then
                        ! for score, only need to count how many 9's are reached
                        iends_found = [iends_found, i]
                        jends_found = [jends_found, j]
                    end if
                else
                    if (i>1)             call search(i-1,j,array(i,j))
                    if (i<size(array,1)) call search(i+1,j,array(i,j))
                    if (j>1)             call search(i,j-1,array(i,j))
                    if (j<size(array,2)) call search(i,j+1,array(i,j))
                end if
            end if
        end subroutine search

end program problem_10
