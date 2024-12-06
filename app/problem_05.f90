program problem_05

use aoc_utilities

implicit none

integer :: iunit, n, i, i1, i2, isum, n_elements, j, k, isum2
character(len=:),allocatable :: line
logical :: header_done
character(len=1) :: c
integer,dimension(:),allocatable :: i1_array, i2_array, ilist, ilist_sorted
type(string),dimension(:),allocatable :: vals
logical :: updated

call clk%tic()

! open(newunit=iunit, file='inputs/day5_test.txt', status='OLD')
open(newunit=iunit, file='inputs/day5.txt', status='OLD')

isum = 0
isum2 = 0
n_elements = 0
n = number_of_lines_in_file(iunit)
allocate(i1_array(0), i2_array(0))
header_done = .false.  ! start process the header (the dependencies)
do i = 1, n
    line = read_line(iunit)
    if (line=='') then
        header_done = .true.
    else

        if (header_done) then ! process a list
            ! 75,47,61,53,29
            vals = split(line, ',')
            if (allocated(ilist)) deallocate(ilist)
            allocate(ilist(size(vals)))
            do j = 1, size(vals)
                ilist(j) = vals(j)%to_int()
            end do
            ! bubble sort!
            ilist_sorted = ilist
            do
                updated = .false.
                do j = 1, size(ilist)-1
                    ! compare these two
                    do k = 1, size(i1_array)
                        if (i2_array(k)==ilist_sorted(j) .and. i1_array(k)==ilist_sorted(j+1)) then
                            call swap(ilist_sorted(j), ilist_sorted(j+1))
                            updated = .true.
                        end if
                    end do
                end do
                if (.not. updated) exit  ! done, they are all sorted
            end do
            ! compare to original list to see if it was properly sorted
            ! get middle index and add:
            if (all((ilist-ilist_sorted)==0)) then
                isum = isum + ilist(size(ilist)/2+1)
            else
                ! ones that weren't propertly sorted for part 2
                isum2 = isum2 + ilist_sorted(size(ilist_sorted)/2+1)
            end if
        else
            ! 47|53
            read(line, '(I2,A1,I2)') i1, c, i2
            i1_array = [i1_array, i1]  ! accumulate the dependencies in arrays
            i2_array = [i2_array, i2]
        end if
    end if

end do

close(iunit)

write(*,*) '5a:', isum
write(*,*) '5b:', isum2

call clk%toc('5')

end program problem_05