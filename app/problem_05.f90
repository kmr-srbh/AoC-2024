program problem_5

use iso_fortran_env
use aoc_utilities
use dag_module

implicit none

integer :: iunit, n, i, i1, i2, isum, n_elements, j, istat
character(len=:),allocatable :: line
logical :: header_done
character(len=1) :: c
integer,dimension(:),allocatable :: i1_array, i2_array, ilist, ilist_sorted
type(dag) :: d
integer,dimension(:),allocatable :: order
type(string),dimension(:),allocatable :: vals

call clk%tic()

open(newunit=iunit, file='inputs/day5_test.txt', status='OLD')
! open(newunit=iunit, file='inputs/day5.txt', status='OLD')

isum = 0
n_elements = 0
n = number_of_lines_in_file(iunit)
allocate(i1_array(0), i2_array(0))
header_done = .false.  ! start process the header (the dependencies)
do i = 1, n
    line = read_line(iunit)
    !print*, line
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
            ! print*, ilist

            ! try another option....
            !... maybe extract only the ones from the full list that apply to the ones in this line ?

            ! for all the elements of ilist, extract from i1_array,i2_array all the connected elements


            if (.false.) then

                if (n_elements==0) then
                    ! toposort the dependencies:
                    ! The first section specifies the page ordering rules, one per line. The first rule, 47|53, means that if an update includes both page number 47 and page number 53, then page number 47 must be printed at some point before page number 53. (47 doesn't necessarily need to be immediately before 53; other pages are allowed to be between them.)
                    n_elements = max(maxval(i1_array), maxval(i2_array))  ! max number in the set

                    ! print*, 'min i1 = ', minval(i1_array)
                    ! print*, 'min i2 = ', minval(i2_array)
                    ! print*, 'max i1 = ', maxval(i1_array)
                    ! print*, 'max i2 = ', maxval(i2_array)

                    !print*, 'max num = ', n_elements
                    call d%set_vertices(n_elements)
                    do j = 1, size(i1_array)
                        call d%add_edge(ivertex=i2_array(j),iedge=i1_array(j))  ! 2 depends on 1
                    end do
                    !call save_plot('day5_plot')

                    call d%toposort(order,istat)
                    print*, 'istat=',istat
                    if (istat/=0) error stop 'error in toposort'
                    ! order is a valid order
                end if

                !.... this approach doesn't work because there is some cycle in the dependencies!
                !     so it can't be toposorted ! ARG !
                !
                ! check if this line is valid by comparison to the toposorted list
                !... the problem is the order might not be unique... so we don't want to exclude ones that have
                !    a different order for elements not specified...... ug.....
                ! .... depends on if all the nodes aren't specified in the list?
                !
                !extract ilist elements from the order and compare
                ! ... this is super inefficient...
                if (allocated(ilist_sorted)) deallocate(ilist_sorted)
                allocate(ilist_sorted(0))
                do j = 1, size(order)
                    if (any(order(j)==ilist)) then
                        ilist_sorted = [ilist_sorted, order(j)]
                    end if
                end do
                if (all((ilist-ilist_sorted)==0)) then
                    print*, ilist
                    !icorrect = icorrect + 1
                    ! get middle index and add:
                    isum = isum + ilist(size(ilist)/2+1)
                end if

            end if

        else
            ! 47|53
            read(line, '(I2,A1,I2)') i1, c, i2
            if (i1/=0 .and. i2/=0) then  ! do i need this ??
                if (i1==i2) error stop 'why?'
                i1_array = [i1_array, i1]  ! accumulate the dependencies in arrays
                i2_array = [i2_array, i2]
            end if
        end if
    end if

end do

close(iunit)

! --- part 1


write(*,*) '5a:', isum

! --- part 2

! write(*,*) '5b:', isum

call clk%toc('5')

contains

    subroutine save_plot(filename)
        !! save the plot of the dag
        character(len=*),intent(in) :: filename
        character(len=*),parameter :: filetype = 'pdf'  !! filetype for output plot ('pdf', png', etc.)
        call d%save_digraph(filename//'.dot','RL',300)
        call execute_command_line('cat '//filename//'.dot')
        call execute_command_line('dot -T'//filetype//' -o '//&
                                    filename//'.'//filetype//' '//&
                                    filename//'.dot')
    end subroutine save_plot

end program problem_5