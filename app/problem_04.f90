program problem_3

use iso_fortran_env
use aoc_utilities

implicit none

integer(ip):: isum, nrows, ncols, i, j

character(len=1),dimension(:,:),allocatable :: array
character(len=:),allocatable :: s1, s2, s3, s4

call clk%tic()

array = read_file_to_char_array('inputs/day4.txt')
nrows = size(array,1)
ncols = size(array,2)

! --- part 1
isum = 0
do i = 1, nrows
    do j = 1, ncols
        call search(i,j,'XMAS')
    end do
end do
write(*,*) '3a:', isum

! --- part 2
isum = 0
do i = 1, nrows
    do j = 1, ncols
        if (array(i,j)/='A') cycle
        s1 = get_string(i,j,5,2) ! up right
        s2 = get_string(i,j,6,2) ! up left
        s3 = get_string(i,j,7,2) ! down right
        s4 = get_string(i,j,8,2) ! down left
        ! just test all 4 permutations
        if ((s1=='AS' .and. s4=='AM' .and. s2=='AM' .and. s3=='AS') .or. &
            (s1=='AM' .and. s4=='AS' .and. s2=='AS' .and. s3=='AM') .or. &
            (s1=='AM' .and. s4=='AS' .and. s2=='AM' .and. s3=='AS') .or. &
            (s1=='AS' .and. s4=='AM' .and. s2=='AS' .and. s3=='AM') ) then
            isum = isum + 1
        end if
    end do
end do

write(*,*) '3b:', isum

call clk%toc('3')

contains

    subroutine search(i,j,str)
        ! check for str at this grid index [for part 1]
        integer(ip),intent(in) :: i,j
        character(Len=*),intent(in) :: str
        integer :: icase
        if (array(i,j)/=str(1:1)) return
        do icase = 1, 8
            if (get_string(i,j,icase,len(str))==str) then
                isum = isum + 1
            end if
        end do
    end subroutine search

    function get_string(i,j,dir,ilen) result(s)
        !! get the string starting at this grid point
        integer(ip),intent(in) :: i,j !! grid indices (row,col)
        integer,intent(in) :: dir !! direction
        integer,intent(in) :: ilen !! string length
        character(len=:),allocatable :: s ! the returned string

        integer :: k !! loop index
        integer :: m

        m = ilen - 1
        s = ''
        select case (dir)
        case(1) ! up
            if (i>=ilen) then
                do k = 0, m
                    s = s//array(i-k,j)
                end do
            end if
        case(2) ! down
            if (i<=nrows-m) then
                do k = 0, m
                    s = s//array(i+k,j)
                end do
            end if
        case(3) ! right
            if (j<=ncols-m) then
                do k = 0, m
                    s = s//array(i,j+k)
                end do
            end if
        case(4) ! left
            if (j>=ilen) then
                do k = 0, m
                    s = s//array(i,j-k)
                end do
            end if
        case(5) ! up right
            if (i>=ilen .and. j<=ncols-m) then
                do k = 0, m
                    s = s//array(i-k,j+k)
                end do
            end if
        case(6) ! up left
            if (i>=ilen .and. j>=ilen) then
                do k = 0, m
                    s = s//array(i-k,j-k)
                end do
            end if
        case(7) ! down right
            if (i<=nrows-m .and. j<=ncols-m) then
                do k = 0, m
                    s = s//array(i+k,j+k)
                end do
            end if
        case(8) ! down left
            if (i<=nrows-m .and. j>=ilen) then
                do k = 0, m
                    s = s//array(i+k,j-k)
                end do
            end if
        case default
            error stop 'invalid input'
        end select
    end function get_string

end program problem_3