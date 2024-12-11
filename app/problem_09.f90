program problem_09

    use aoc_utilities

    implicit none

    integer,dimension(:),allocatable :: iline, imap
    integer(ip) :: i, j, n_elements, istart, iend, isum

    call clk%tic()

    ! iline = read_file_to_int_vec('inputs/day9_test.txt')
    iline = read_file_to_int_vec('inputs/day9.txt')
    n_elements = sum(iline)
    allocate(imap(n_elements))

    ! create the map array:
    imap = -1
    j = 1
    do i = 1, size(iline)
        if (mod(i,2)/=0) imap(j:j+iline(i)-1) = (i-1)/2  ! files
        j = j + iline(i)
    end do
    ! in the imap array, a -1 indicates an empty slot

    ! defrag:
    istart = -1; iend = size(imap)+1  ! pointers for forward and backward counters
    do
        do  ! first next empty slot
            istart = istart + 1
            if (imap(istart)==-1) exit
        end do
        do  ! next slot to move from the end
            iend = iend - 1
            if (imap(iend)/=-1) exit
        end do
        if (istart>=iend) exit
        imap(istart) = imap(iend)
        imap(iend)   = -1
    end do
    isum = sum( [(i, i = 0,n_elements-1)]*imap, mask=imap/=-1)

    write(*,*) '9a:', isum
    ! write(*,*) '9b:', isum2

    call clk%toc('9')

end program problem_09
