program problem_09

    use aoc_utilities

    implicit none

    integer,dimension(:),allocatable :: iline, imap
    integer(ip) :: i, j

    call clk%tic()

    ! iline = read_file_to_int_vec('inputs/day9_test.txt')
    iline = read_file_to_int_vec('inputs/day9.txt')
    allocate(imap(sum(iline)))  ! sum all the ints to get number of elements

    ! create the map array:
    imap = -1  ! in the imap array, a -1 indicates an empty slot
    j = 1
    do i = 1, size(iline)
        if (mod(i,2)/=0) imap(j:j+iline(i)-1) = (i-1)/2  ! files
        j = j + iline(i)
    end do

    write(*,*) '9a:', defrag(imap)
    write(*,*) '9b:', defrag2(imap)

    call clk%toc('9')

    contains

    function defrag(im) result(isum)
        integer,dimension(:),intent(in) :: im
        integer(ip) :: isum
        integer,dimension(:),allocatable :: imap
        integer(ip) :: istart, iend, i

        imap = im ! make a copy
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
        isum = checksum(imap)

    end function defrag

    function defrag2(im) result(isum) ! part 2
        integer,dimension(:),intent(in) :: im
        integer(ip) :: isum

        integer,dimension(:),allocatable :: iindex, idx !! indices of ival in imap
        integer :: i, ival, istart
        integer,dimension(:),allocatable :: imap

        imap = im ! make a copy
        iindex = [(i, i = 1, size(imap))]  ! index array
        allocate(idx(1)); idx=size(imap)

        main: do ival = maxval(imap), 1, -1

            ! note: this is slow because we search the array everytime
            ! a loop would probably be faster here, starting from the end
            ! speed it up a little bit by skipping the part we already searched
            idx = pack( iindex(1:idx(1)), mask=imap==ival)  ! indices of this file

            ! look for a contiguous slot to put it
            istart = -1
            do  ! first next empty slot
                istart = istart + 1
                if (imap(istart)==-1) then
                    if (all(imap(istart:istart+size(idx)-1)==-1)) exit ! big enough slot to hold it
                end if
                if (istart+size(idx)>=idx(1)) cycle main ! didn't find one
            end do

            ! found a slot:
            imap(istart:istart+size(idx)-1) = ival
            imap(idx) = -1

        end do main
        isum = checksum(imap)

    end function defrag2

    pure function checksum(imap) result(isum)
        integer,dimension(:),intent(in) :: imap
        integer(ip) :: isum, i
        isum = sum([(i, i = 0,size(imap)-1)]*imap, mask=imap/=-1)
    end function checksum

end program problem_09
