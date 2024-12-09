program problem_08

    use aoc_utilities

    implicit none

    integer :: i,j,nrows,ncols,k,n,idel,jdel
    character(len=1),dimension(:,:),allocatable :: array
    logical,dimension(:,:),allocatable :: antinodes, antinodes2
    character(len=1),dimension(:),allocatable :: unique_antennas
    integer,dimension(:),allocatable :: iant, jant
    logical :: found1, found2

    call clk%tic()

    ! array = read_file_to_char_array('inputs/day8_test.txt')
    array = read_file_to_char_array('inputs/day8.txt')
    nrows = size(array,1)
    ncols = size(array,2)
    allocate(antinodes(nrows, ncols)); antinodes = .false.
    antinodes2 = antinodes ! for part 2

    ! identify all the unique antennas in the array:
    unique_antennas = achar(unique(ichar(pack(array, mask=array/='.'))))
    ! process each antenna one at a time:
    do i = 1, size(unique_antennas)
        ! get all the indices of this character (this type of antenna):
        ! do this by creating index matrices, and using mask to get the ones we want
        iant = pack(spread([(k, k = 1, size(array,1))], dim=2, ncopies=size(array,2)), mask=array==unique_antennas(i))
        jant = pack(spread([(k, k = 1, size(array,2))], dim=1, ncopies=size(array,1)), mask=array==unique_antennas(i))
        ! for all permutations of any pair
        ! check for antinodes and accumulate them
        do j = 1, size(iant)
            do k = 1, size(jant)
                if (j>=k) cycle ! only need to do each combo once
                ! offset from one to the other:
                idel = iant(j) - iant(k)
                jdel = jant(j) - jant(k)
                n = 0  ! counter for part 2, use a loop (part 1 is just the n=1 case)
                found1 = .false.; found2 = .false.
                do
                    ! does this pair have antinodes within the array bounds?
                    found1 = (in_bounds(iant(j)+n*idel, jant(j)+n*jdel))
                    if (found1) then
                        if (n==1) antinodes( iant(j)+n*idel, jant(j)+n*jdel) = .true.
                                  antinodes2(iant(j)+n*idel, jant(j)+n*jdel) = .true.
                    end if
                    found2 = (in_bounds(iant(k)-n*idel, jant(k)-n*jdel))
                    if (found2) then
                        if (n==1) antinodes( iant(k)-n*idel, jant(k)-n*jdel) = .true.
                                  antinodes2(iant(k)-n*idel, jant(k)-n*jdel) = .true.
                    end if
                    if (.not. found1 .and. .not. found2) exit ! all have been found
                    n = n + 1
                end do
            end do
        end do

    end do

    write(*,*) '8a:', count(antinodes)
    write(*,*) '8b:', count(antinodes2)

    call clk%toc('8')

contains

    pure logical function in_bounds(i,j) !! is this point in the array bounds?
        integer,intent(in) :: i,j
        in_bounds = (i>=1 .and. i<=nrows .and. j>=1 .and. j<=ncols)
    end function in_bounds

end program problem_08