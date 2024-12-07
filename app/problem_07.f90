program problem_07

    use aoc_utilities

    implicit none

    integer :: iunit, n, i
    integer(ip) :: isum1, isum2, iresult, ires
    integer(ip),dimension(:),allocatable :: ivals
    integer,dimension(:),allocatable :: ioperators
    logical :: done

    integer,parameter :: PLUS  = 0 !! +
    integer,parameter :: TIMES = 1 !! *
    integer,parameter :: CAT   = 2 !! ||

    call clk%tic()

    open(newunit=iunit, file='inputs/day7.txt', status='OLD')
    n = number_of_lines_in_file(iunit)
    isum1 = 0; isum2 = 0
    main: do i = 1, n
        ! read the file line by line
        !line = read_line(iunit)
        call parse_line(read_line(iunit), iresult, ivals, ioperators)

        ! part 1
        done = .false.
        call run(1, [PLUS,TIMES], isum1)

        ! part 2
        ! only retest the ones that didn't pass part 1
        ! [this is slow... it could be sped up by recusiving doing the evaluate]
        if (.not. done) call run(1, [PLUS,TIMES,CAT], isum2)

    end do main
    close(iunit)

    write(*,*) '7a:', isum1
    write(*,*) '7b:', isum1 + isum2

    call clk%toc('7')

    contains

        subroutine parse_line(line, iresult, ivals, ioperators)
            !! parse the line
            character(len=*),intent(in) :: line
            integer(ip),intent(out) :: iresult
            integer(ip),dimension(:),allocatable,intent(out) :: ivals
            integer,dimension(:),allocatable,intent(out) :: ioperators !! will just allocate this array
            type(string),dimension(:),allocatable :: vals, vals2
            integer(ip) :: n_spaces, n_combos
            ! 161011: 16 10 13
            vals = split(line,': ')
            iresult = vals(1)%to_int_64()  ! the results of the calculation
            vals2 = split(vals(2), ' ')
            ivals = vals2%to_int_64()      ! array of values to do the calculation
            n_spaces = size(ivals)-1
            n_combos = 2**(n_spaces) ! number of operator combinations
            allocate(ioperators(n_spaces)) !; ioperators = PLUS
        end subroutine parse_line

        function evaluate(igoal) result(isum)
            ! evaluate, given the values and operators
            integer(ip),intent(in) :: igoal !! the solution we are looking for
            integer(ip) :: isum
            integer(ip) :: i
            type(string) :: s1, s2
            isum = ivals(1)
            do i = 2, size(ivals)
                select case (ioperators(i-1))
                case(PLUS);  isum = isum + ivals(i)
                case(TIMES); isum = isum * ivals(i)
                case(CAT)
                    s1 = int_to_string(isum)
                    s2 = int_to_string(ivals(i))
                    s1%str = s1%str // s2%str
                    isum = s1%to_int_64()
                case default; error stop 'invalid operator'
                end select
                if (isum > igoal) exit ! we don't have to continue.
                    ! since there is no - operator, the sum can only get larger
            end do
        end function evaluate

        recursive subroutine run (i,icoeffs,isum)
            !! recursive combo generation & evaluate for part 2
            !! see https://github.com/jacobwilliams/polyroots-fortran/blob/master/test/polyroots_test_10.f90
            integer, intent(in) :: i
            integer,dimension(:),intent(in) :: icoeffs !! set of operators
            integer(ip),intent(inout) :: isum
            integer :: ix !! counter

            if (done) return ! global var
            if (i > size(ioperators)) then
                ! do the evaluation here.... but.... really we need to also recursively
                ! do the computation..... so we aren't starting over for each one... or maybe cache something ?

                ires = evaluate(iresult)

                if (ires == iresult) then
                    ! this one works
                    isum = isum + iresult
                    done = .true.
                    return  ! don't need to try any more (some can have more than one option)
                end if
            else
                do ix = 1,size(icoeffs)
                    ioperators(i) = icoeffs(ix)
                    call run(i+1,icoeffs,isum)
                    if (done) return
                end do
            end if
        end subroutine run

    end program problem_07


