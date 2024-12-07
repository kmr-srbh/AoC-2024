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

        pure function atomic_evaluate(iop, ival1, ival2) result(isum)
            ! evaluate, given the values and operators
            integer,intent(in)     :: iop   !! the operator to apply
            integer(ip),intent(in) :: ival1 !! the first value
            integer(ip),intent(in) :: ival2 !! the second value
            integer(ip) :: isum
            select case (iop)
            case(PLUS);  isum = ival1 + ival2
            case(TIMES); isum = ival1 * ival2
            case(CAT);   isum = ival2 + ival1*10**num_digits(ival2) ! cat the two numbers [ 11 || 2 -> 112 ]
            case default; error stop 'invalid operator'
            end select
        end function atomic_evaluate

        pure integer function num_digits(i)
            !! return the number of digits in the integer
            integer(ip),intent(in) :: i
            num_digits = 1+int(log10(float(i)))
        end function num_digits

        function evaluate(igoal) result(isum)
            ! evaluate, given the values and operators
            integer(ip),intent(in) :: igoal !! the solution we are looking for
            integer(ip) :: isum
            integer(ip) :: i
            isum = ivals(1)
            do i = 2, size(ivals)
                isum = atomic_evaluate(ioperators(i-1), isum, ivals(i))
                if (isum > igoal) exit ! we don't have to continue.
                                       ! since there is no - operator,
                                       ! the sum can only get larger
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
                ! we could speed this up by doing the evaluation recursively also
                ! [now, we are starting over each time once the combo is generated,
                !  so there are a lot of duplicated calculations]
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


