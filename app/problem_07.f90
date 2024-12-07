program problem_07

    use aoc_utilities

    implicit none

    integer :: iunit, n, i
    character(len=:),allocatable :: line
    integer(ip) :: isum, isum1, isum2, iresult, n_combos, ires, n_spaces, j
    integer(ip),dimension(:),allocatable :: ivals
    type(string),dimension(:),allocatable :: vals, vals2
    integer,dimension(:),allocatable :: ioperators
    logical :: done

    integer,parameter :: PLUS  = 0
    integer,parameter :: TIMES = 1
    integer,parameter :: CAT   = 2

    call clk%tic()

    open(newunit=iunit, file='inputs/day7.txt', status='OLD')
    n = number_of_lines_in_file(iunit)
    isum = 0
    isum1 = 0
    main: do i = 1, n
        ! 161011: 16 10 13
        line = read_line(iunit)
        vals = split(line,': ')
        iresult = vals(1)%to_int_64()  ! the results of the calculation
        vals2 = split(vals(2), ' ')
        ivals = vals2%to_int_64()      ! array of values to do the calculation
        n_spaces = size(ivals)-1
        n_combos = 2**(n_spaces) ! number of operator combinations
        if (allocated(ioperators)) deallocate(ioperators)
        allocate(ioperators(n_spaces)) !; ioperators = PLUS

        ! part 1 [this is pretty fast]
        do j = 1, n_combos
            call permute(j)  ! permute for next one
            ires = evaluate(iresult)
            if (ires == iresult) then
                ! this one works
                isum1 = isum1 + iresult  ! only the part 1 sum
                isum = isum + iresult
                cycle main ! don't have to try the part 2 method
            end if
        end do

        ! if it didn't work, then try part 2 (with ||)
        ! [this is slow.. it could be sped up by recusiving doing the evaluate]
        done = .false.
        call generate (1)

    end do main

    write(*,*) '7a:', isum1
    write(*,*) '7b:', isum

    call clk%toc('7')

    contains

        subroutine permute(icase)
            !! part 1 permute method
            integer(ip),intent(in) :: icase ! generate this permutation of the operators

            !note: there's some way to do this with btest ???
            !      this way is using strings and it probably wildly inefficient

            ! this won't work for part 2 since there are now 3 operators ...

            integer :: k, kk
            character(64) :: gchar

            ! convert to a binary string 000, 001, 010, etc...
            write(gchar,'(B64.64)') icase-1
            kk = 1
            do k=64,1,-1
                if (gchar(k:k)=='1') then
                    ioperators(kk)=TIMES
                else
                    ioperators(kk)=PLUS
                end if
                kk = kk+1
                if (kk>n_spaces) exit
            end do
        end subroutine permute

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

        recursive subroutine generate (i)
            !! recursive combo generation & evaluate for part 2
            !! see https://github.com/jacobwilliams/polyroots-fortran/blob/master/test/polyroots_test_10.f90
            integer, intent(in) :: i
            integer :: ix
            integer,dimension(*),parameter :: icoeffs = [PLUS,TIMES,CAT] !! set of operators

            if (done) return ! global var
            if (i > n_spaces) then
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
                    call generate(i+1)
                    if (done) return
                end do
            end if
        end subroutine generate

    end program problem_07


