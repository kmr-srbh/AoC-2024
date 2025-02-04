program problem_13

    !! this one requires to be compiled with wp => REAL128
    !!
    !! i.e., fpm run --profile release problem_13 --flag "-DREAL128"

    use aoc_utilities
    ! use iso_fortran_env, only: ep => real128

    implicit none

    integer :: iunit, i, n_lines
    character(len=:),allocatable :: line
    type(string),dimension(:),allocatable :: vals
    real(wp),dimension(2,2) :: a, ainv
    real(wp),dimension(2,1) :: b, x
    logical :: status_ok
    integer(ip) :: icost
    real(wp) :: icost2 ! this one is too big to fit in int64 !

    call clk%tic()

    ! open(newunit=iunit, file='inputs/day13_test.txt', status='OLD')
    open(newunit=iunit, file='inputs/day13.txt', status='OLD')
    n_lines = number_of_lines_in_file(iunit)
    icost = 0
    icost2 = 0
    do i = 1, n_lines, 4

        ! note: no - signs in the data

        !   A        x     =     b
        ! [ax bx] |apress| = |x_prize|
        ! [ay by] |bpress| = |y_prize|

        line = read_line(iunit); vals = split(line(11:),', ')
        a(:,1) = [real(int(vals(1)%str(3:)), wp), real(int(vals(2)%str(3:)), wp)]
        line = read_line(iunit); vals = split(line(11:),', ')
        a(:,2) = [real(int(vals(1)%str(3:)), wp), real(int(vals(2)%str(3:)), wp)]
        line = read_line(iunit); vals = split(line(8:),', ')
        b(:,1) = [real(int(vals(1)%str(3:)), wp), real(int(vals(2)%str(3:)), wp)]
        line = read_line(iunit) ! blank line

        call inverse (a, ainv, status_ok)
        if (status_ok) then
            ! part 1
            x = matmul(ainv, b)
            if (all(x>0.0_wp) .and. all(is_int(x))) icost = icost + 3_ip*x(1,1) + x(2,1)
            ! part 2
            x = matmul(ainv, 10000000000000.0_wp + b)
            if (all(x>0.0_wp) .and. all(is_int(x))) icost2 = icost2 + 3_ip*x(1,1) + x(2,1)
        else
            print*, 'matrix cannot be inverted'
        end if
    end do

    write(*,'(a,i20)')   '13a:', icost
    write(*,'(a,f20.0)') '13b:', icost2

    call clk%toc('13')

    contains

        pure elemental logical function is_int(r) !! true if r is close to an integer
            real(wp),intent(in) :: r
            is_int = abs(r - nint(r, ip)) <= 1.0e-6_wp
        end function is_int

end program problem_13