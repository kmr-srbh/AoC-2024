program problem_11

    use aoc_utilities
    !use iso_fortran_env, only: wp => real64

    implicit none

    integer(ip),dimension(:),allocatable :: array
    integer :: iunit
    integer(ip) :: isum, isum2, i

    call clk%tic()

    ! open(newunit=iunit, file='inputs/day11_test2.txt', status='OLD')
    open(newunit=iunit, file='inputs/day11.txt', status='OLD')
    array = int(split(trim(adjustl(read_line(iunit))), ' '))
    close(iunit)

    !print ('(A,*(I5))'), 'array = ', array
    do i = 1, 25
        array = blink(array)
        !print ('(A,*(I7))'), 'array = ', array
    end do

    write(*,*) '11a:', size(array)

    !write(*,*) '11b:', isum2

    call clk%toc('11')

    contains
        pure function blink(iin) result(iout)
        integer(ip),dimension(:),intent(in) :: iin
        integer(ip),dimension(:),allocatable :: iout

        integer(ip) :: n, itop, ibottom
        integer(ip) :: i

        allocate(iout(0))
        do i = 1_ip, size(iin)
            if (iin(i)==0) then
                iout = [iout, 1_ip]
            else
                n = num_digits(iin(i))
                if (mod(n,2_ip)==0) then
                    n = n/2_ip
                    ! split number  aaabbb -> aaa bbb
                    itop    = floor(real(iin(i), wp)/10_ip**n,ip)
                    ibottom = iin(i) - itop*10_ip**n
                    iout = [iout, itop, ibottom]
                else
                    iout = [iout, iin(i)*2024_ip]
                end if
            end if
        end do

        end function blink

        pure integer function num_digits(i)
            !! return the number of digits in the integer
            integer(ip),intent(in) :: i
            num_digits = 1_ip+int(log10(float(i)), ip)
        end function num_digits

end program problem_11

