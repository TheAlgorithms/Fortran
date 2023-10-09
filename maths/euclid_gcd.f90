!> GCD of 2 numbers using Euclid algorithm
!! Reference: https://en.wikipedia.org/wiki/Euclidean_algorithm

program euclid_gcd
    use, intrinsic :: iso_fortran_env, only : input_unit, error_unit
    implicit none
    integer :: a, b, val, istat
    character(len=1024) :: msg

    print *, "Enter the two numbers (+ve integers): "
    ! The following model of reading input is not necessary.
    ! ----------
    read(unit=input_unit, fmt=*, iostat=istat, iomsg=msg) a, b
    if (istat /= 0) then
        write(error_unit, fmt='(2A)') 'error: ', trim(msg)
        stop 1
    end if
    ! ----------
    ! Above section can be replaced with following if error handling is not required.
    ! read *, a, b
    val = gcd(a, b)
    print *, 'The greatest common divisor is: ', val

    contains
    function gcd(a,b) result(val)
        integer, value :: a, b
        integer :: t, val
        do while (b /= 0)
            t = b
            b = mod(a, b)
            a = t
        end do
        val = a
    end function gcd
end program euclid_gcd