!> Program to compute the GCD of two numbers using the gcd_module

program euclid_gcd_program
    use gcd_module
    implicit none
    integer :: a, b, val
    character(len=1024) :: msg
    integer :: istat

    print *, "Enter the two numbers (+ve integers): "
    read(*, *, iostat=istat, iomsg=msg) a, b
    if (istat /= 0) then
        write(*, fmt='(2A)') 'error: ', trim(msg)
        stop 1
    end if

    val = gcd(a, b)
    print *, 'The greatest common divisor is: ', val

end program euclid_gcd_program
