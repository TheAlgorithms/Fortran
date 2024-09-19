!> Program to compute the GCD of two numbers using the gcd_module

program euclid_gcd_program
    use gcd_module
    implicit none
    integer :: a, b, val

    a = 56
    b = 98

    val = gcd(a, b)
    print *, 'The greatest common divisor of ', a, ' and ', b, ' is: ', val

end program euclid_gcd_program
