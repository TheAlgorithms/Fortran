!> Module implementing the Euclidean Algorithm for GCD
!! Reference: https://en.wikipedia.org/wiki/Euclidean_algorithm

module gcd_module
    implicit none
contains

    !! Function to compute the GCD of two integers using the Euclidean algorithm
    function gcd(a, b) result(val)
        integer, value :: a, b
        integer :: t, val

        ! Euclidean algorithm for GCD
        do while (b /= 0)
            t = b
            b = mod(a, b)
            a = t
        end do
        val = a
    end function gcd

end module gcd_module
