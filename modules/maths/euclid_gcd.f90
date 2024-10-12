!> Module implementing the Euclidean Algorithm for GCD
!!
!!  Modified by: Ramy-Badr-Ahmed (https://github.com/Ramy-Badr-Ahmed)
!!  in Pull Request: #31
!!  https://github.com/TheAlgorithms/Fortran/pull/31
!!
!!  Please mention me (@Ramy-Badr-Ahmed) in any issue or pull request
!!  addressing bugs/corrections to this file. Thank you!
!!
!! Reference: https://en.wikipedia.org/wiki/Euclidean_algorithm

module gcd_module
    implicit none
contains

    !! Function to compute the GCD of two integers using the Euclidean algorithm
    function gcd(a, b) result(val)
        integer, value :: a, b
        integer :: t, val

        ! Ensure the GCD is non-negative
        a = abs(a)
        b = abs(b)

        ! Euclidean algorithm for GCD
        do while (b /= 0)
            t = b
            b = mod(a, b)
            a = t
        end do
        val = a
    end function gcd

end module gcd_module
