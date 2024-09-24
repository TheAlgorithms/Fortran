!> Simpson's Rule Module
!!
!! This module implements Simpson's rule for numerical integration.
!!
!! Simpson's rule approximates the definite integral of a function by
!! dividing the area under the curve into parabolic segments and summing
!! their areas, providing a higher degree of accuracy than the Trapezoidal rule.
!!
!! Note: This implementation is valid for one-dimensional functions
!!
!! Input:
!! - `a`: Lower bound of the integration (real(dp))
!! - `b`: Upper bound of the integration (real(dp))
!! - `n`: Number of panels (integer, must be even)
!! - `func`: The function to integrate (interface)
!!
!! Output:
!! - `integral_result`: Approximate value of the definite integral (real(dp))
!!

module simpson_rule
    implicit none
    integer, parameter :: dp = kind(0.d0)       !! Double precision parameter

contains

    ! Simpson's rule with function passed via interface
    subroutine simpson(integral_result, a, b, n, func)
        implicit none
        integer, intent(in) :: n
        real(dp), intent(in) :: a, b
        real(dp), intent(out) :: integral_result

        real(dp), dimension(:), allocatable :: x, fx
        real(dp) :: h
        integer :: i

        ! Interface for the function
        interface
            real(kind(0.d0)) function func(x)
                real(kind(0.d0)), intent(in) :: x
            end function func
        end interface

        ! Check if n is even
        if (mod(n, 2) /= 0) then
            write (*, *) 'Error: The number of panels (n) must be even.'
            stop
        end if

        ! Step size
        h = (b - a)/(1.0_dp*n)

        ! Allocate arrays
        allocate (x(0:n), fx(0:n))

        ! Create an array of x values, contains the endpoints and the midpoints.
        x = [(a + i*h, i=0, n)]

        ! Apply the function to each x value
        do i = 0, n
            fx(i) = func(x(i))
        end do

        ! Apply Simpson's rule using array slicing
        integral_result = (fx(0) + fx(n) + 4.0_dp*sum(fx(1:n - 1:2)) + 2.0_dp*sum(fx(2:n - 2:2)))*(h/3.0_dp)

        ! Deallocate arrays
        deallocate (x, fx)
    end subroutine simpson

end module simpson_rule
