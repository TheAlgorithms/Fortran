!> Trapezoidal Rule Module
!!
!! This module implements the Trapezoidal rule for numerical integration.
!!
!! The Trapezoidal rule approximates the definite integral of a function by
!! dividing the area under the curve into trapezoids and summing their areas.
!!
!! Note: This implementation is valid for one-dimensional functions
!!
!! Input:
!! - `a`: Lower bound of the integration (real(dp))
!! - `b`: Upper bound of the integration (real(dp))
!! - `n`: Number of panels (integer)
!! - `func`: The function to integrate (interface)
!!
!! Output:
!! - `integral_result`: Approximate value of the definite integral (real(dp))
!!

module trapezoidal_rule
    implicit none
    integer, parameter :: dp = kind(0.d0)       !! Double precision parameter

contains

    ! Trapezoidal rule with function passed via interface
    subroutine trapezoid(integral_result, a, b, n, func)
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

        ! Step size
        h = (b - a)/real(n, dp)

        ! Allocate arrays
        allocate (x(0:n), fx(0:n))

        ! Create an array of x values
        x = [(a + (real(i, dp))*h, i=0, n)]

        ! Apply the function to each x value
        do i = 0, n
            fx(i) = func(x(i))
        end do

        ! Apply trapezoidal rule using array slicing
        integral_result = ((fx(0) + fx(n))*0.5_dp + sum(fx(1:n)))*h

        ! Deallocate arrays
        deallocate (x, fx)
    end subroutine trapezoid

end module trapezoidal_rule
