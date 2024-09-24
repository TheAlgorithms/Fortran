!> Midpoint rule Module
!!
!! This module implements Midpoint rule for numerical integration.
!!
!! The midpoint rule approximates the integral by calculating the function
!! value at the midpoint of each subinterval and summing these values, multiplied
!! by the width of the subintervals.
!!
!! Note: This implementation is valid for one-dimensional functions
!!
!! Input:
!! - `a`: Lower bound of integration (real(dp))
!! - `b`: Upper bound of integration (real(dp))
!! - `n`: Number of panels (integer)
!! - `func`: The function to integrate (interface)
!!
!! Output:
!! - `integral_result`: Approximate value of the integral (real(dp))

module midpoint_rule
    implicit none
    integer, parameter :: dp = kind(0.d0)       !! Double precision parameter

contains

    subroutine midpoint(integral_result, a, b, n, func)
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
        h = (b - a)/(1.0_dp*n)

        ! Allocate array for midpoints
        allocate (x(1:n), fx(1:n))

        ! Calculate midpoints
        x = [(a + (i - 0.5_dp)*h, i=1, n)]

        ! Apply function to each midpoint
        do i = 1, n
            fx(i) = func(x(i))
        end do

        ! Final integral value
        integral_result = h*sum(fx)

        ! Deallocate arrays
        deallocate (x, fx)

    end subroutine midpoint

end module midpoint_rule
