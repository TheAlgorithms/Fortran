!> Monte Carlo Integration Module
!!
!! This module estimates the integral of a function over a specified range
!! using the Monte Carlo method (with OpenMP parallelization) and provides an error estimate.
!!
!! The method works by randomly sampling points within the integration range [a, b]
!! and evaluating the function at those points to estimate the integral.
!!
!! Note: This implementation is valid for one-dimensional functions
!!
!! Input:
!! - `a`: Lower bound of integration (real(dp))
!! - `b`: Upper bound of integration (real(dp))
!! - `n`: Number of random samples (integer)
!! - `func`: The function to integrate (interface)
!!
!! Output:
!! - `integral_result`: Approximate value of the integral (real(dp))
!! - `error_estimate`: Estimated error of the integral approximation (real(dp))

module monte_carlo_integration
    use omp_lib                 !! OpenMP library for parallelization
    implicit none
    integer, parameter :: dp = kind(0.d0)       !! Double precision parameter

contains

    subroutine monte_carlo(integral_result, error_estimate, a, b, n, func)
        implicit none
        integer, intent(in) :: n
        real(dp), intent(in) :: a, b
        real(dp), intent(out) :: integral_result, error_estimate

        real(dp), dimension(:), allocatable :: uniform_sample, fx
        real(dp) :: sum_fx, sum_fx_squared
        integer :: i

        ! Interface for the function
        interface
            real(kind(0.d0)) function func(x)
                real(kind(0.d0)), intent(in) :: x
            end function func
        end interface

        ! Allocate arrays for random samples and function values
        allocate(uniform_sample(1:n), fx(1:n))

        ! Generate uniform random points in [a, b]
        call random_number(uniform_sample)
        uniform_sample = a + (b - a) * uniform_sample  !! Scale to the interval [a, b]

        ! Evaluate the function at all random points in parallel
        !$omp parallel do           !! OpenMP parallelization to distribute the loop across multiple threads
        do i = 1, n
            fx(i) = func(uniform_sample(i))
        end do
        !$omp end parallel do

        ! Sum of function values and sum of function values squared (for error estimation)
        sum_fx = sum(fx)
        sum_fx_squared = sum(fx**2)

        ! Compute the Monte Carlo estimate of the integral
        integral_result = (b - a) * (sum_fx / real(n, dp))

        ! Estimate the error using the variance of the function values
        error_estimate = sqrt((sum_fx_squared/n - (sum_fx/n)**2) / (n-1)) * (b-a)

        ! Deallocate arrays
        deallocate(uniform_sample, fx)

    end subroutine monte_carlo

end module monte_carlo_integration
