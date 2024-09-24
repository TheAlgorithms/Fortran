!> Example Program for Monte Carlo Integration
!! This program demonstrates the use of Monte Carlo module for numerical integration.
!!
!! It sets the integration limits and number of random samples, and calls the
!! monte_carlo subroutine to compute the approximate value of the definite integral
!! of the specified function.
!!
!! Example function: f(x) = exp(-x^2) * cos(2.0_dp * x)

program example_monte_carlo
    use monte_carlo_integration
    implicit none

    real(dp) :: a, b, integral_result, error_estimate
    integer :: n

    ! Set the integration limits and number of random samples
    a = -1.0_dp
    b = 1.0_dp
    n = 1E6     !! Number of random samples

    ! Call Monte Carlo integration
    call monte_carlo(integral_result, error_estimate, a, b, n, func)

    write (*, '(A, F12.6, A, F12.6)') "Monte Carlo result: ", integral_result, " +- ", error_estimate     !! ≈ 0.858421

contains

    function func(x) result(fx)
        implicit none
        real(dp), intent(in) :: x
        real(dp) :: fx

        fx = exp(-x**2) * cos(2.0_dp * x)       !! Example function to integrate
    end function func

end program example_monte_carlo
