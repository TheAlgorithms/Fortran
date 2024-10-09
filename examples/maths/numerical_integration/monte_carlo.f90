!> Example Program for Monte Carlo Integration
!!
!!  Created by: Ramy-Badr-Ahmed (https://github.com/Ramy-Badr-Ahmed)
!!  in Pull Request: #25
!!  https://github.com/TheAlgorithms/Fortran/pull/25
!!
!!  Please mention me (@Ramy-Badr-Ahmed) in any issue or pull request
!!  addressing bugs/corrections to this file. Thank you!
!!
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

    real(dp) :: lower_bound, upper_bound, integral_result, error_estimate
    integer :: random_samples_number

    ! Set the integration limits and number of random samples
    lower_bound = -1.0_dp
    upper_bound = 1.0_dp
    random_samples_number = 1000000     !! 1E6 Number of random samples

    ! Call Monte Carlo integration with the function passed as an argument
    call monte_carlo(integral_result, error_estimate, lower_bound, upper_bound, random_samples_number, function)

    write (*, '(A, F12.6, A, F12.6)') "Monte Carlo result: ", integral_result, " +- ", error_estimate     !! ≈ 0.858421

contains

    function function(x) result(fx)
        implicit none
        real(dp), intent(in) :: x
        real(dp) :: fx

        fx = exp(-x**2)*cos(2.0_dp*x)       !! Example function to integrate
    end function function

end program example_monte_carlo
