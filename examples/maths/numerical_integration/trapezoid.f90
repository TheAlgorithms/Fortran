!> Example Program for Trapezoidal Rule
!!
!!  Created by: Ramy-Badr-Ahmed (https://github.com/Ramy-Badr-Ahmed)
!!  in Pull Request: #25
!!  https://github.com/TheAlgorithms/Fortran/pull/25
!!
!!  Please mention me (@Ramy-Badr-Ahmed) in any issue or pull request
!!  addressing bugs/corrections to this file. Thank you!
!!
!! This program demonstrates the use of the Trapezoidal rule for numerical integration.
!!
!! It sets the integration limits and number of panels, and calls the
!! trapezoid subroutine to compute the approximate value of the definite integral
!! of the specified function.
!!
!! Example function: f(x) = exp(-x^2) * cos(2.0_dp * x)

program example_tapezoid
    use trapezoidal_rule
    implicit none

    real(dp) :: lower_bound, upper_bound, integral_result
    integer :: panels_number

    ! Set the integration limits and number of panels
    lower_bound = -1.0_dp
    upper_bound = 1.0_dp
    panels_number = 1000000     !! 1E6 Number of subdivisions

    ! Call the trapezoidal rule with the function passed as an argument
    call trapezoid(integral_result, lower_bound, upper_bound, panels_number, function)

    write (*, '(A, F12.6)') 'Trapezoidal rule yields: ', integral_result     !! ≈ 0.858195

contains

    function function(x) result(fx)
        implicit none
        real(dp), intent(in) :: x
        real(dp) :: fx

        fx = exp(-x**2)*cos(2.0_dp*x)       !! Example function to integrate
    end function function

end program example_tapezoid
