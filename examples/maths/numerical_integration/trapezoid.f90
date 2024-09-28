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

    real(dp) :: a, b, integral_result
    integer :: n

    ! Set the integration limits and number of panels
    a = -1.0_dp
    b = 1.0_dp
    n = 1000000     !! 1E6 Number of subdivisions

    ! Call the trapezoidal rule with the function passed as an argument
    call trapezoid(integral_result, a, b, n, func)

    write (*, '(A, F12.6)') 'Trapezoidal rule yields: ', integral_result     !! ≈ 0.858195

contains

    function func(x) result(fx)
        implicit none
        real(dp), intent(in) :: x
        real(dp) :: fx

        fx = exp(-x**2)*cos(2.0_dp*x)       !! Example function to integrate
    end function func

end program example_tapezoid
