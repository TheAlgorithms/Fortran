!> Example Program for Midpoint Rule
!!
!!  Created by: Ramy-Badr-Ahmed (https://github.com/Ramy-Badr-Ahmed)
!!  in Pull Request: #25
!!  https://github.com/TheAlgorithms/Fortran/pull/25
!!
!!  Please mention me (@Ramy-Badr-Ahmed) in any issue or pull request
!!  addressing bugs/corrections to this file. Thank you!
!!
!! This program demonstrates the use of Midpoint Rule for numerical integration.
!!
!! It sets the integration limits and number of subintervals (panels), and calls the
!! midpoint subroutine to compute the approximate value of the definite integral
!! of the specified function.
!!
!! Example function: f(x) = exp(-x^2) * cos(2.0_dp * x)

program example_midpoint
    use midpoint_rule
    implicit none

    real(dp) :: lower_bound, upper_bound, integral_result
    integer :: panels_number

    ! Set the integration limits and number of panels
    lower_bound = -1.0_dp
    upper_bound = 1.0_dp
    panels_number = 400     !! Number of subdivisions

    ! Call the midpoint rule subroutine with the function passed as an argument
    call midpoint(integral_result, lower_bound, upper_bound, panels_number, function)

    write (*, '(A, F12.6)') "Midpoint rule yields: ", integral_result  !! ≈ 0.858196

contains

    function function(x) result(fx)
        implicit none
        real(dp), intent(in) :: x
        real(dp) :: fx

        fx = exp(-x**2)*cos(2.0_dp*x)       !! Example function to integrate
    end function function

end program example_midpoint
