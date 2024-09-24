!> Example Program for Simpson's Rule
!! This program demonstrates the use of Simpson's rule for numerical integration.
!!
!! It sets the integration limits and number of panels, and calls the
!! simpson subroutine to compute the approximate value of the definite integral
!! of the specified function.
!!
!! Example function: f(x) = exp(-x^2) * cos(2.0_dp * x)

program example_simpson
    use simpson_rule
    implicit none

    real(dp) :: a, b, integral_result
    integer :: n

    ! Set the integration limits and number of panels
    a = -1.0_dp
    b = 1.0_dp
    n = 100     !! Number of subdivisions (must be even)

    ! Call Simpson's rule with the function passed as an argument
    call simpson(integral_result, a, b, n, func)

    write(*, '(A, F12.8)') "Simpson's rule yields: ", integral_result       !! ≈ 0.85819555

contains

    function func(x) result(fx)
        implicit none
        real(dp), intent(in) :: x
        real(dp) :: fx

        fx = exp(-x**2) * cos(2.0_dp * x)       !! Example function to integrate
    end function func

end program example_simpson