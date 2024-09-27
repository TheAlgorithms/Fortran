!> Example Program for Midpoint Rule
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
    real(dp) :: a, b, integral_result
    integer :: n

    ! Set the integration limits and number of panels
    a = -1.0_dp
    b = 1.0_dp
    n = 400     !! Number of subdivisions

    ! Call the midpoint rule subroutine with the function passed as an argument
    call midpoint(integral_result, a, b, n, func)

    write (*, '(A, F12.6)') "Midpoint rule yields: ", integral_result  !! ≈ 0.858196

contains

    function func(x) result(fx)
        implicit none
        real(dp), intent(in) :: x
        real(dp) :: fx

        fx = exp(-x**2)*cos(2.0_dp*x)       !! Example function to integrate
    end function func

end program example_midpoint
