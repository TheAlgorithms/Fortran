!> Example Program for Gaussian-Legendre Quadrature Module
!!
!!  Created by: Ramy-Badr-Ahmed (https://github.com/Ramy-Badr-Ahmed)
!!  in Pull Request: #25
!!  https://github.com/TheAlgorithms/Fortran/pull/25
!!
!!  Please mention me (@Ramy-Badr-Ahmed) in any issue or pull request
!!  addressing bugs/corrections to this file. Thank you!
!!
!! This program demonstrates the use of Gaussian-Legendre Quadrature Module for numerical integration.
!!
!! It sets the integration limits and the number of quadrature points (n), and calls the
!! gauss_legendre_quadrature subroutine to compute the approximate value of the definite integral
!! of the specified function.
!!
!! Example function: f(x) = exp(-x^2) * cos(2.0_dp * x)

program example_gaussian_quadrature
    use gaussian_legendre_quadrature
    implicit none

    real(dp) :: lower_bound, upper_bound, integral_result
    integer :: quadrature_points_number

    ! Set the integration limits and number of quadrature points
    lower_bound = -1.0_dp
    upper_bound = 1.0_dp
    quadrature_points_number = 5       !! Number of quadrature points (order of accuracy) up to 5

    ! Call Gaussian quadrature to compute the integral with the function passed as an argument
    call gauss_legendre_quadrature(integral_result, lower_bound, upper_bound, quadrature_points_number, function)

    write (*, '(A, F12.6)') "Gaussian Quadrature result: ", integral_result          !! ≈ 0.858574

contains

    function function(x) result(fx)
        implicit none
        real(dp), intent(in) :: x
        real(dp) :: fx

        fx = exp(-x**2)*cos(2.0_dp*x)       !! Example function to integrate
    end function function

end program example_gaussian_quadrature
