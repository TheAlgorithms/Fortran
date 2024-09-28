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

    real(dp) :: a, b, integral_result
    integer :: n

    ! Set the integration limits and number of quadrature points
    a = -1.0_dp
    b = 1.0_dp
    n = 5       !! Number of quadrature points

    ! Call Gaussian quadrature to compute the integral
    call gauss_legendre_quadrature(integral_result, a, b, n, func)

    write (*, '(A, F12.6)') "Gaussian Quadrature result: ", integral_result          !! ≈ 0.858574

contains

    function func(x) result(fx)
        implicit none
        real(dp), intent(in) :: x
        real(dp) :: fx

        fx = exp(-x**2)*cos(2.0_dp*x)       !! Example function to integrate
    end function func

end program example_gaussian_quadrature
