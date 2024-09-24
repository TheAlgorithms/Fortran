!> Gaussian-Legendre Quadrature Module
!!
!! This module provides the implementation of Gaussian-Legendre Quadrature.
!!
!! The method approximates the definite integral of a function over a specified interval [a, b].
!!
!! The quadrature method works by transforming nodes and weights from the reference interval [-1, 1] to the
!! interval [a, b] and then evaluating the function at these nodes. The integral is then approximated by summing
!! the product of function values and corresponding weights.
!!
!! Contents:
!! - `gauss_legendre_quadrature`: A subroutine to perform Gaussian-Legendre quadrature using provided nodes and weights.
!! - `gauss_legendre_weights`: A helper subroutine to initialize the quadrature nodes and weights for different orders (n).
!!
!! Input:
!! - `a`: Lower bound of integration (real(dp))
!! - `b`: Upper bound of integration (real(dp))
!! - `n`: Number of quadrature points (integer)
!! - `func`: The function to integrate (interface)
!!
!! Output:
!! - `integral_result`: Approximate value of the integral (real(dp))

module gaussian_legendre_quadrature
    implicit none
    integer, parameter :: dp = kind(1.0d0)      !! Double precision parameter

contains

    ! General Gaussian Quadrature for definite integral
    subroutine gauss_legendre_quadrature(integral_result, a, b, n, func)
        implicit none
        real(dp), intent(out) :: integral_result
        real(dp), intent(in) :: a, b
        integer, intent(in) :: n         !! Number of quadrature points (order of accuracy)

        real(dp), dimension(n) :: t, w, x
        real(dp), dimension(:), allocatable :: fx
        integer :: i

        ! Interface for the function
        interface
            real(kind(0.d0)) function func(x) result(fx)
                real(kind(0.d0)), intent(in) :: x
            end function func
        end interface

        ! Initialize nodes and weights for Gauss-Legendre quadrature based on n
        call gauss_legendre_weights(t, w, n)

        ! Allocate the function value array
        allocate (fx(n))

        ! Transform the nodes from the reference interval [-1, 1] to [a, b]
        x = (b + a)/2.0_dp + (b - a)*t/2.0_dp

        ! Compute function values at the transformed points
        do i = 1, n
            fx(i) = func(x(i))
        end do

        ! Apply the Gaussian-Legendre quadrature formula
        integral_result = sum(w*fx)*(b - a)/2.0_dp

        ! Deallocate fx array
        deallocate (fx)

    end subroutine gauss_legendre_quadrature

    ! Subroutine to initialize Gauss-Legendre nodes and weights
    subroutine gauss_legendre_weights(t, w, n)
        implicit none
        integer, intent(in) :: n
        real(dp), intent(out), dimension(n) :: t, w  !! Nodes (t) and weights (w)

        ! Predefined nodes and weights for different values of n
        select case (n)
        case (1)
            t = [0.0_dp]        !! Single node at the center for n = 1
            w = [2.0_dp]        !! Weight of 2 for the single point
        case (2)
            t = [-0.5773502692_dp, 0.5773502692_dp]     !! Symmetric nodes for n = 2
            w = [1.0_dp, 1.0_dp]                        !! Equal weights for n = 2
        case (3)
            t = [-0.7745966692_dp, 0.0_dp, 0.7745966692_dp]             !! Symmetric nodes for n = 3
            w = [0.5555555556_dp, 0.8888888889_dp, 0.5555555556_dp]     !! Weights for n = 3
        case (4)
            t = [-0.8611363116_dp, -0.3399810436_dp, 0.3399810436_dp, 0.8611363116_dp]      !! Nodes for n = 4
            w = [0.3478548451_dp, 0.6521451549_dp, 0.6521451549_dp, 0.3478548451_dp]        !! Weights for n = 4
        case (5)
            t = [-0.9061798459_dp, -0.5384693101_dp, 0.0_dp, 0.5384693101_dp, 0.9061798459_dp]          !! Nodes for n = 5
            w = [0.2369268851_dp, 0.4786286705_dp, 0.5688888889_dp, 0.4786286705_dp, 0.2369268851_dp]   !! Weights for n = 5
            ! You can add more cases to support higher values of n.
        case default
            print *, 'Gauss-Legendre quadrature for n > 5 is not implemented.'
        end select

    end subroutine gauss_legendre_weights

end module gaussian_legendre_quadrature
