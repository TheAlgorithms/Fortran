!> Test program for the Gaussian Legendre Quadrature module
!!
!!  Created by: Your Name (https://github.com/YourGitHub)
!!  in Pull Request: #32
!!  https://github.com/TheAlgorithms/Fortran/pull/32
!!
!!  This program provides test cases to validate the gaussian_legendre_quadrature module against known integral values.

program test_gaussian_legendre_quadrature
    use gaussian_legendre_quadrature
    implicit none

    ! Run test cases
    call test_integral_x_squared_0_to_1()
    call test_integral_e_x_0_to_1()
    call test_integral_sin_0_to_pi()
    call test_integral_cos_0_to_pi_over_2()
    call test_integral_1_over_x_1_to_e()
    call test_integral_x_cubed_0_to_1()
    call test_integral_sin_squared_0_to_pi()
    call test_integral_1_over_x_1_to_e()

    print *, "All tests completed."

contains

    ! Test case 1: ∫ x^2 dx from 0 to 1 (Exact result = 1/3 ≈ 0.3333)
    subroutine test_integral_x_squared_0_to_1()
        real(dp) :: lower_bound, upper_bound, integral_result, expected
        integer :: panels_number
        lower_bound = 0.0_dp
        upper_bound = 1.0_dp
        panels_number = 5           ! Adjust the number of quadrature points as needed from 1 to 5
        expected = 1.0_dp/3.0_dp
        call gauss_legendre_quadrature(integral_result, lower_bound, upper_bound, panels_number, f_x_squared)
        call assert_test(integral_result, expected, "Test 1: ∫ x^2 dx from 0 to 1")
    end subroutine test_integral_x_squared_0_to_1

    ! Test case 2: ∫ e^x dx from 0 to 1 (Exact result = e - 1 ≈ 1.7183)
    subroutine test_integral_e_x_0_to_1()
        real(dp) :: lower_bound, upper_bound, integral_result, expected
        integer :: panels_number
        lower_bound = 0.0_dp
        upper_bound = 1.0_dp
        panels_number = 3
        expected = exp(1.0_dp) - 1.0_dp
        call gauss_legendre_quadrature(integral_result, lower_bound, upper_bound, panels_number, exp_function)
        call assert_test(integral_result, expected, "Test 2: ∫ e^x dx from 0 to 1")
    end subroutine test_integral_e_x_0_to_1

    ! Test case 3: ∫ sin(x) dx from 0 to π (Exact result = 2)
    subroutine test_integral_sin_0_to_pi()
        real(dp) :: lower_bound, upper_bound, integral_result, expected
        integer :: panels_number
        real(dp), parameter :: pi = 4.D0*DATAN(1.D0)  ! Define Pi. Ensure maximum precision available on any architecture.
        lower_bound = 0.0_dp
        upper_bound = pi
        panels_number = 5
        expected = 2.0_dp
        call gauss_legendre_quadrature(integral_result, lower_bound, upper_bound, panels_number, sin_function)
        call assert_test(integral_result, expected, "Test 3: ∫ sin(x) dx from 0 to π")
    end subroutine test_integral_sin_0_to_pi

    ! Test case 4: ∫ cos(x) dx from 0 to π/2 (Exact result = 1)
    subroutine test_integral_cos_0_to_pi_over_2()
        real(dp) :: lower_bound, upper_bound, integral_result, expected
        real(dp), parameter :: pi = 4.D0*DATAN(1.D0)  ! Define Pi. Ensure maximum precision available on any architecture.
        integer :: panels_number
        lower_bound = 0.0_dp
        upper_bound = pi/2.0_dp
        panels_number = 5
        expected = 1.0_dp
        call gauss_legendre_quadrature(integral_result, lower_bound, upper_bound, panels_number, cos_function)
        call assert_test(integral_result, expected, "Test 4: ∫ cos(x) dx from 0 to π/2")
    end subroutine test_integral_cos_0_to_pi_over_2

    ! Test case 5: ∫ (1/x) dx from 1 to e (Exact result = 1)
    subroutine test_integral_1_over_x_1_to_e()
        real(dp) :: lower_bound, upper_bound, integral_result, expected
        integer :: panels_number
        lower_bound = 1.0_dp
        upper_bound = exp(1.0_dp)
        panels_number = 5
        expected = 1.0_dp
        call gauss_legendre_quadrature(integral_result, lower_bound, upper_bound, panels_number, log_function)
        call assert_test(integral_result, expected, "Test 5: ∫ (1/x) dx from 1 to e")
    end subroutine test_integral_1_over_x_1_to_e

    ! Test case 6: ∫ x^3 dx from 0 to 1 (Exact result = 1/4 = 0.25)
    subroutine test_integral_x_cubed_0_to_1()
        real(dp) :: lower_bound, upper_bound, integral_result, expected
        integer :: panels_number
        lower_bound = 0.0_dp
        upper_bound = 1.0_dp
        panels_number = 4
        expected = 0.25_dp
        call gauss_legendre_quadrature(integral_result, lower_bound, upper_bound, panels_number, f_x_cubed)
        call assert_test(integral_result, expected, "Test 6: ∫ x^3 dx from 0 to 1")
    end subroutine test_integral_x_cubed_0_to_1

    ! Test case 7: ∫ sin^2(x) dx from 0 to π (Exact result = π/2 ≈ 1.5708)
    subroutine test_integral_sin_squared_0_to_pi()
        real(dp) :: lower_bound, upper_bound, integral_result, expected
        real(dp), parameter :: pi = 4.D0*DATAN(1.D0)  ! Define Pi. Ensure maximum precision available on any architecture.
        integer :: panels_number
        lower_bound = 0.0_dp
        upper_bound = pi
        panels_number = 5
        expected = 1.57084_dp    ! Approximate value, adjust tolerance as needed
        call gauss_legendre_quadrature(integral_result, lower_bound, upper_bound, panels_number, sin_squared_function)
        call assert_test(integral_result, expected, "Test 7: ∫ sin^2(x) dx from 0 to π")
    end subroutine test_integral_sin_squared_0_to_pi

    ! Function for x^2
    real(dp) function f_x_squared(x)
        real(dp), intent(in) :: x
        f_x_squared = x**2
    end function f_x_squared

    ! Function for e^x
    real(dp) function exp_function(x)
        real(dp), intent(in) :: x
        exp_function = exp(x)
    end function exp_function

    ! Function for 1/x
    real(dp) function log_function(x)
        real(dp), intent(in) :: x
        log_function = 1.0_dp/x
    end function log_function

    ! Function for cos(x)
    real(dp) function cos_function(x)
        real(dp), intent(in) :: x
        cos_function = cos(x)
    end function cos_function

    ! Function for x^3
    real(dp) function f_x_cubed(x)
        real(dp), intent(in) :: x
        f_x_cubed = x**3
    end function f_x_cubed

    ! Function for sin(x)
    real(dp) function sin_function(x)
        real(dp), intent(in) :: x
        sin_function = sin(x)
    end function sin_function

    ! Function for sin^2(x)
    real(dp) function sin_squared_function(x)
        real(dp), intent(in) :: x
        sin_squared_function = sin(x)**2
    end function sin_squared_function

    !> Subroutine to assert the test results
    subroutine assert_test(actual, expected, test_name)
        real(dp), intent(in) :: actual, expected
        character(len=*), intent(in) :: test_name
        real(dp), parameter :: tol = 1.0e-5_dp

        if (abs(actual - expected) < tol) then
            print *, test_name, " PASSED"
        else
            print *, test_name, " FAILED"
            print *, " Expected: ", expected
            print *, " Got:      ", actual
            stop 1
        end if
    end subroutine assert_test

end program test_gaussian_legendre_quadrature
