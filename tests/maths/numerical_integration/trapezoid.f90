!> Test program for the Trapezoidal Rule module
!!
!!  Created by: Ramy-Badr-Ahmed (https://github.com/Ramy-Badr-Ahmed)
!!  in Pull Request: #32
!!  https://github.com/TheAlgorithms/Fortran/pull/32
!!
!!  Please mention me (@Ramy-Badr-Ahmed) in any issue or pull request
!!  addressing bugs/corrections to this file. Thank you!
!!
!!  This program provides test cases to validate the trapezoidal_rule module against known integral values.


program test_trapezoidal_rule
    use trapezoidal_rule
    implicit none

    real(dp) :: lower_bound, upper_bound, integral_result, expected
    real(dp), parameter :: pi = 4.d0*DATAN(1.d0)  ! Define Pi. Ensures maximum precision available on any architecture

    integer :: panels_number

    ! Test 1: ∫ x^2 dx from 0 to 1 (Exact result = 1/3 ≈ 0.3333)
    lower_bound = 0.0_dp
    upper_bound = 1.0_dp
    panels_number = 1000000
    expected = 1.0_dp / 3.0_dp
    call trapezoid(integral_result, lower_bound, upper_bound, panels_number, f_x_squared)
    call assert_test(integral_result, expected, "Test 1: ∫ x^2 dx from 0 to 1")

    ! Test 2: ∫ x^2 dx from 0 to 2 (Exact result = 8/3 ≈ 2.6667)
    lower_bound = 0.0_dp
    upper_bound = 2.0_dp
    panels_number = 1000000
    expected = 8.0_dp / 3.0_dp
    call trapezoid(integral_result, lower_bound, upper_bound, panels_number, f_x_squared)
    call assert_test(integral_result, expected, "Test 2: ∫ x^2 dx from 0 to 2")

    ! Test 3: ∫ sin(x) dx from 0 to π (Exact result = 2)
    lower_bound = 0.0_dp
    upper_bound = pi
    panels_number = 1000000
    expected = 2.0_dp
    call trapezoid(integral_result, lower_bound, upper_bound, panels_number, sin_function)
    call assert_test(integral_result, expected, "Test 3: ∫ sin(x) dx from 0 to π")

contains

    ! Function for x^2
    real(dp) function f_x_squared(x)
        real(dp), intent(in) :: x
        f_x_squared = x**2
    end function f_x_squared

    ! Function for sin(x)
    real(dp) function sin_function(x)
        real(dp), intent(in) :: x
        sin_function = sin(x)
    end function sin_function

    ! Assertion subroutine
    subroutine assert_test(result, expected, test_name)
        real(dp), intent(in) :: result, expected
        character(len=*), intent(in) :: test_name
        real(dp), parameter :: tol = 1.0e-5_dp

        if (abs(result - expected) < tol) then
            print *, test_name, " PASSED"
        else
            print *, test_name, " FAILED"
            print *, " Expected: ", expected
            print *, " Got:      ", result
            stop 1
        end if
    end subroutine assert_test

end program test_trapezoidal_rule
